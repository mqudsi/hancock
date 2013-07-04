#include <assert.h>

#include "hl.hh"

/* map to stream functions */


#ifndef __MAPSTREAM_
#define __MAPSTREAM_

typedef struct HRSnextEntry_s *HRSnextEntry_t;

typedef struct {
  HRSmap_t it_map;
  void *low;
  void *high;
} HRSiterate_s;

extern HRSnextEntry_t HRSnextEntryInit(HRSiterate_s mapDescr);
extern void *HRSgetNextExistingEntry(HRSnextEntry_t handle);

#endif

/* tms = true map stream */
static int HLgetMapNext(char set, void *v, long long *val)
{
  long long *rv;
  HRSnextEntry_t tms = (HRSnextEntry_t) v;

  assert(set);
  assert(tms != NULL);

  if ((rv = HRSgetNextExistingEntry(tms)) == NULL)
    return HRS_STREAM_STOP;
  else {
    *val = *rv;
    return HRS_STREAM_KEEP_REC;
  }
}

static HLmapStream_s mkMS(HRSnextEntry_t tms)
{
  HLmapStream_s ms(:HLgetMapNext, tms:) = NULL;

  return ms;
}

HLmapStream_s HLcreateMapStream(HRSmap_t m, char all,
				long long low, long long high)
{
  HRSiterate_s md;
  HRSnextEntry_t tsm;

  if (all)
    HRSmapRanges(m, &low, &high);

  md.it_map = m;
  md.low = &low;
  md.high = &high;

  tsm = HRSnextEntryInit(md);

  return mkMS(tsm);

}

/* stream of keys functions */

int HLgetValidKey(char set, long long low, long long high,
		  Sfio_t *input, long long *pn)
{
  int found = 0;
  char c;

  found = sfscanf(input, "%lld\n", pn);
  if (found == EOF) return HRS_STREAM_DROP_REC;
  if (found == 0 ) {
    sfscanf(input, "%c", &c); /* read one byte to advance stream */
    return HRS_STREAM_DROP_REC;
  };
  if (set && (*pn < low)) return HRS_STREAM_DROP_REC;
  if (set && (*pn > high)) return HRS_STREAM_DROP_REC;
  return 1;
}

/* extra stream functions */

void HLinitStream(HRSstream_t s, void *tf)
{ 
  HRSstreamStart(s, tf, 0, NULL);
}

int HLadvanceStream(HRSstream_t s, void *data) 
{
  return HRSstreamNext(s, (char *)data);
}


struct mm_s {
  HLmapStream_s ms1;
  HLmapStream_s ms2;
  HLmapStream_s ms3;
  long long k1;
  long long k2;
  long long k3;
  char done1;
  char done2;
  char done3;
  char operation;   /* OR_MERGE or AND_MERGE */
};

int HLgetNextmm2(char set, void *v, long long *val);

static HLmapStream_s mkMM2(struct mm_s *mm2)
{
  HLmapStream_s s(:HLgetNextmm2, mm2:) = NULL;

  return s;
}

HLmapStream_s HLcreateMapMerge2(HRSmap_t m1, HRSmap_t m2, char all,
				long long low, long long high, 
			        char operation)
{ 
  struct mm_s *mm2 = malloc(sizeof(struct mm_s));

  mm2->ms1 = HLcreateMapStream(m1, all, low, high);
  HRSstreamStart(mm2->ms1, NULL, 0, NULL);

  mm2->ms2 = HLcreateMapStream(m2, all, low, high);
  HRSstreamStart(mm2->ms2, NULL, 0, NULL);
  
  if (HLadvanceStream(mm2->ms1, &mm2->k1) == 0)
    mm2->done1 = 1;
  else
    mm2->done1 = 0;

  if (HLadvanceStream(mm2->ms2, &mm2->k2) == 0)
    mm2->done2 = 1;
  else
    mm2->done2 = 0;

  mm2->operation = operation;
  return mkMM2(mm2);
}


int HLgetNextmm2(char set, void *v, long long *val)
{
  struct mm_s *msd = (struct mm_s *) v;
  long long t = LONGLONG_MAX;
  char op = msd->operation;

  assert(set);

  if (msd->done1 && msd->done2)
    return HRS_STREAM_STOP;

  if (op == OR_MERGE) {
    if ((msd->done1 == 0) && (msd->k1 < t))
      t = msd->k1;

    if ((msd->done2 == 0) && (msd->k2 < t))
      t = msd->k2;

    if ((msd->done1 == 0) && (msd->k1 == t)) {
      if (HLadvanceStream(msd->ms1, &msd->k1) == 0)
      msd->done1 = 1;
    }

    if ((msd->done2 == 0) && (msd->k2 == t)) {
      if (HLadvanceStream(msd->ms2, &msd->k2) == 0)
        msd->done2 = 1;
    }

    *val = t;
    return HRS_STREAM_KEEP_REC;
  } else { /* AND_MERGE */ 
    if ((msd->done1 == 1) || (msd->done2 == 1))
      return HRS_STREAM_STOP;

    if (msd->k1 == msd->k2) {
      *val = msd->k1;

      if (HLadvanceStream(msd->ms1, &msd->k1) == 0)
	msd->done1 = 1;

      if (HLadvanceStream(msd->ms2, &msd->k2) == 0)
        msd->done2 = 1;

      return HRS_STREAM_KEEP_REC;  

    } else if (msd->k1 < msd->k2) {
      if (HLadvanceStream(msd->ms1, &msd->k1) == 0)
	msd->done1 = 1;
      return HLgetNextmm2(set, v, val);

    } else {  /* msd->k2 < msd->k1 */
      if (HLadvanceStream(msd->ms2, &msd->k2) == 0)
	msd->done2 = 1;
      return HLgetNextmm2(set, v, val);
    }
  }
}

int HLgetNextmm3(char set, void *v, long long *val);
static HLmapStream_s mkMM3(struct mm_s *mm)
{
  HLmapStream_s s(:HLgetNextmm3, mm:) = NULL;

  return s;
}


HLmapStream_s HLcreateMapMerge3(HRSmap_t m1, HRSmap_t m2, HRSmap_t m3,
				char all, long long low, long long high,
				char operation)
{ 
  struct mm_s *mm = malloc(sizeof(struct mm_s));

  mm->ms1 = HLcreateMapStream(m1, all, low, high);
  HRSstreamStart(mm->ms1, NULL, 0, NULL);
  
  mm->ms2 = HLcreateMapStream(m2, all, low, high);
  HRSstreamStart(mm->ms2, NULL, 0, NULL);
  
  mm->ms3 = HLcreateMapStream(m3, all, low, high);
  HRSstreamStart(mm->ms3, NULL, 0, NULL);

  if (HLadvanceStream(mm->ms1, &mm->k1) == 0)
    mm->done1 = 1;
  else
    mm->done1 = 0;

  if (HLadvanceStream(mm->ms2, &mm->k2) == 0)
    mm->done2 = 1;
  else
    mm->done2 = 0;

  if (HLadvanceStream(mm->ms3, &mm->k3) == 0)
    mm->done3 = 1;
  else
    mm->done3 = 0;

  mm->operation = operation;
  return mkMM3(mm);  
}


int HLgetNextmm3(char set, void *v, long long *val)
{
  struct mm_s *msd = (struct mm_s *) v;
  long long t = LONGLONG_MAX;

  assert(set);

  if (msd->done1 && msd->done2 && msd->done3) 
    return HRS_STREAM_STOP;

  if (msd->operation == OR_MERGE) {
    if (msd->done1 == 0) 
      t = msd->k1;

    if ((msd->done2 == 0) && (msd->k2 < t))
      t = msd->k2;

    if ((msd->done3 == 0) && (msd->k3 < t))
      t = msd->k3;

    if ((msd->done1 == 0) && (msd->k1 == t)) {
      if (HLadvanceStream(msd->ms1, &msd->k1) == 0)
	msd->done1 = 1;
    }

    if ((msd->done2 == 0) && (msd->k2 == t)) {
      if (HLadvanceStream(msd->ms2, &msd->k2) == 0)
	msd->done2 = 1;
    }

    if ((msd->done3 == 0) && (msd->k3 == t)) {
      if (HLadvanceStream(msd->ms3, &msd->k3) == 0)
	msd->done3 = 1;
    }

    *val = t;
    return HRS_STREAM_KEEP_REC;  
  } else {

    if ((msd->done1 == 1) ||
	(msd->done2 == 1) ||
	(msd->done3 == 1))
      return HRS_STREAM_STOP;

    t = msd->k1;

    if (msd->k2 < t)
      t = msd->k2;

    if (msd->k3 < t)
      t = msd->k3;

    if (msd->k1 <= t) {
      if (HLadvanceStream(msd->ms1, &msd->k1) == 0)
	msd->done1 = 1;
    }

    if (msd->k2 <= t) {
      if (HLadvanceStream(msd->ms2, &msd->k2) == 0)
	msd->done2 = 1;
    }

    if (msd->k3 <= t) {
      if (HLadvanceStream(msd->ms3, &msd->k3) == 0)
	msd->done3 = 1;
    }

    if ((msd->k1 == t) &&
	(msd->k2 == t) &&
	(msd->k3 == t)) {
      *val = t;
      return HRS_STREAM_KEEP_REC;
    } else {
      return HLgetNextmm3(set, v, val);
    }
  }
}



