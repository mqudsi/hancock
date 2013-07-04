/**
  This file implements Hancock's map data structure.

  File organization:  
    Auxiliary functions for
      Low-level I/O including HRSreadBytes, HRSwriteBytes
      Portable I/O format 
      Managing key descriptions
      Managing keys

    Functions for handling the in-memory and on-disk
    representation of maps.  In particular, for
      Handling self-identification information
      Managing indices
      Managing bitvectors (used in blocks)
      Managing stripes (used in blocks)
      Managing blocks
      Managing stripe_cache. (purely an in-memory structure)
      Managing free-lists/file space.
  
    Auxiliary routines used to by HRSmapOpen.
      
    Runtime system interface functions:
       HRSmapOpen
       HRSmapDone
       HRSmapCopy (with cvtSMtoCSIDBV)
       HRSmapGet
       HRSmapPut
       HRSmapBlockExists
       HRSmapQuery
       HRSmapRemove
       HRScvtOldMaptoNew 
        (with cvtOSMBlocktoSM and cvtCSIDMBtoSM)
       HRSnextEntryInit
       HRSgetNextExistsingEntry
       HRSmapMakeSpace (with a bunch of auxiliary routines)
**/

#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <assert.h>
#include <sfio.h>
#include "util.h"
#include <stdlib.h>
#include <math.h>
#include <sys/resource.h>
#include <HRS.h>
#include "runtime.h"
#include "mapport.h"
#include "mapstream.h"
#include <vcodex.h>
#include "HRS-internal.h"

// defined in util.c
extern char isP2(int32 v, int32 *sb);

/* functions local to this file */
static void setMemoryUsageLimit();
static void setAccessOrder(HRSmap_t m);
static void setFileBufferSize(HRSmap_t m, int buffered);

static void flushStripe(HRSmap_t m);

int enoughSpace(HRSmap_t m, loc_t oldLoc, stripeSize_t oldSize,  
		stripeSize_t newSize);
static void freeFileSpace(HRSmap_t m, loc_t loc, stripeSize_t size);
static loc_t allocFileSpace(HRSmap_t m, int32 size);

static stripeCache_t * createStripeCache(HRSmap_t m, int32 bn);
static stripeSize_t flushNfreeStripeCache(HRSmap_t m,int32 bn);
static stripeSize_t freeStripeCache(HRSmap_t m,int32 bn);
static void readFreeList(HRSmap_t m);

HRSmap_t headMapList = NULL;
int HRSmapFlushEager = 0;
int HRSmapFullBitOK = 0;

int32 NUM_ENTRIES_IN_STRIPE_DEFAULT=75;


/*******************************************************
**                                                    **
** Functions translating between portable             **
** and non-portable formats                           **
**                                                    **
*******************************************************/

static int32 write_loc_tNS(fileInfo_s fi, loc_t value, loc_t loc) {
  assert(sizeof(loc_t)==sizeof(int64));
  value=hton_int64(value);
  HRSwriteBytes(fi, (uint8 *) &value, loc, sizeof(loc_t), NOSYNC);
  return sizeof(loc_t);
}

static int32 write_intNS(fileInfo_s fi, int32 value, loc_t loc) {
  value=hton_int32(value);
  HRSwriteBytes(fi, (uint8 *) &value, loc, sizeof(int32), NOSYNC);
  return sizeof(int32);
}

static int32 write_llNS(fileInfo_s fi, int64 value, loc_t loc) {
  value=hton_int64(value);
  HRSwriteBytes(fi, (uint8 *) &value, loc, sizeof(int64), NOSYNC);
  return sizeof(int64);
}

static int32 read_loc_t(fileInfo_s fi, loc_t *value, loc_t loc) {
  assert(sizeof(loc_t)==sizeof(int64));

  HRSreadBytes(fi, (uint8 *) value, loc, sizeof(loc_t));
  *value=ntoh_int64(*value);
  return sizeof(loc_t);
}

static int32 read_int(fileInfo_s fi, int32 *value, loc_t loc) {
  HRSreadBytes(fi, (uint8 *) value, loc, sizeof(int32));
  *value=ntoh_int32(*value);
  return sizeof(int32);
}

static int32 read_ll(fileInfo_s fi, int64 *value, loc_t loc) {
  HRSreadBytes(fi, (uint8 *) value, loc, sizeof(int64));
  *value=ntoh_int64(*value);
  return sizeof(int64);
}



static int32 readArrayOfInts(fileInfo_s fi, uint8 *a, loc_t loc, int32 numBytes) {
  int32 numInts=numBytes/sizeof(int32);
  int32 i;

  assert((numBytes%sizeof(int32))==0);

  HRSreadBytes(fi, a, loc, numBytes); 
  for (i=0;i<numInts;i++)
    ((int32*)a)[i]=ntoh_int32(((int32 *)a)[i]);

  return numInts*sizeof(int32);
}

/* SL and pSL may point to the same space */
static int32 portSL(HRSmap_t m, stripeSize_t *SL, stripeSize_t *pSL) {
  int32 slLen;
  int32 i;

  assert(sizeof(int32) == sizeof(stripeSize_t));
  assert((m->block.SLSize%sizeof(stripeSize_t)) == 0);

  /* len in INTS not bytes */
  slLen = m->block.SLSize/sizeof(stripeSize_t);
  
  for (i=0; i< slLen; i++)
    pSL[i]=hton_int32(SL[i]);

  return slLen*sizeof(stripeSize_t);
}


static int32 checkInt32(fileInfo_s fi, loc_t loc, int val, char *errMsg)
{
  int32 temp;

  read_int(fi, &temp, loc);

  if (temp != val)
    HRSerrorExit1(errMsg, fi.name);

  return sizeof(int32);
}


static int32 checkInt64(fileInfo_s fi, loc_t loc, int64 val, char *errMsg)
{
  int64 temp;

  read_ll(fi, &temp, loc);

  if (temp != val)
    HRSerrorExit1(errMsg, fi.name);

  return sizeof(int64);
}

/*******************************************************
**                                                    **
** Functions for managing key descriptions            **
**                                                    **
*******************************************************/

/* DO NOT change these values */
#define BLOCK 0
#define STRIPE 1
#define ENTRY 2
#define LINE 1   /* used for LINE slot for 2-level keys */
#define FULLKEY 0  /* used for long long keys */

#define LLKey(m) ((m)->origKey.levels == 1)
#define L2Key(m) ((m)->origKey.levels == 2)
#define L3Key(m) ((m)->origKey.levels == 3)

/* read key description from a file, return number of bytes read */
int32 readKDfromFile(fileInfo_s fi, HRSkey_desc_s *kd, loc_t loc)
{ int l;
  loc_t nloc = loc;

  nloc += read_int(fi, &kd->levels, nloc);
  assert(kd->levels <= MAXLEVELS);

  for (l=0; l < kd->levels; l++) {
    int low, high;
    nloc += read_int(fi, &low, nloc);
    kd->ranges[l].low = low;
    nloc += read_int(fi, &high, nloc);
    kd->ranges[l].high = high;
  }
  return nloc - loc;
}


/* write key description to a file, return number of bytes written */
int32 writeKDtoFile(fileInfo_s fi, HRSkey_desc_s kd, loc_t loc)
{ int l;
  loc_t nloc = loc;

  write_intNS(fi, kd.levels, nloc);
  nloc += sizeof(int32);

  for (l=0; l < kd.levels; l++) {
    int low, high;
    low = (int) kd.ranges[l].low;    /* ranges are now kept as long long's */
    write_intNS(fi, low, nloc);
    nloc += sizeof(int32);
    high = (int) kd.ranges[l].high;
    write_intNS(fi, high, nloc);
    nloc += sizeof(int32);
  }
  return nloc - loc;
}

/* check that two key descriptions match */
void matchKeyDesc(HRSkey_desc_s k1, HRSkey_desc_s k2)
{ int i;

  if (k1.levels != k2.levels)
    HRSerrorExit("matchKeys: Key types do not match\n");

  for (i=0; i < k2.levels; i++) {
    if ((k1.ranges[i].low != k2.ranges[i].low) ||
	(k1.ranges[i].high != k2.ranges[i].high))
      HRSerrorExit("matchKeys: Key types do not match\n");
  }
}

/* Adjust the key description to be a zero-based, three level key */
/* assume the map type has been set                               */
static HRSkey_desc_s adjKeyDesc(HRSmap_t m, HRSkey_desc_s orig)
{
  HRSkey_desc_s kd;
  /*  char *fname = "adjKeyDesc"; */

  assert((orig.levels >= 1) || (orig.levels <= 3));

  kd.levels = 3;
  kd.ranges[BLOCK].low = 0;
  kd.ranges[STRIPE].low = 0;
  kd.ranges[ENTRY].low = 0;

  if (L3Key(m)) {
    /* OLD_STRIPED MAPS and COMPRESSED_SELF_ID_BV maps could only
       have 2-level keys */
    assert(m->type == STRIPED_MAP);
    /* new style key -- adjust */
    kd.ranges[BLOCK].high = orig.ranges[BLOCK].high - orig.ranges[BLOCK].low;
    kd.ranges[STRIPE].high = 
      orig.ranges[STRIPE].high - orig.ranges[STRIPE].low;
    kd.ranges[ENTRY].high = 
      orig.ranges[ENTRY].high - orig.ranges[ENTRY].low;
  } else if (LLKey(m)) {
    HRSLLKey_t adjHigh = 
      orig.ranges[FULLKEY].high - orig.ranges[FULLKEY].low;

    assert(m->type == LL_SPLIT_MAP);

    kd.ranges[BLOCK].high = 
      ((int) ceil((double) adjHigh/(double) m->blockSplit)) - 1;
    kd.ranges[STRIPE].high = 
      ((int) ceil((double) m->blockSplit/(double) m->stripeSplit)) - 1;
    kd.ranges[ENTRY].high = m->stripeSplit-1;
  } else if (m->type == STRIPED_MAP) {
    kd.ranges[BLOCK].high = orig.ranges[BLOCK].high - orig.ranges[BLOCK].low;
    /* stripe size MUST be the default, if it is not specified. */
    kd.ranges[STRIPE].high = 
      (orig.ranges[LINE].high - orig.ranges[LINE].low)/NUM_ENTRIES_IN_STRIPE_DEFAULT;
    kd.ranges[ENTRY].high = NUM_ENTRIES_IN_STRIPE_DEFAULT-1;
  } else {
    assert(m->type == COMPRESSED_SELF_ID_BV);
    /* only one stripe in old style maps */
    kd.ranges[STRIPE].high = 0;
    kd.ranges[ENTRY].high = orig.ranges[LINE].high - orig.ranges[LINE].low;
  }

  return kd;

}

/*******************************************************
**                                                    **
** Functions for managing keys                        **
**                                                    **
*******************************************************/

void keyError(HRSmap_t m, void *k) {
  char *fname = "keyError";
  int32 errLen = strlen(m->fi.name) + 50 + 1;
  char *errStr = (char *) HRSmalloc(errLen, fname);
  
  if (LLKey(m)) {
    HRSLLKey_t llk = *((HRSLLKey_t *) k);
    sfsprintf(errStr, errLen, "%s: Key %lld out of range\n",
	      m->fi.name, llk);
  }
  else if (L2Key(m)) {
    HRSkey_s k2 = *((HRSkey_s *) k);
    sfsprintf(errStr, errLen, "%s: Key %d %d out of range\n",
	      m->fi.name, k2.block, k2.line);
  } else {
    HRS3Key_s k3 = *((HRS3Key_s *) k);
    sfsprintf(errStr, errLen, "%s: Key %d %d %d out of range\n",
	      m->fi.name, k3.block, k3.stripe, k3.entry);
  }

  HRSerrorExit(errStr);
}


/* adjust only the block part of the key */
int32 adjBlock(HRSmap_t m, int32 bn) {

  if ((bn < m->origKey.ranges[BLOCK].low) || 
      (bn > m->origKey.ranges[BLOCK].high))
    HRSerrorExit1("%s: block out of range\n", m->fi.name);

  return (bn - m->origKey.ranges[BLOCK].low);
}


static void adjKey(HRSmap_t m, void *k, HRS3Key_s *adjK3)
{
  range_desc_s *oKR = m->origKey.ranges;
  assert(m != NULL);

  /* check key and adjust as necessary */
  if (LLKey(m)) {
    /* LL keys */
    HRSLLKey_t llk = (*(HRSLLKey_t *)k);
    HRSLLKey_t adjKey;
    HRSLLKey_t x;
    int y;

    if ((oKR[FULLKEY].low > llk) ||
        (llk > oKR[FULLKEY].high)) 
      keyError(m, k);

    if (llk == m->lastSeen) {
      *adjK3 = m->lastSeen3;
      return;
    }

    adjKey = llk - oKR[FULLKEY].low;
    if (m->blockP2) {
      adjK3->block = adjKey >> m->blockShift;
      x = adjKey - ((long long) adjK3->block << m->blockShift);
    }
    else {
      adjK3->block = adjKey/m->blockSplit;
      x = adjKey - ((long long) adjK3->block* (long long)m->blockSplit);
    }

    if (m->stripeP2) {
      adjK3->stripe = x >> m->stripeShift;
      y = x - (adjK3->stripe << m->stripeShift);
    } else {
      adjK3->stripe = x/m->stripeSplit;
      y = x - (adjK3->stripe*m->stripeSplit);
    }
    adjK3->entry = y;

    assert((adjK3->block >= 0) && (adjK3->block <= m->adjKey.ranges[BLOCK].high));
    assert((adjK3->stripe >= 0) && (adjK3->stripe <= m->adjKey.ranges[STRIPE].high));
    assert((adjK3->entry >= 0) && (adjK3->entry <= m->adjKey.ranges[ENTRY].high));


    m->lastSeen = llk;
    m->lastSeen3 = *adjK3;

  } else if (L2Key(m)) {
    HRSkey_s *k2 = (HRSkey_s *)k;
    int32 adjLine;

    /* two-level key.  check and adjust */
    if ((k2->block < oKR[BLOCK].low) ||
	(k2->block > oKR[BLOCK].high))
      keyError(m, k);
    adjK3->block = k2->block - oKR[BLOCK].low;

    if ((k2->line < oKR[LINE].low) ||
	(k2->line > oKR[LINE].high))
      keyError(m, k);
    adjLine = k2->line - oKR[LINE].low;
    adjK3->stripe = adjLine/m->block.numStripeEntries;
    adjK3->entry = adjLine%m->block.numStripeEntries;

  } else {
    HRS3Key_s *k3 = (HRS3Key_s *)k;

    if ((k3->block < oKR[BLOCK].low) ||
	(k3->block > oKR[BLOCK].high))
      keyError(m, k);
    adjK3->block = k3->block - oKR[BLOCK].low;

    if ((k3->stripe < oKR[STRIPE].low) ||
	(k3->stripe > oKR[STRIPE].high))
      keyError(m, k);
    adjK3->stripe = k3->stripe - oKR[STRIPE].low;

    if ((k3->entry < oKR[ENTRY].low) ||
	(k3->entry > oKR[ENTRY].high))
      keyError(m, k);
    adjK3->entry = k3->entry - oKR[ENTRY].low;
  }
}


int32 compressSingle(HRSmapCompress_s ci,
		   char set,  
		   void *pt, 
		   uint8 *from, 
		   uint8 *to, 
		   int32 to_size)
{
  if (ci.type == HRS_USER_STRIPE_COMPRESS) {
    HRSstripeCompressFn_t f = (HRSstripeCompressFn_t) ci.cf;

    return f(set, pt, from, 1, to, to_size);
  } else {
    HRSentryCompressFn_t f = (HRSentryCompressFn_t) ci.cf;
    return f(set, pt, from, to, to_size);
  }
}

int32 decompressSingle(HRSmapCompress_s ci,
		       char set, 
		       void * pt, 
		       uint8 *from, 
		       int32 from_size,
		       uint8 *to)
{
  if (ci.type == HRS_USER_STRIPE_COMPRESS) {
    HRSstripeDecompressFn_t f = (HRSstripeDecompressFn_t) ci.df;
    return f(set, pt, from, from_size, to, 1);

  } else {
    HRSentryDecompressFn_t f = (HRSentryDecompressFn_t) ci.df;
    return f(set, pt, from, from_size, to);

  }
}






/*******************************************************
**                                                    **
** Functions for reading/writing self-identification  **
** information in the file header.                    **
**                                                    **
** LL_SPLIT_MAP header:                               **
**       type (int32)                                 **
**       entry size (int32)                           **
**       compression factor (int32)                   ** 
**       size of compressed const default             **
**          (might be HRS_REMOVEENTRY)                **
**       compressed const default                     **
**          (entrySize * compression factor bytes)    **
**       size of compressed other                     **
**          (might be HRS_REMOVEENTRY, but only       **
**           if compressed other is just the          **
**           initial compressed default value)        **
**       compressed other                             **
**          (entrySize * compression factor bytes)    **
**       original Key desciption                      **
**          (HRSkey_desc_s)                           **
**       blockSplit                                   **
**       entrySplit                                   *8
**       adjusted Key description                     **
**          (HRSkey_desc_s)                           **
**                                                    **
** STRIPED_MAP header:                                **
**       type (int32)                                 **
**       entry size (int32)                           **
**       compression factor (int32)                   ** 
**       size of compressed const default             **
**          (might be HRS_REMOVEENTRY)                **
**       compressed const default                     **
**          (entrySize * compression factor bytes)    **
**       size of compressed other                     **
**          (might be HRS_REMOVEENTRY, but only       **
**           if compressed other is just the          **
**           initial compressed default value)        **
**       compressed other                             **
**          (entrySize * compression factor bytes)    **
**       original Key desciption                      **
**          (HRSkey_desc_s)                           **
**       adjusted Key description                     **
**          (HRSkey_desc_s)                           **
**                                                    **
** OLD_STRIPED_MAP header:                            **
**       type (int32)                                 **
**       entry size (int32)                           **
**       compression factor (int32)                   ** 
**       compressed const default                     **
**          (entrySize * compression factor bytes)    **
**       compressed other                             **
**          (entrySize * compression factor bytes)    **
**       adjusted Key ranges                          **
**          (int32 * 6)                               **
**                                                    **
**                                                    **
** COMPRESSED_SELF_ID_BV header:                      **
**       type (int32)                                 **
**       entry size (int32)                           **
**       const default                                **
**          (entrySize bytes)                         **
*******************************************************/

/* used to find the location of the second compressed
   value in the self id information for striped maps */
#define val2Loc(m) (3*sizeof(int32)+sizeof(int32)+(m)->cf*(m)->block.entrySize)


static int32 readEntryFromSelfID(HRSmap_t m, loc_t loc)
{
  int32 uc;         /* return code from ucr (number of bytes consumed) */
  int32 c;          /* return code from cr (number of bytes generated) */
  int32 rv;
  int32 ruc;
  uint8 *cval;
  uint8 *rcval;     /* recompressed value */
  uint8 *val;       /* decompressed value */
  uint8 *rval;       /* decompressed value */
  int32 eSize = m->block.entrySize;
  char *fname = "readEntryFromSelfID";
  int maxCVSize = m->cf*eSize;
  int32 bytesRead;

  assert(m->def.type == CONSTANTDEFAULT);

  cval = (uint8 *) HRSmalloc(maxCVSize, fname);
  rcval = (uint8 *) HRSmalloc(maxCVSize, fname);

  val = (uint8 *) HRSmalloc(eSize, fname);
  bzero(val, eSize);
  rval = (uint8 *) HRSmalloc(eSize, fname);
  bzero(rval, eSize);

  bytesRead = read_int(m->fi, &rv, loc);
  loc += bytesRead;

  if (rv == HRS_REMOVEENTRY) {
    /* compressed value was the map default */
    memcpy(val, m->def.constant, eSize);
    c = compressSingle(m->compressInfo, 
		       m->set, m->pt, val, rcval, maxCVSize);    
    if (c != HRS_REMOVEENTRY) {
      assert(c > 0);
      uc = decompressSingle(m->compressInfo,
			    m->set, m->pt, rcval, maxCVSize, rval);
      if ((uc != c) || 
	  (memcmp(val, rval, eSize) != 0))
	HRSerrorExit1("readEntryFromSelfid 1: %s compression functions do not seem to match\n", m->fi.name);
    }
  } else {
    assert(rv > 0);
    HRSreadBytes(m->fi, cval, loc, maxCVSize);
    uc = decompressSingle(m->compressInfo,
			  m->set, m->pt, cval, maxCVSize, val);
    if (uc != rv) 
      HRSerrorExit1("readEntryFromSelfid 2: %s compression functions do not seem to match\n", m->fi.name);

    c = compressSingle(m->compressInfo, m->set, m->pt, val, rcval, maxCVSize);
    ruc = decompressSingle(m->compressInfo,
			   m->set,m->pt, rcval, maxCVSize, rval);
    /*    if ((c != ruc) ||
	(memcmp(val, rval, eSize) != 0))
	HRSerrorExit1("readEntryFromSelfid 3: %s compression functions do not seem to match\n", m->fi.name); */
  }
  bytesRead += maxCVSize;

  HRSfree(val);
  HRSfree(rcval);
  return bytesRead;

}

/* assumes the map type, compression factor, and entry size */
/* have already been set */
/* assumes that m->origKey and m->adjKey have been set */
/* for STRIPED and LL_SPLIT maps*/

static loc_t readSelfID(HRSmap_t m) {
  loc_t loc;
  char *fname = "readSelfID";
  
  if ((m->type == LL_SPLIT_MAP) ||
      (m->type == STRIPED_MAP) || 
      (m->type == OLD_STRIPED_MAP) ) {
    loc = sizeof(int32); /* skip over type */
  
    loc += checkInt32(m->fi, loc, m->block.entrySize, 
		      "CheckSelfID: %s is the wrong entry size\n");
    
    /* skip over compression factor */
    loc += sizeof(int32);

    if (m->def.type == CONSTANTDEFAULT) {
      loc += readEntryFromSelfID(m, loc);
      loc += readEntryFromSelfID(m, loc);
    }

    if (m->type == LL_SPLIT_MAP) {
      HRSkey_desc_s fileAdjKey;
      /* check original key */
      loc += checkInt64(m->fi, loc, m->origKey.ranges[FULLKEY].low,
			"%s has the wrong key lower bound\n");

      loc += checkInt64(m->fi, loc, m->origKey.ranges[FULLKEY].high,
			"%s has the wrong key upper bound\n");

      loc += checkInt32(m->fi, loc, m->blockSplit,
			"%s has the wrong block split\n");

      loc += checkInt32(m->fi, loc, m->stripeSplit,
			"%s has the wrong stripe split\n");

      /* check adjusted key */
      loc += readKDfromFile(m->fi, &fileAdjKey, loc);
      matchKeyDesc(m->adjKey, fileAdjKey);
      
    } else if (m->type == STRIPED_MAP) {
      HRSkey_desc_s fileOrigKey, fileAdjKey;
      /* check original key */
      loc += readKDfromFile(m->fi, &fileOrigKey, loc);
      matchKeyDesc(m->origKey, fileOrigKey);

      /* check adjusted key */
      loc += readKDfromFile(m->fi, &fileAdjKey, loc);
      matchKeyDesc(m->adjKey, fileAdjKey);
    } else {
      assert(m->type == OLD_STRIPED_MAP);
      loc += checkInt32(m->fi, loc, m->adjKey.ranges[BLOCK].low, 
			"%s has the wrong lower bound on primary key \n");
      loc += checkInt32(m->fi, loc, m->adjKey.ranges[BLOCK].high, 
			"%s has the wrong upper bound on primary key \n");
      loc += checkInt32(m->fi, loc, m->adjKey.ranges[STRIPE].low, 
			"%s has the wrong lower bound on secondary key \n");
      loc += checkInt32(m->fi, loc, m->adjKey.ranges[STRIPE].high, 
			"%s has the wrong upper bound on secondary key \n");
      loc += checkInt32(m->fi, loc, m->adjKey.ranges[ENTRY].low, 
			"%s has the wrong lower bound on tertiary key \n");
      loc += checkInt32(m->fi, loc, m->adjKey.ranges[ENTRY].high, 
			"%s has the wrong upper bound on tertiary key \n");
    }      

    return bytealign(loc);

  } else { /* COMPRESSED_SELF_ID_MAP */
    assert(m->type == COMPRESSED_SELF_ID_BV);

    loc = sizeof(int32);  /* skip over type */
    loc += checkInt32(m->fi, loc, m->block.entrySize, 
		      "Self identifcation failed: %s has the wrong entry size\n");

    /* check constant default -- NOT portable */
    if (m->def.type == CONSTANTDEFAULT) {
      char *d = (char *)HRSmalloc(m->block.entrySize, fname);

      HRSreadBytes(m->fi, (uint8 *)d, loc, m->block.entrySize);
      if (memcmp(d, m->def.constant, m->block.entrySize) != 0) 
	HRSerrorExit1("%s: Self identification failed", m->fi.name);
      HRSfree(d);
    }
    /* increment past the default information */
    loc += m->block.entrySize;

    return loc;
  }
}


static int32 writeEntryToSelfID(HRSmap_t m, void *data, loc_t loc, 
				int syncRequired,
				int reAllowed) {
  uint8 *cdata;
  int maxCVSize;
  char *fname = "writeEntryToSelfID";
  int32 rv;

  maxCVSize=m->cf*m->block.entrySize;
  cdata=(uint8*)HRSmalloc(maxCVSize, fname);
  bzero(cdata, maxCVSize);

  rv = compressSingle(m->compressInfo,
		      m->set, m->pt, data, cdata, maxCVSize);

  if ((rv == HRS_REMOVEENTRY) && (reAllowed == 0)) {
    HRSfree(cdata);
    return HRS_REMOVEENTRY;
  }

  loc += write_intNS(m->fi, rv, loc);
  HRSwriteBytes(m->fi, cdata, loc, maxCVSize, syncRequired);
  HRSfree(cdata);

  return sizeof(int32) + maxCVSize;
}


static loc_t writeSelfID(HRSmap_t m) {
  loc_t loc=0;

  assert(m->fi.mode!=READONLY);
  
  loc += write_intNS(m->fi, m->type, loc);
  loc += write_intNS(m->fi, m->block.entrySize, loc);

  if ((m->type == LL_SPLIT_MAP) ||
      (m->type==STRIPED_MAP)) {
    loc += write_intNS(m->fi, m->cf, loc);

    /* write constant twice.  in HRSmapPut we will */
    /* replace the second constant with another value */

    if (m->def.type == CONSTANTDEFAULT) {
      loc += writeEntryToSelfID(m, m->def.constant, loc, NOSYNC, 1);
      loc += writeEntryToSelfID(m, m->def.constant, loc, NOSYNC, 1);
    }

    if (m->type == LL_SPLIT_MAP) {
      loc += write_llNS(m->fi, m->origKey.ranges[FULLKEY].low, loc);
      loc += write_llNS(m->fi, m->origKey.ranges[FULLKEY].high, loc);
      loc += write_intNS(m->fi, m->blockSplit, loc);
      loc += write_intNS(m->fi, m->stripeSplit, loc);
    } else
      loc += writeKDtoFile(m->fi, m->origKey, loc);

    loc += writeKDtoFile(m->fi, m->adjKey, loc);

    sfsync(m->fi.file);
    return bytealign(loc);
  }
  else {
    /* write constant defaults  */
    if (m->def.type == CONSTANTDEFAULT) 
      HRSwriteBytes(m->fi, (uint8 *) m->def.constant, loc, m->block.entrySize, NOSYNC);
    /* increment past the space for the default */
    loc += m->block.entrySize;
    sfsync(m->fi.file);
    return loc;
  }
}


/*******************************************************
**                                                    **
**  Functions for managing the index                  **
**                                                    **
**                                                    **
** LL_SPLIT, STRIPED_MAPS, OLD_STRIPED_MAPS:          **
**  in file:                                          **
**     portable format for (loc_t + int32 size)*      **
**  in memory:                                        **
**     array of C structs with loc_t, int32 size,     **
**     pointer to stripe cache)                       **
**                                                    **
** COMPRESSED_SELF_ID_BV:                             **
**   in file:  (NOT PORTABLE)                         **
**     array of C structs with loc_t, int 32 size,    **
**     and a char full bit.  We believe that there    **
**     are no maps with full bits set.  If there are  **
**     they can be converted with: HRScvtOldMapToNew  **
**   in memory: same as STRIPED_MAPS.                 **
*******************************************************/

static void newIndex(HRSmap_t m) {
  int32 i;
  int32 numEntries=m->adjKey.ranges[BLOCK].high+1;
  int32 s;
  char *fname = "newIndex";

  /* set up the index for later use */
  m->index.numEntries = numEntries;
  m->index.index = 
    (index_entry_s *) HRSmalloc(numEntries*sizeof(index_entry_s), 
				fname);

  m->index.scV = 
    (stripeCache_t **) HRSmalloc(numEntries*sizeof(stripeCache_t *), 
				fname);

  for (i=0; i <numEntries; i++) {
    m->index.index[i].loc = HRS_INVALID;
    m->index.index[i].size = 0;  /* change to HRS_INVALID? */
    m->index.scV[i] = NULL;
  }

  m->index.maxBSseen = HRS_INVALID;

  if (m->type == COMPRESSED_SELF_ID_BV) {
    m->index.fbV = (uint8 *) HRSmalloc(numEntries, fname);
    bzero(m->index.fbV, numEntries);
  }
  else
    m->index.fbV = NULL;

  /* Setting the index updated bit will force a write of the */
  /* index to the file at the end of the execution */
  m->index.updated=1;

  assert((m->type == LL_SPLIT_MAP) ||
	 (m->type == STRIPED_MAP) ||
	 (m->type == COMPRESSED_SELF_ID_BV));

  s = (m->type == COMPRESSED_SELF_ID_BV)
    ? m->index.numEntries*sizeof(index_entry_s)
    : m->index.numEntries*(sizeof(int64)+sizeof(int32));

  m->headerSize = m->selfIDSize + bytealign(s);
 
  m->index.initialized = 1;
}


static void readIndex(HRSmap_t m) {
  int32 i;
  index_entry_s *index;
  int32 numEntries=m->adjKey.ranges[BLOCK].high+1;
  int8 *bufp;
  int8 *tobefreed;
  int32 s;
  char *fname = "readIndex";

  /* set up the index for later use */
  m->index.updated = 0;
  m->index.numEntries = numEntries;
  /* allow OLD_STRIPED_MAP as a type */
  /* because we want to be able to open such */
  /* files before converting them to STRIPED_MAPs */
  assert((m->type == LL_SPLIT_MAP) ||
	 (m->type == STRIPED_MAP) ||
	 (m->type == OLD_STRIPED_MAP) || 
	 (m->type == COMPRESSED_SELF_ID_BV));

  m->index.initialized = 1;

  s = (m->type == COMPRESSED_SELF_ID_BV)
    ? m->index.numEntries*sizeof(index_entry_s)
    : m->index.numEntries*(sizeof(int64)+sizeof(int32));

  s = bytealign(s);

  m->headerSize = m->selfIDSize + s;

  bufp=(int8 *) HRSmalloc(s, fname);
  tobefreed=bufp;

  /*  assert(sizeof(stripeCache_t *)==sizeof(int32)); */
  m->index.index = 
    (index_entry_s *) HRSmalloc(numEntries*sizeof(index_entry_s), fname);
  index=m->index.index;

  m->index.scV = 
    (stripeCache_t **) HRSmalloc(numEntries*sizeof(stripeCache_t *), 
				fname);
  HRSreadBytes(m->fi, (uint8 *)bufp, m->selfIDSize, s);

  m->index.maxBSseen = HRS_INVALID;
  if (m->type==COMPRESSED_SELF_ID_BV) {
    m->index.fbV = (uint8 *) HRSmalloc(numEntries, fname);
    bzero(m->index.fbV, numEntries);
    for (i=0;i<numEntries;i++) {
      int8 fb;
      read_int64(&bufp, &(index[i].loc));
      assert((index[i].loc == HRS_INVALID) ||
	     (index[i].loc >= m->headerSize));
      read_int32(&bufp, &(index[i].size));
      assert(index[i].size >= 0);
      if (m->index.maxBSseen < index[i].size) 
	m->index.maxBSseen = index[i].size;
      read_int8(&bufp, (int8 *) &fb);
      assert((fb == 0) || (fb == 1));
      m->index.fbV[i] = fb;
      bufp=(int8 *)(((int8*)bufp)+3);
      m->index.scV[i] = NULL;
    }
  } else {
    m->index.fbV = NULL;
    for (i=0;i<numEntries;i++) {
      read_int64(&bufp, &(index[i].loc));
      assert((index[i].loc == HRS_INVALID) ||
	     (index[i].loc >= m->headerSize));
      read_int32(&bufp, &(index[i].size));
      assert(index[i].size >= 0);
      if (m->index.maxBSseen < index[i].size) 
	m->index.maxBSseen = index[i].size;
      m->index.scV[i] = NULL;
    }
  }
  HRSfree(tobefreed);
}

/* writes out the whole index on file right after the selfID 
   AND ruins the in-memory representation of the index 
*/
static void writeIndex(HRSmap_t m) {
  int32 i;
  index_entry_s *index=m->index.index;
  int8 *bufp=(int8 *)index;
  int32 numEntries=m->index.numEntries;
  /* size of index in the file */
  int32 s = m->headerSize - m->selfIDSize;  
  int32 bufsize;

  /*  assert(sizeof(stripeCache_t *)==sizeof(int32)); */
  assert(sizeof(int32)/sizeof(int8)==4);

  assert((m->type == LL_SPLIT_MAP) ||
	 (m->type == STRIPED_MAP) ||
	 (m->type == COMPRESSED_SELF_ID_BV));


  if (m->type==COMPRESSED_SELF_ID_BV) {
    /* change the index into the portable format */
    for (i=0;i<numEntries;i++) {
      assert((index[i].loc == HRS_INVALID) ||
	     (index[i].loc >= m->headerSize));
      store_int64(&bufp,index[i].loc);
      store_int32(&bufp,index[i].size);
      store_int8(&bufp, m->index.fbV[i]);
      bufp=(int8 *)(((int8*)bufp)+3);
    }
  } else {
    for (i=0;i<numEntries;i++) {
      assert((index[i].loc == HRS_INVALID) ||
	     (index[i].loc >= m->headerSize));
      store_int64(&bufp,index[i].loc);
      store_int32(&bufp,index[i].size);
    }
  }

  bufsize = bytealign((bufp-(int8 *) index));
  assert(bufsize==s);
  HRSwriteBytes(m->fi, (uint8 *) index, m->selfIDSize, s, SYNC);
}


/* assume map is empty, but the stripe cache may contain */
/* some residual empty blocks.  reset the data structure. */
void resetIndex(HRSmap_t m)
{
  int i;
  int numEntries = m->index.numEntries;

  flushStripe(m);   /* flush the current stripe back to the stripe cache */

  for (i=0;i<numEntries;i++) {
    if (m->index.scV[i] != NULL)
      flushNfreeStripeCache(m,i);
  }
}

static loc_t findEndOfBlocks(HRSmap_t m)
{
  int i;
  loc_t end;
  int32 endPos;
  index_entry_s *index = m->index.index;
  int32 numEntries=m->index.numEntries;

  /* find the end of the blocks.  blocks can't overlap,     */
  /* so we only need to find the block with the largest     */
  /* starting location                                      */
  end = HRS_INVALID;
  for (i=0; i < numEntries; i++)
    if (index[i].loc >= end) {
      end = index[i].loc;
      endPos=i;
    }

  if (end == HRS_INVALID) 
    end = m->headerSize;
  else 
    end = index[endPos].loc + 
      numSubblocks(m, index[endPos].size)*m->block.subBlockSize;

  assert(end>=m->headerSize);
  return end;
}

/*******************************************************
**                                                    **
** Functions for bit vectors                          **
**                                                    **
*******************************************************/

#define test_bv(m, bv, entry) ((((bv) != NULL) && \
    ((((m->type == STRIPED_MAP) || (m->type == LL_SPLIT_MAP)) && \
      (((bv)[(entry)/BVBASE] & (0x1 << ((entry) % BVBASE))))) || \
     ((m->type == COMPRESSED_SELF_ID_BV) && \
      ((int *)bv)[(entry)/CSIDBV_BVBASE] & \
      (0x1 << ((entry) % CSIDBV_BVBASE))))) \
			       ? 1 : 0)

static void set_bv(HRSmap_t m, bv_t bv, int32 entry) {
  assert(bv != NULL);

  if (m->type == COMPRESSED_SELF_ID_BV) 
    ((int32 *) bv)[entry/CSIDBV_BVBASE] = 
      ((int32 *)bv)[entry/CSIDBV_BVBASE] | (0x1 << (entry % CSIDBV_BVBASE));
  else
    bv[entry/BVBASE] = bv[entry/BVBASE] | (0x1 << (entry % BVBASE));
}


static void unset_bv(HRSmap_t m, bv_t bv, int32 entry) {
  if (bv == NULL)
    return;

  if (m->type == COMPRESSED_SELF_ID_BV) 
    ((int32 *) bv)[entry/CSIDBV_BVBASE] = 
      ((int32 *) bv)[entry/CSIDBV_BVBASE] & ~(0x1 << (entry % CSIDBV_BVBASE));
  else 
    bv[entry/BVBASE] = bv[entry/BVBASE] & ~(0x1 << (entry % BVBASE));
}

/*******************************************************
**                                                    **
** Functions for stripes                              **
**                                                    **
** in file: bitvector + compressed data               **
**   (in COMPRESSED_SELF_ID_BV maps, full blocks do   **
**    not have a bitvector)                           **
** in memory:                                         **
**   compressed stripes:                              **
**     C struct with a pointer to the whole stripe,   **
**     a pointer to the bitvector portion of the      **
**     stripe, a pointer to the compressed data       **
**     portion of the stripe, and a size field.       **
**     The stripe pointer and the bitvector pointer   **
**     point to the same place.  The compressed data  **
**     pointer points to the bytes after the          **
**     bitvector.                                     **
**                                                    **
**     If the compressed stripe is in memory, the     **
**     size field contains the size of the compressed **
**     data, that is, the stripe size minus the       **
**     bitvector size.  If the data is not in memory, **
**     the size field can have one of three values:   **
**        0 - which indicates that there is no data   **
**            for this stripe.                        **
**        HRS_IN_FILE - which indicates that there    **
**            is data for this stripe in the map's    **
**            file.                                   **
**        HRS_IN_BACKUPFILE - which indicates that    **
**            there is data for this stripe, but it   **
**            resides in the backup map's file.       **
**                                                    **
**     If the size is 0, the bitvector will be NULL   **
**     unless the stripe is in use. (see below)       **
**                                                    **
**   uncompressed stripe: for any map, we have only   **
**     one stripe uncompressed at any time.           **
**     C struct with the block number and stripe      **
**     number for the "current" uncompressed stripe,  **
**     a pointer to the bitvector for the current     **
**     stripe, space to hold a compressed             **
**     stripe temporarily, space to hold the          **
**     uncompressed data for the current stripe, and  **
**     a bit to indicate whether the current stripe   **
**      has been updated.                             **
**                                                    **
*******************************************************/


static void readCompressedStripe(HRSmap_t m, int32 bn, int32 stripe) {

  loc_t loc;
  loc_t stripeLoc;
  stripeCache_t *cSC=m->index.scV[bn];
  stripeSize_t numBytes;
  uint8 *sp;
  HRSmap_t map;
  int32 ss;
  char *fname = "readCompressedStripe";
  char fullSet;
  int32 bvExtraBytes;

  assert(cSC!=NULL);
  ss = cSC->stripesInMem[stripe].size;
  assert((ss == HRS_IN_FILE) || (ss == HRS_IN_BACKUPFILE));

  numBytes=cSC->stripeLocations[stripe+1]-cSC->stripeLocations[stripe];
  assert(numBytes > 0);

  map = (ss == HRS_IN_FILE) ? m : m->backup;
  fullSet = ((map->type == COMPRESSED_SELF_ID_BV) &&
	     (map->index.fbV[bn] == 1)) ? 1 : 0;
  bvExtraBytes = (fullSet) ? m->block.BVSize : 0;
  sp = (uint8 *) HRSmalloc(numBytes+bvExtraBytes, fname);

  if (fullSet) {
    memset(sp, 0xffff, bvExtraBytes);
    if (m->fi.mode == READWRITE) {
      cSC->blockUpdated = 1;
      m->index.fbV[bn] = 0;
      m->index.updated = 1;
    }
  }

  loc=map->index.index[bn].loc;
  assert(loc>=map->headerSize);

  stripeLoc = loc + cSC->stripeLocations[stripe];
  HRSreadBytes(map->fi, sp+bvExtraBytes, stripeLoc, numBytes); 

  cSC->stripesInMem[stripe].size=numBytes + bvExtraBytes - map->block.BVSize;
  cSC->stripesInMem[stripe].sp = sp;
  cSC->stripesInMem[stripe].cdata = sp + map->block.BVSize;
  cSC->stripesInMem[stripe].bv = (bv_t) sp;
  return;
}


static stripeCache_t *findCompressedStripe(HRSmap_t m, 
					    int32 bn, int32 stripe) {
  int32 numAccesses = m->numAccesses;
  int32 ss;
  stripeCache_t *cSC;

  /* This is used to determine the access pattern of a map */
  if (numAccesses<=ACCESS_ORDER_WINDOW) {
    if (numAccesses==ACCESS_ORDER_WINDOW)
      setAccessOrder(m);
    else if ((m->lastaccessedblock>bn)&&(numAccesses!=0))
      m->isAccessOrderOffdir=1;
    m->numAccesses++;
  }

  /* if the previous block is no longer needed and it
     won't be flushed later. then free it now */
  if ((((m->accessOrder == ONDIR) || HRSmapFlushEager)) &&
      (m->lastaccessedblock != bn) &&
      (m->lastaccessedblock != HRS_INVALID) &&
      (m->uncompressedStripeInfo.bn != m->lastaccessedblock)) {
    stripeCache_t *oldcSC = m->index.scV[m->lastaccessedblock];
    
    if ((oldcSC != NULL) && (oldcSC->blockUpdated == 0)) {
      /* know at this point that old block
         is no longer needed.  get rid of it. */
      freeStripeCache(m, m->lastaccessedblock);
    }
  }

  /* Setting m->lastaccessedblock tells flushSomeStripes 
     that bn is the current block so it doesn't flush 
     it before we are done with it. This is needed since 
     we can run out memory, and therefore start flushing 
     stripeCaches, at any point in time. */
  m->lastaccessedblock=bn;

  cSC = m->index.scV[bn];
  if (cSC == NULL)
    cSC = createStripeCache(m,bn);

  ss = cSC->stripesInMem[stripe].size; 
  if ((ss == HRS_IN_FILE) || (ss == HRS_IN_BACKUPFILE))
    /* stripe is NOT in cache, get it */
    readCompressedStripe(m,bn,stripe);

  return cSC;
}


/* test whether there is a value for the line */
static int32 keyExists(HRSmap_t m, HRS3Key_s k3) {
  stripeCache_t *cSC;
  int v;

  if ((m->index.index[k3.block].loc == HRS_INVALID) &&
      (m->index.scV[k3.block] == NULL))
    return 0;

  cSC = findCompressedStripe(m, k3.block, k3.stripe);
  v = test_bv(m, cSC->stripesInMem[k3.stripe].bv, k3.entry);
  return ((v == 0) ? 0 : 1);
}

/* function for compressing stripes using a user-specified
   or compiler-generated single-entry compression function */

int32 compressStripeEntry(HRSmap_t m,
			  uint8 *to,
			  stripeSize_t to_size)
{
  int32 eSize=m->block.entrySize;
  uint8 *from=m->uncompressedStripeInfo.data;
  int32 bn = m->uncompressedStripeInfo.bn;
  stripeCache_t *cSC=m->index.scV[bn];
  bv_t bv = m->uncompressedStripeInfo.bv;
  int32 e=0;
  int32 i=0;
  stripeSize_t offset=0;
  int32 entriesInStripe=m->block.numStripeEntries;
  HRSentryCompressFn_t crWrapper = (HRSentryCompressFn_t) m->compressInfo.cf;

  for (e=0;e<entriesInStripe;i+=eSize, e++) {
    if (test_bv(m, bv,e)!=0) {
      int32 ns;
      ns = crWrapper(m->set, m->pt, from+i, to+offset, to_size-offset);
      
      if (ns==HRS_REMOVEENTRY) {
	unset_bv(m, bv, e);
	cSC->blockUpdated=1;
	ns=0;
      } else if (ns < 0) {
	HRSerrorExit1("%s: compression function failed\n", m->fi.name);
      }
      
      offset+=ns;
      
      if (offset>to_size)
	HRSerrorExit1("%s: compressed data too large\n", m->fi.name);
    }
  }

  return offset;
}

/*
  Compresses the stripe in m->uncompressedStripeInfo
  and puts it in m->uncompressedStripeInfo.cdata.
  Assumes m->uncompressedStripeInfo.cdata!=NULL.
  Assumes m->uncompressedStripeInfo.data!=NULL.
*/

static stripeSize_t packStripe(HRSmap_t m) {
  int32 eSize=m->block.entrySize;
  uint8 *from=m->uncompressedStripeInfo.data;
  int32 bn = m->uncompressedStripeInfo.bn;
  stripeCache_t *cSC=m->index.scV[bn];
  bv_t bv = m->uncompressedStripeInfo.bv;
  uint8 *to=m->uncompressedStripeInfo.cdata;
  stripeSize_t to_size=m->cf*m->block.stripeSize;
  int32 e=0;
  int32 i=0;
  stripeSize_t offset=0;
  int32 entriesInStripe=m->block.numStripeEntries;

  assert(cSC!=NULL);
  assert(m->uncompressedStripeInfo.bn != HRS_INVALID);
  assert(m->uncompressedStripeInfo.stripe != HRS_INVALID);

  if (m->compressInfo.type == HRS_USER_ENTRY_COMPRESS)
    return compressStripeEntry(m, to, to_size);
  else if ((m->compressInfo.type == HRS_GENERIC_COMPRESS) &&
	   (m->type != LL_SPLIT_MAP)) {
    int osize;

    osize = compressStripeEntry(m, to, to_size);

    if (osize < 0)
      HRSerrorExit1("%s: compression failed\n", m->fi.name);

    return osize;
  }
  else if (m->compressInfo.type == HRS_GENERIC_COMPRESS) {
    /* call compiler generated compression function to
       move data into temp. vector */
    struct extraCompress_s *ec = (struct extraCompress_s *)m->compressInfo.extra;
    uint8 *tmp = ec->tmp;
    int32 tmpSize = ec->tmpSize;
    int osize;

    offset = compressStripeEntry(m, tmp, tmpSize);
    osize = vcapply(ec->cvc, (Void_t *)tmp, offset,  &to, to_size);

    if (osize < 0) {
      HRSerrorExit1("%s: compression failed\n", m->fi.name);
    
    }
    return osize;
  } else {
    /* move data into temp. vector,
       call user specified stripe compression function. */
    struct extraCompress_s *ec = m->compressInfo.extra;
    uint8 *tmp = ec->tmp;
    int32 tmpSize = ec->tmpSize;
    int osize;
    HRSstripeCompressFn_t crStripe = 
      (HRSstripeCompressFn_t) m->compressInfo.cf;
    int32 num;

    assert(m->compressInfo.type == HRS_USER_STRIPE_COMPRESS);

    num = 0;
    for (e=0;e<entriesInStripe;i+=eSize, e++) {
      if (test_bv(m, bv,e)!=0) {
	memcpy(tmp+offset, from+i, eSize);
	offset += eSize;
	num++;
	if (offset>tmpSize)
	  HRSerrorExit1("%s: compressed data too large\n", m->fi.name);
      }
    }

    osize = crStripe(m->set, m->pt, tmp, num, to, to_size);
    if (osize < 0) {
      HRSerrorExit1("%s: compression failed\n", m->fi.name);
    }

    return osize;
  }
}


/* function for decompressing stripes using a user-specified
   or compiler-generated single-entry decompression function */

void decompressStripeEntry(HRSmap_t m, int bn, int stripe,
			    uint8 *from, int32 from_size)
{
  HRSentryDecompressFn_t ucrWrapper = 
    (HRSentryDecompressFn_t) m->compressInfo.df;
  stripeCache_t *cSC=m->index.scV[bn];
  int32 entriesInStripe=m->block.numStripeEntries;
  int32 eSize=m->block.entrySize;
  bv_t bv = cSC->stripesInMem[stripe].bv;
  uint8 *to = m->uncompressedStripeInfo.data;
  stripeSize_t offset_to = 0;
  stripeSize_t offset_from = 0;
  int j;

  bzero(to, eSize*entriesInStripe);
  for(j=0; j < entriesInStripe; j++) {
    if (test_bv(m, bv, j) != 0) {
      offset_from += 
	ucrWrapper(m->set, m->pt, from+offset_from, 
		   from_size-offset_from, to+offset_to);
      
      if (offset_from > from_size)
	HRSerrorExit1("%s: decompression failed\n", m->fi.name);
    }
    offset_to += eSize;
  }
}

/* Uncompresses stripe from stripeCache to 
   m->uncompressedStripe. */
static void unpackStripe(HRSmap_t m, int32 bn, int32 stripe) {
  int32 j;
  stripeCache_t *cSC=m->index.scV[bn];
  int32 entriesInStripe=m->block.numStripeEntries;
  int32 eSize=m->block.entrySize;
  bv_t bv;
  uint8* cdata;
  uint8 *from; 
  stripeSize_t offset_from;
  uint8 *to;
  stripeSize_t offset_to;
  stripeSize_t from_size;
  char *fname = "unpackStripe";

  assert(cSC!=NULL);

  if ((m->uncompressedStripeInfo.bn ==  bn) &&
      (m->uncompressedStripeInfo.stripe ==  stripe))
    /* already unpacked */
    return;

  m->lastaccessedblock = bn;
  flushStripe(m); 
  from_size = cSC->stripesInMem[stripe].size;

  if (from_size != 0) {
    bv = cSC->stripesInMem[stripe].bv;
    cdata = cSC->stripesInMem[stripe].cdata;
    /* This assert ensures that the space we are unpacking from 
       is allocated. */
    assert(cdata != NULL);
    assert(bv != NULL);

    /* This assert ensures that the space we are unpacking to is allocated. */
    assert(m->uncompressedStripeInfo.data!=NULL);

    assert(from_size>0);

    from = cdata;
    offset_from=0;
    to=m->uncompressedStripeInfo.data;
    offset_to=0;

    if (m->compressInfo.type == HRS_USER_ENTRY_COMPRESS)
      decompressStripeEntry(m, bn, stripe, from, from_size);

    else if ((m->compressInfo.type == HRS_GENERIC_COMPRESS) &&
	     (m->type != LL_SPLIT_MAP))
      decompressStripeEntry(m, bn, stripe, from, from_size);

    else if (m->compressInfo.type == HRS_GENERIC_COMPRESS) {
      struct extraCompress_s *ec = (struct extraCompress_s *)m->compressInfo.extra;
      uint8 *tmp = ec->tmp;
      int32 tmpSize = ec->tmpSize;
      int32 osize;

      osize = vcapply(ec->dvc, (void *) from, from_size, &tmp, tmpSize);
      /* osize should be equal to the number of set bits*eSize */
      decompressStripeEntry(m, bn, stripe, tmp, osize);
    } else if (m->compressInfo.type == HRS_USER_STRIPE_COMPRESS) { 
      struct extraCompress_s *ec = 
	(struct extraCompress_s *)m->compressInfo.extra;
      uint8 *tmp = ec->tmp;
      int32 tmpSize = ec->tmpSize;
      HRSstripeDecompressFn_t ucrStripe = 
	(HRSstripeDecompressFn_t) m->compressInfo.df;

      ucrStripe(m->set, m->pt, from, from_size, tmp, tmpSize);
      bzero(to, eSize*entriesInStripe);

      offset_from = 0;
      offset_to = 0;

      for(j=0; j < entriesInStripe; j++) {
	if (test_bv(m, bv, j) != 0) {
	  memcpy(to+offset_to, tmp+offset_from, eSize);
	  offset_from += eSize;
	}
	offset_to += eSize;
      }
    }
  } else {
    /* when from_size == 0, the bitvector needs to be allocated */
    cSC->stripesInMem[stripe].sp = 
      (bv_t) HRSmalloc(m->block.BVSize, fname);
    cSC->stripesInMem[stripe].bv = cSC->stripesInMem[stripe].sp;
    bzero(cSC->stripesInMem[stripe].bv, m->block.BVSize);
  }

  m->uncompressedStripeInfo.stripe=stripe;
  m->uncompressedStripeInfo.bn=bn;
  m->uncompressedStripeInfo.updated=0;
  m->uncompressedStripeInfo.bv = cSC->stripesInMem[stripe].bv;
}

/*
  Compresses the updated, uncompressed stripe from
  m->uncompressedStripeInfo, and copies it to its stripeCache.
*/
static void flushStripe(HRSmap_t m) {
  int32 bn=m->uncompressedStripeInfo.bn;
  int32 stripe=m->uncompressedStripeInfo.stripe;
  stripeSize_t csize;
  stripeSize_t oldsize;
  stripeCache_t *cSC=NULL;

  if  ((bn == HRS_INVALID) || (stripe == HRS_INVALID))
    return;

  cSC = m->index.scV[bn];
  if (m->uncompressedStripeInfo.updated) {
    assert(cSC!=NULL);
    assert(m->uncompressedStripeInfo.cdata!=NULL);

    oldsize=cSC->stripesInMem[stripe].size;
    assert(oldsize >= 0);

    /* compress data into cdata */
    csize= packStripe(m);
    assert(csize >= 0);

    if (csize == 0) {
      HRSfree(cSC->stripesInMem[stripe].sp);
      cSC->stripesInMem[stripe].size = 0;
      cSC->stripesInMem[stripe].sp = NULL;
      cSC->stripesInMem[stripe].bv = NULL;
      cSC->stripesInMem[stripe].cdata = NULL;
    } else {
      if (csize > oldsize){
	cSC->stripesInMem[stripe].sp = 
	  HRSresize(cSC->stripesInMem[stripe].sp, 
		    csize+m->block.BVSize, 
		    "flushStripe");
	cSC->stripesInMem[stripe].bv = cSC->stripesInMem[stripe].sp;
	cSC->stripesInMem[stripe].cdata = 
	  cSC->stripesInMem[stripe].sp + m->block.BVSize;
      }
      cSC->stripesInMem[stripe].size=csize;
      memcpy(cSC->stripesInMem[stripe].cdata, 
	     m->uncompressedStripeInfo.cdata, 
	     csize);
    }

    m->uncompressedStripeInfo.updated=0;
    m->uncompressedStripeInfo.bn=HRS_INVALID;
    m->uncompressedStripeInfo.stripe=HRS_INVALID;
    m->uncompressedStripeInfo.bv=NULL;

    /* Since the stripe in m->uncompressedStripeInfo was updated, 
       the block was updated. */
    cSC->blockUpdated=1;
  }

  /* flush block NOW to save space */
  if ((bn != m->lastaccessedblock) &&
      (cSC != NULL) &&
      ((m->accessOrder == ONDIR) || HRSmapFlushEager))
    flushNfreeStripeCache(m, bn);
}

/*******************************************************
**                                                    **
** Functions managing blocks                          **
**                                                    **
** LL_SPLIT, STRIPED maps:                            **
**   in file: stripe location vector + stripe*        **
**                                                    **
** COMPRESSED_SELF_ID_BV:                             **
**   in file: single stripe                           **
**                                                    **              
*******************************************************/

/* Initialize block data structure.  
   Called by HRSmapOpen. 

   assumes m->cf is set
*/

static void initBlock(HRSmap_t m, int32 entrySize)
{
  int32 bvbytes;
  stripeSize_t tmpCSS;
  char *fname = "initBlock";

  assert((m->type == LL_SPLIT_MAP) ||
	 (m->type == STRIPED_MAP) ||
	 (m->type == OLD_STRIPED_MAP) ||
	 (m->type == COMPRESSED_SELF_ID_BV));

  m->block.entrySize = entrySize;
  m->block.numStripes = m->adjKey.ranges[STRIPE].high+1;
  m->block.numStripeEntries = m->adjKey.ranges[ENTRY].high+1;
  m->block.numEntries = m->block.numStripes * m->block.numStripeEntries;

  m->block.subBlockSize = SUBBLOCKSIZE;

  bvbytes = (m->block.numStripeEntries%BVBASE == 0)
    ? m->block.numStripeEntries/BVBASE
    : (m->block.numStripeEntries/BVBASE + 1);
  m->block.BVSize = bytealign(bvbytes);

  m->block.stripeSize = m->block.numStripeEntries*entrySize;
  tmpCSS = m->block.stripeSize*m->cf;

  /* one extra element in the stripeLocations space is needed
     to store the location of the end of the last stripe */
  m->block.SLSize = 
    bytealign((m->block.numStripes+1)*sizeof(stripeSize_t));
  m->newStripeLocations = (stripeSize_t *)HRSmalloc(m->block.SLSize, fname);

  if (m->type == COMPRESSED_SELF_ID_BV) {
    int32 oldblksize;
    m->block.maxBlockSizeinFile = m->block.BVSize + tmpCSS;
    oldblksize = m->block.numStripeEntries*m->block.entrySize;
    m->block.nsb = (oldblksize*MAX_COMPRESSION)/SUBBLOCKSIZE+1;
  } else {
    m->block.maxBlockSizeinFile = m->block.SLSize 
      + m->block.numStripes*(m->block.BVSize + tmpCSS);
    m->block.nsb = (m->block.maxBlockSizeinFile)/SUBBLOCKSIZE+1;
  }


  m->block.tmpSpace = (uint8 *) HRSmalloc(m->block.maxBlockSizeinFile, fname);

}

/*
  Reads block bn from m->backup->fi.file and writes it out to m->.fi.file.
*/
static void moveBlockFromBackup(HRSmap_t m, int32 bn) {
  stripeSize_t blocksize;
  loc_t new_loc;
  HRSmap_t backup = m->backup;
  int32 bvExtraBytes;

  assert(m->fi.mode!=READONLY);
  assert(m->index.index[bn].loc==HRS_IN_BACKUPFILE);
  assert(m->index.scV[bn] == NULL);
  assert(m->type == backup->type);

  assert(backup!=NULL);
  assert(backup->index.index[bn].loc>=m->headerSize);

  assert(m->block.tmpSpace!=NULL);

  blocksize = backup->index.index[bn].size;

  if ((backup->type == COMPRESSED_SELF_ID_BV) &&
      (backup->index.fbV[bn] == 1)) {
    bvExtraBytes = m->block.BVSize;
    memset(m->block.tmpSpace, 0xffff, bvExtraBytes);
    m->index.fbV[bn] = 0;
  }
  else
    bvExtraBytes = 0;

  HRSreadBytes(backup->fi, m->block.tmpSpace+bvExtraBytes, 
	       backup->index.index[bn].loc, 
	       blocksize);
  
  blocksize += bvExtraBytes;

  new_loc = allocFileSpace(m, blocksize);
  HRSwriteBytes(m->fi, m->block.tmpSpace, new_loc, blocksize, SYNC);
  m->index.index[bn].loc = new_loc;
  m->index.index[bn].size = blocksize;
  if (m->index.maxBSseen < blocksize) 
    m->index.maxBSseen = blocksize;
  m->index.updated=1;
}

/* constructs block in memory and returns the size of the constructed block */
static stripeSize_t constructBlock(HRSmap_t m, int32 bn)
{
  int32 i;
  stripeCache_t *cSC=m->index.scV[bn];
  int32 numStripes=m->block.numStripes;
  stripeSize_t mloc;
  stripeSize_t totBlockSize = 0;

  assert(cSC != NULL);
  assert(m->block.tmpSpace != NULL);

  /* save space in block the SLV, if necessary */
  mloc = ((m->type == STRIPED_MAP) || (m->type == LL_SPLIT_MAP)) ? m->block.SLSize : 0;

  
  m->newStripeLocations[0]=mloc;
  for (i=0;i<numStripes;i++) {
    if ((cSC->stripesInMem[i].size == HRS_IN_FILE) ||
	(cSC->stripesInMem[i].size == HRS_IN_BACKUPFILE)) {
      HRSmap_t map = 
	(cSC->stripesInMem[i].size == HRS_IN_FILE) ? m : m->backup;
      loc_t blockLoc = map->index.index[bn].loc;
      int32 oldStripeSize = cSC->stripeLocations[i+1]-cSC->stripeLocations[i];
      loc_t stripeLoc = blockLoc + cSC->stripeLocations[i];

      assert(blockLoc >= map->headerSize);
      assert(oldStripeSize > 0);
      assert((cSC->stripeLocations[i]+oldStripeSize) <= map->index.index[bn].size);

      HRSreadBytes(map->fi, (uint8 *) (m->block.tmpSpace+mloc),
		   stripeLoc, oldStripeSize);
      totBlockSize+=oldStripeSize;
      mloc+=oldStripeSize;
      m->newStripeLocations[i+1]=m->newStripeLocations[i]+oldStripeSize;
    } else if (cSC->stripesInMem[i].size == 0) {
      /* no data for this stripe anywhere */
      m->newStripeLocations[i+1]=m->newStripeLocations[i];
    } else {
      /* data is in the stripe cache */
      int32 ss = cSC->stripesInMem[i].size + m->block.BVSize;

      assert(ss > 0);
      memcpy(m->block.tmpSpace+mloc, cSC->stripesInMem[i].sp, ss);
      totBlockSize+=ss;
      mloc+=ss;
      m->newStripeLocations[i+1]=m->newStripeLocations[i]+ss;
    }
  }

  /* prepend stripes with stripe location vector */
  if ((totBlockSize > 0) && 
      ((m->type == STRIPED_MAP) || (m->type == LL_SPLIT_MAP)))
    totBlockSize += 
      portSL(m, m->newStripeLocations, (stripeSize_t *)m->block.tmpSpace);

  return totBlockSize;
}

int32 blockIsEmpty(HRSmap_t m, int32 bn) {
  int32 j;
  int32 numStripes=m->block.numStripes;

  if (m->index.scV[bn]==NULL) {
    if (m->index.index[bn].loc == HRS_INVALID)
      return 1;
    else {
      /* m->index.index[bn].loc == HRS_IN_BACKUPFILE || */
      /* m->index.index[bn].loc >= m->headerSize */
      return 0;
    }
  } else {
    for (j=0;j<numStripes;j++) {
      int ss = m->index.scV[bn]->stripesInMem[j].size;
      if ((ss == HRS_IN_FILE) ||
	  (ss == HRS_IN_BACKUPFILE) ||
	  (ss > m->block.BVSize))
	return 0;
      else
	assert(ss == 0);
    }
  }

  return 1;
}

int32 mapIsEmpty(HRSmap_t m) {
  int32 i;
  int32 numEntries;

  if (m->index.initialized == 0) {
    readIndex(m);
    if (m->fi.mode != READONLY)
      readFreeList(m);
  } 

  if (m->uncompressedStripeInfo.updated) {
    flushStripe(m);
  }

  numEntries=m->index.numEntries;
  for (i=0;i<numEntries;i++)
    if (!blockIsEmpty(m,i))
      return 0;
  return 1;
}


/*******************************************************
**                                                    **
** Functions managing a stripeCache                   **
**                                                    **
** in memory structure only:                          **
**    one per block.                                  **
**    contains stripe location vector, compressed     **
**    stripes, and a block updated bit.               **
**                                                    **
*******************************************************/

/*
  Creates m->index.scV[bn].
*/
static stripeCache_t *createStripeCache(HRSmap_t m, int32 bn) {
  int32 i;
  stripeCache_t *cSC;
  stripe_t *sIM;
  stripeSize_t *sL;
  int32 numStripes=m->block.numStripes;
  int tag;
  char *fname = "createStripeCache";

  assert(m->index.scV[bn]==NULL);

  m->index.scV[bn] =
    (stripeCache_t *)HRSmalloc(sizeof(stripeCache_t), fname);
  cSC=m->index.scV[bn];

  cSC->stripeLocations =
    (stripeSize_t *)HRSmalloc(m->block.SLSize, fname);
  sL=cSC->stripeLocations;

  cSC->stripesInMem =
    (stripe_t *)HRSmalloc(sizeof(stripe_t)*numStripes, fname);
  sIM=cSC->stripesInMem;

  /* setting stripe location vector */
  if (m->index.index[bn].loc == HRS_INVALID) {
    for(i=0;i<numStripes+1;i++) 
      sL[i]=0;
    tag = 0;
  } else {
    HRSmap_t map;

    if (m->index.index[bn].loc >=m->headerSize) {
      map = m;
      tag = HRS_IN_FILE;
    }
    else {
      assert(m->index.index[bn].loc == HRS_IN_BACKUPFILE);
      map = m->backup;
      tag = HRS_IN_BACKUPFILE;
    }

    if (map->type == COMPRESSED_SELF_ID_BV) {
      sL[0]=0;
      sL[1]=map->index.index[bn].size;
    } else {
      loc_t loc=map->index.index[bn].loc;
      readArrayOfInts(map->fi,(uint8 *) sL,loc,map->block.SLSize);
    }
  }

  for (i=0;i<numStripes;i++) {
    if (sL[i+1]-sL[i] == 0)
      sIM[i].size = 0;
    else
      sIM[i].size = tag;

    sIM[i].bv = NULL;
    sIM[i].cdata = NULL;
    sIM[i].sp = NULL;
  }

  if (tag == HRS_IN_BACKUPFILE)
    cSC->blockUpdated=1;
  else
    cSC->blockUpdated=0;

  return cSC;
}

/* Frees the space taken up by m->index.scV[bn] */
static stripeSize_t freeStripeCache(HRSmap_t m, int32 bn) {
  int32 j;
  stripeSize_t freedBytes=0;
  stripeCache_t *cSC=m->index.scV[bn];
  stripe_t *sIM=cSC->stripesInMem;
  stripeSize_t *sL=cSC->stripeLocations;
  int32 numStripes=m->block.numStripes;

  for(j=0;j<numStripes;j++)
    if (sIM[j].sp != NULL) {
      HRSfree(sIM[j].sp);
      freedBytes += sIM[j].size + m->block.BVSize;
    }

  HRSfree(sIM);
  cSC->stripesInMem=NULL;
  freedBytes+=sizeof(stripe_t)*numStripes;

  HRSfree(sL);
  cSC->stripeLocations=NULL;
  freedBytes+=m->block.SLSize;

  HRSfree(cSC);
  m->index.scV[bn]=NULL;
  freedBytes+=sizeof(stripeCache_t);

  return freedBytes;
}

/*
  Flushes (if bn was updated) and frees bn's stripeCache.
  Returns number of bytes freed.  
*/
static stripeSize_t flushNfreeStripeCache(HRSmap_t m,int32 bn) {
  stripeSize_t newBlockSize;
  stripeSize_t freedBytes;
  loc_t oldloc;
  int32 oldBlockSize;
  loc_t newloc;

  assert(m->index.scV[bn] != NULL);

  if (m->index.scV[bn]->blockUpdated == 0)
    return freeStripeCache(m,bn);

  assert(m->fi.mode!=READONLY);

  /* construct the updated block in memory */
  newBlockSize = constructBlock(m, bn);
  
  /* done with stripe cache */
  freedBytes = freeStripeCache(m, bn);

  oldloc = m->index.index[bn].loc;
  oldBlockSize = m->index.index[bn].size;
  m->index.index[bn].loc = HRS_INVALID;
  m->index.index[bn].size = 0;

  if ((newBlockSize > 0) &&
      (enoughSpace(m, oldloc, oldBlockSize, newBlockSize))) {
    /* the current block has enough space for the new size */
    m->index.index[bn].loc = oldloc;
    m->index.index[bn].size = newBlockSize;
    HRSwriteBytes(m->fi, m->block.tmpSpace, oldloc, newBlockSize, SYNC);      
  }
  else {
    if (oldloc >= m->headerSize)
      freeFileSpace(m, oldloc, oldBlockSize);

    if (newBlockSize > 0) {
      newloc = allocFileSpace(m, newBlockSize);
      m->index.index[bn].loc=newloc;
      m->index.index[bn].size=newBlockSize;
      HRSwriteBytes(m->fi, m->block.tmpSpace, newloc, newBlockSize, SYNC);
    }
  }

  if (m->index.maxBSseen < m->index.index[bn].size)
    m->index.maxBSseen = m->index.index[bn].size;

  m->index.updated = 1;  /* updated index with new block size */
  return freedBytes; 
}


/*
  Flushes and frees all the stripeCaches as well as
  finishes the lazy copy from m->backup to m->fi.file.
*/
static void closeStripeCaches(HRSmap_t m) {
  int32 i;
  int32 numEntries=m->index.numEntries;

  for (i=0;i<numEntries;i++) {
    if (m->index.scV[i] != NULL)
      flushNfreeStripeCache(m,i);
    else if (m->index.index[i].loc == HRS_IN_BACKUPFILE)
      moveBlockFromBackup(m,i);
  }
}

/*******************************************************
**                                                    **
** Functions maintaining the freelist                 **
**                                                    **
** in file: ((loc_t)* 0)*                             **
** in memory:                                         **
**   vector of free list entries.                     ** 
**     one per number of subblocks                    **
**   location that marks the current end of the       **
**     blocks in the file.                            **
**                                                    **
*******************************************************/

static void resetFreeList(HRSmap_t m, loc_t newEnd) {
  int i;
  freeListEntry_t prev;
  freeListEntry_t curr;
  int32 nb = m->block.nsb;

  for (i=0; i < nb; i++) {
    prev = NULL;
    curr = m->fl.fl[i];
    while (curr != NULL) {
      if (curr->loc >= newEnd) {
	freeListEntry_t tmp;
	tmp = curr;

	/* this free list entry is obsolete */
	if (prev == NULL)
	  /* remove it from the front */
	  m->fl.fl[i] = curr->next;
	else
	  /* remove it from the middle */
	  prev->next = curr->next;

	curr = curr->next;
	/* prev does not change  */
	HRSfree(tmp);
      }
      else {
	prev = curr;
	curr = curr->next;
      }
    }
  }
}

/* 
   Reads the free list from a map file.
*/
static void readFreeList(HRSmap_t m) {
  loc_t c;
  loc_t end;
  int32 b;
  int32 nb = m->block.nsb;
  loc_t fileEnd = (loc_t) sfsize(m->fi.file);
  char *fname = "readFreeList";

  /* initialize the buckets */
  m->fl.fl = (freeListEntry_t *) HRSmalloc(sizeof(freeListEntry_t)*nb,
					   fname);
  for (b=0; b < nb; b++)
    m->fl.fl[b] = NULL;

  end = findEndOfBlocks(m);
  m->fl.blockEnd = end;
  if ((end == fileEnd) ||
      (end == m->headerSize))
    return;  /* done */
  
  /* start of the free list is at the end of the blocks  */
  c = end;

  for (b=0; b < nb; ) {
    loc_t loc;
    
    c += read_loc_t(m->fi, &loc, c);

    if (loc == 0)
      /* end of bucket */
      b++;
    else { 
      freeListEntry_t fle = 
	(freeListEntry_t) HRSmalloc(sizeof(freeListEntry_s), fname);
      
      fle->loc = loc;
      fle->next = m->fl.fl[b];
      m->fl.fl[b] = fle;
    }
  }

  assert(c == fileEnd);
}

/* writes out the freelist to file, after the blocks */
static void writeFreeList(HRSmap_t m) {
  freeListEntry_t fle;
  int32 b;
  loc_t endMarker = 0;
  int32 nb = m->block.nsb;
  loc_t end;
  
  /* sanity check */
  end = findEndOfBlocks(m);
  assert(end == m->fl.blockEnd);

  if (end > m->headerSize) {
    for (b=0; b < nb; b++) {
      for (fle = m->fl.fl[b]; fle != NULL; ) {
	freeListEntry_t tmp = fle;
	end += write_loc_tNS(m->fi, fle->loc, end);
	fle = fle->next;
	HRSfree(tmp);
      }
      end += write_loc_tNS(m->fi, endMarker, end);
    }
  }
  
  ftruncate(sffileno(m->fi.file), end);
  sfsync(m->fi.file);
}

/* brain dead space allocation for now. */
static loc_t allocFileSpace(HRSmap_t m, stripeSize_t size) {
  loc_t temp;
  int32 b = numSubblocks(m, size);

  assert(size > 0);
  assert(size <= m->block.maxBlockSizeinFile);
  assert(b <= m->block.nsb);

  assert(m->fi.mode!=READONLY);

  if (m->fl.fl[b] != NULL) {
    freeListEntry_t fle = m->fl.fl[b];
    temp = fle->loc;
    m->fl.fl[b] = fle->next;
    HRSfree(fle);
  } else {
    /* put at end of file */
    temp = m->fl.blockEnd;
    m->fl.blockEnd += b*m->block.subBlockSize;
  }
  return temp;
}


int enoughSpace(HRSmap_t m, loc_t oldLoc, stripeSize_t oldSize,  
		stripeSize_t newSize)
{
  loc_t blkEnd;
  int oldb, newb;

  if (oldLoc < m->headerSize)
    return 0;

  oldb = numSubblocks(m, oldSize);
  newb = numSubblocks(m, newSize);

  if (oldb == newb)
    return 1;

  blkEnd = oldLoc+oldb*m->block.subBlockSize; 
  if (blkEnd == m->fl.blockEnd) {
    m->fl.blockEnd = oldLoc+newb*m->block.subBlockSize;
    return 1;
  }
  
  return 0;
}


static void freeFileSpace(HRSmap_t m, loc_t loc, stripeSize_t size) {
  freeListEntry_t fle;
  int32 b;
  loc_t blkEnd;
  char *fname = "freeFileSpace";

  assert(loc>=0);
  assert(size > 0);
  assert(size <= m->block.maxBlockSizeinFile);
  assert(m->fi.mode!=READONLY);

  fle = (freeListEntry_t) HRSmalloc(sizeof(freeListEntry_s), fname);
  fle->loc = loc;
  b = numSubblocks(m, size);
  fle->next = m->fl.fl[b];
  m->fl.fl[b] = fle;

  blkEnd = loc+b*m->block.subBlockSize;
  assert(blkEnd <= m->fl.blockEnd);
  /* if the block that we just freed was the last one
     in the file, we want to remove newly invalid free
     list entry */
  if (blkEnd == m->fl.blockEnd) {
    loc_t newEnd = findEndOfBlocks(m);
    assert(newEnd >= m->headerSize);
    assert(m->fl.blockEnd > newEnd);
    m->fl.blockEnd = newEnd;
    resetFreeList(m, newEnd);
  }
  return;
}

/*******************************************************
   Auxiliary routines used to by HRSmapOpen.
      
*******************************************************/

HRSmap_t checkOpenMaps(char *fname, int32 readonly, int32 mode, 
		       HRSkey_desc_s kd,
		       int32 blockSplit, int32 stripeSplit,
		       int32 entrySize, 
		       HRSmapDefault_s entryDefault)
{
  HRSmap_t tempmap = headMapList;
  
  while ((tempmap != NULL) &&
	 (strcmp(fname,tempmap->fi.name) != 0))
    tempmap=tempmap->next;

  if (tempmap == NULL)
    return NULL;

  if (tempmap->fi.mode!=readonly)
    HRSerrorExit1("HRSmapOpen: %s opened multiple times. \nMaps must either be opened as const all times or zero times.\n",fname);
  
  if (mode==NEW)
    HRSerrorExit1("HRSmapOpen: %s opened multiple times. \nA map can be opened as new only the first time.\n",fname);

  matchKeyDesc(tempmap->origKey, kd);
  if (tempmap->type == LL_SPLIT_MAP) {
    if ((tempmap->blockSplit != blockSplit) ||
	(tempmap->stripeSplit != stripeSplit)) {
      HRSerrorExit1("HRSmapOpen: reopening map %s with a different map type \n",
		    fname);
      
    }
  }

  if (tempmap->block.entrySize != entrySize)
    HRSerrorExit1("HRSmapOpen: reopening map %s with a different map type \n",
		  fname);

  if (tempmap->def.type != entryDefault.type) 
    HRSerrorExit1("HRSmapOpen: reopening map %s with a different map type \n",
		  fname);

  if (entryDefault.type == CONSTANTDEFAULT) {
    int i;
    for (i=0; i < entrySize; i++) {
      if (tempmap->def.constant[i] != entryDefault.constant[i])
	HRSerrorExit1("HRSmapOpen: reopening map %s with a different map type \n",
		      fname);
    }
  } else {  /* flag == FUNCTIONDEFAULT or PARAMFUNCTIONDEFAULT */
    if (tempmap->def.f != entryDefault.f)
      HRSerrorExit1("HRSmapOpen: reopening map %s with a different map type \n",
		    fname);

  }

  return tempmap;
} 

/*
  Initializes the default field in the map with entryDefault.
  This is the value that will be returned when the application
  accesses a field that is not stored in the map.
*/
static void initDefault(HRSmap_t m, HRSmapDefault_s entryDefault, int32 eSize) 
{
  char *fname  ="initDefault";

  m->def.type = entryDefault.type;
  if (entryDefault.type == CONSTANTDEFAULT) {
    m->def.constant = (void *) HRSmalloc(eSize, fname);
    m->def.f = NULL;
    memcpy(m->def.constant, entryDefault.constant, eSize);
  } else {  
    m->def.constant = NULL;
    m->def.f = entryDefault.f;
  }
}


/*******************************************************
**                                                    **
** Runtime system interface functions                 **
**                                                    **
*******************************************************/

/* Creates the in-memory representation of the map. */
HRSmap_t HRSmapOpen(char *filename, int32 entrySize, 
		    HRSmapDefault_s entryDefault,
		    int32 readonly, int32 mode, 
		    char toplevel,
		    HRSkey_desc_s keyDesc,
		    HRSmapCompress_s compressInfo) 
{

  HRSmap_t m;
  HRSmap_t tempmap=headMapList;
  int32 fileexists;
  struct stat stbuf;
  int32 errLen = 50+strlen(filename)+1;
  int i;
  char *fname = "HRSmapOpen";

  HRSexitSetup();

  /* checks to see if map is already open.  */
  /* if so, check that new declaration is consistent with */
  /* the old one */
  tempmap = checkOpenMaps(filename, readonly, mode, keyDesc, 
			  HRS_INVALID,  /* not used for non LL_SPLIT maps */
			  HRS_INVALID,  /* not used for non LL_SPLIT maps */
			  entrySize,
			  entryDefault);
  if (tempmap != NULL)
    /* found a matching open map */
    return tempmap;

  fileexists = (stat(filename, &stbuf) != -1);

  /* checks that the status of the file is consistent with the mode */
  switch (mode) {
  case NEW:
    if (fileexists)
      HRSerrorExit1("%s: map with mode NEW cannot exist\n", filename);
    assert(readonly == 0);
    break;

  case EXISTS:
    if (fileexists == 0)
      HRSerrorExit1("%s: map with mode EXISTS must exist\n", filename);
    break;

  case DONTCARE:
    if ((fileexists == 0) && (readonly))
      HRSerrorExit1("%s: cannot create map declared as CONST\n", filename);

    break;

  default:
    HRSerrorExit1("%s: unknown mode in HRSmapOpen\n", filename);
  }

  setMemoryUsageLimit();

  m = (HRSmap_t) HRSmalloc(sizeof(struct entry), fname);
  
  m->complete = 1;

  m->fi.name = HRSmalloc(strlen(filename)+1, fname);
  strcpy(m->fi.name, filename);
  m->fi.file = HRSopenfile(filename, (readonly == 1) ? "r" : "rw");
  m->fi.mode = readonly;    

  /* set type and compression factor from file */
  if (fileexists) {
    int32 type;
    sfsync(m->fi.file);
    read_int(m->fi, &type, 0);
    if ((type != COMPRESSED_SELF_ID_BV)&&(type!= STRIPED_MAP)) {
      if (type == OLD_STRIPED_MAP) {
	sfprintf(sfstderr, "WARNING: OLD_STRIPED_MAP format is no longer supported.  Maps of this type can be used only with HRScvtOldMaptoNew\n");
      } else 
	HRSerrorExit1("%s is of unknown type.\n", m->fi.name);
    }
    m->type=type;
    if ((type == STRIPED_MAP) || (type == OLD_STRIPED_MAP))
      read_int(m->fi, &m->cf, sizeof(int32)*2);
    else
      m->cf = MAX_COMPRESSION;
  }
  else {
    /* assume all NEW maps are striped maps for now */
    /* can only be changed in MAP COPY              */
    m->type = STRIPED_MAP;
    m->cf = MAX_COMPRESSION;
  }

  /* save the original key description for validating keys */
  m->origKey.levels = keyDesc.levels;
  for (i=0; i < keyDesc.levels; i++) {
    m->origKey.ranges[i].low = keyDesc.ranges[i].low;
    m->origKey.ranges[i].high = keyDesc.ranges[i].high;
  }

  for (i = keyDesc.levels; i < MAXLEVELS; i++) {
    m->origKey.ranges[i].low = HRS_INVALID;
    m->origKey.ranges[i].high = HRS_INVALID;
  }

  m->adjKey = adjKeyDesc(m, keyDesc);

  m->compressInfo = compressInfo;
  m->compressInfo.extra = NULL;



  m->set = 0;
  m->ptSize = 0;
  m->pt = NULL;

  initBlock(m, entrySize);
  initDefault(m, entryDefault, entrySize);

  if (fileexists) {
    m->selfIDSize = readSelfID(m);
    m->index.initialized = 0;
    m->index.updated = 0;
    m->index.index = NULL;
    m->index.scV = NULL;
  }
  else {
    assert(m->fi.mode != READONLY);
    m->selfIDSize = writeSelfID(m);
    newIndex(m);
    readFreeList(m);
  }

  if ((m->type == STRIPED_MAP) && (m->def.type == CONSTANTDEFAULT))
    m->updateSelfID = 1;
  else
    m->updateSelfID = 0;

  /*
    if (m->fi.mode!=READONLY)
    readFreeList(m); */

  /* do not uncompressed stripes for OLD_STRIPED_MAPS */
  if ((m->type == STRIPED_MAP) || 
      (m->type == COMPRESSED_SELF_ID_BV)) {
    m->uncompressedStripeInfo.data = 
      (uint8 *) HRSmalloc(m->block.stripeSize, fname);
    m->uncompressedStripeInfo.cdata = 
      (uint8 *) HRSmalloc(m->block.stripeSize*m->cf, fname);
    m->uncompressedStripeInfo.updated = 0;
    m->uncompressedStripeInfo.bn = HRS_INVALID;
    m->uncompressedStripeInfo.stripe = HRS_INVALID;
    m->uncompressedStripeInfo.bv = NULL;
  }

  /* Sets the default access pattern and the default number 
     of blocks to be freed when running out of memory */
  m->accessOrder=OFFDIR;
  if ((m->index.numEntries*m->block.maxBlockSizeinFile) <NUM_BYTES_2B_FREED_OFFDIRDEFAULT)
    m->num_bytes_2B_freed=HRS_FREEALL;
  else
    m->num_bytes_2B_freed=NUM_BYTES_2B_FREED_OFFDIRDEFAULT;

  /* start with an unbuffered stream */
  setFileBufferSize(m, 0);

  m->isAccessOrderOffdir=0;
  m->numAccesses=0;
  m->lastaccessedblock = HRS_INVALID;

  m->backup = NULL;
  m->copyto = NULL;

  /* add map to the list of top-level data structures */
  /* if necessary                                     */
  if (toplevel)
    HRSaddPDS(MAP, m);

  /* add map to list of maps */
  m->next = headMapList;
  headMapList = m;
  return m;
}

/* Creates the in-memory representation of LL_SPLIT maps. */
HRSmap_t HRSLLmapOpen(char *filename, 
		      int32 entrySize, 
		      HRSmapDefault_s entryDefault,
		      int32 readonly, 
		      int32 mode, 
		      char toplevel,
		      HRSkey_desc_s keyDesc,
         	      int32 blockSplit,
		      int32 stripeSplit,
		      HRSmapCompress_s compressInfo,
		      char set,
		      int32 ptSize,
		      void *pt) 
{
  HRSmap_t m;
  HRSmap_t tempmap=headMapList;
  int32 fileexists;
  struct stat stbuf;
  int32 errLen = 50+strlen(filename)+1;
  char *fname = "HRSmapOpen";
  char keyComplete = 1;
  char defComplete = 1;

  HRSexitSetup();

  /* use HRSmapOpen for old style keys */
  if (keyDesc.levels != 1) 
   return HRSmapOpen(filename, entrySize, entryDefault,
		     readonly, mode, toplevel, keyDesc,
		     compressInfo);


  /* checks to see if map is already open.  */
  /* if so, check that new declaration is consistent with */
  /* the old one */
  tempmap = checkOpenMaps(filename, readonly, mode,
			  keyDesc,  blockSplit, stripeSplit,
			  entrySize, entryDefault);
  if (tempmap != NULL)
    /* found a matching open map */
    return tempmap;

  fileexists = (stat(filename, &stbuf) != -1);

  /* checks that the status of the file is consistent with the mode */
  switch (mode) {
  case NEW:
    if (fileexists)
      HRSerrorExit1("%s: map with mode NEW cannot exist\n", filename);
    assert(readonly == 0);
    break;

  case EXISTS:
    if (fileexists == 0)
      HRSerrorExit1("%s: map with mode EXISTS must exist\n", filename);
    break;

  case DONTCARE:
    if ((fileexists == 0) && (readonly) )
      HRSerrorExit1("%s: map with mode CONST must exist\n", filename);
    break;

  default:
    HRSerrorExit1("%s: unknown mode in HRSmapOpen\n", filename);
  }

  setMemoryUsageLimit();

  m = (HRSmap_t) HRSmalloc(sizeof(struct entry), fname);

  m->fi.name = HRSmalloc(strlen(filename)+1, fname);
  strcpy(m->fi.name, filename);
  m->fi.file = HRSopenfile(filename, (readonly == 1) ? "r" : "rw");
  m->fi.mode = readonly;    

  /* set type and compression factor from file */
  if (fileexists) {
    int32 type;
    loc_t loc = 0;
    sfsync(m->fi.file);

    loc += read_int(m->fi, &type, loc);
    if (type != LL_SPLIT_MAP)
      HRSerrorExit1("%s is of unknown type.\n", m->fi.name);
    m->type=type;

    /* skip over entry size */
    loc += sizeof(int32);

    loc += read_int(m->fi, &m->cf, loc);

    /* skip over constants */
    if (entryDefault.type == MISSINGCONSTANTDEFAULT) {
      int maxCVSize = m->cf*entrySize;
      uint8 *cval;
      int32 rv;
      int uc;

      entryDefault.type = CONSTANTDEFAULT;
      read_int(m->fi, &rv, loc);
      if (rv <= 0)
	HRSerrorExit1("%s: constant default must be available in the file\n",
		      m->fi.name);
      loc += sizeof(int);

      cval = (uint8 *) HRSmalloc(maxCVSize, fname);
      HRSreadBytes(m->fi, cval, loc, maxCVSize);
      if (compressInfo.type == HRS_USER_STRIPE_COMPRESS) {
	uc = ((HRSstripeDecompressFn_t) 
	      compressInfo.df)(m->set,
			       m->pt,
			       cval,
			       maxCVSize,
			       (uint8 *) entryDefault.constant,
			       1);
      } else {
	uc = ((HRSentryDecompressFn_t) 
	      compressInfo.df)(m->set, m->pt, cval, maxCVSize, 
			(uint8 *) entryDefault.constant);
      }

      if (uc != rv)
	HRSerrorExit1("%s: default decompressed failed \n", m->fi.name);
      loc += m->cf*entrySize;
    }
    else
      loc += sizeof(int32) + m->cf*entrySize;
    loc += sizeof(int32) + m->cf*entrySize;

    /* if parameters are not supplied.  pull info from file */
    if (keyDesc.ranges[0].low < 0) {
      /* retrieve from the file */
      read_ll(m->fi, &keyDesc.ranges[FULLKEY].low, loc);
    }
    loc += sizeof(int64);

    if (keyDesc.ranges[0].high < 0) {
      /* retrieve from the file */
      read_ll(m->fi, &keyDesc.ranges[FULLKEY].high, loc);
    }
    loc += sizeof(int64);

    if (blockSplit < 0) {
      /* retrieve from the file */
      read_int(m->fi, &blockSplit, loc);
    }
    loc += sizeof(int32);

    if (stripeSplit < 0) {
      /* retrieve from the file */
      read_int(m->fi, &stripeSplit, loc);
    }

  }
  else {
    /* assume all NEW maps are LL_STRIP_MAP for now */
    /* can only be changed in MAP COPY              */
    m->type = LL_SPLIT_MAP;
    m->cf = MAX_COMPRESSION;
    assert(keyDesc.levels == 1);
    if ((keyDesc.ranges[0].low < 0) || (keyDesc.ranges[0].high < 0))
      keyComplete = 0;
    if ((blockSplit < 0) || (stripeSplit < 0)) 
      keyComplete = 0;
    if (entryDefault.type == MISSINGCONSTANTDEFAULT)
      defComplete = 0;
  }

  m->set = set;
  m->ptSize = ptSize;
  if (ptSize != 0) {
    m->pt = HRSmalloc(ptSize, "HRSLLmapOpen");
    memcpy(m->pt, pt, ptSize);
  } else 
    m->pt = NULL;

  /* save the original key description for validating keys */
  assert(keyDesc.levels == 1);
  m->origKey.levels = 1;
  m->origKey.ranges[0].low = keyDesc.ranges[0].low;
  m->origKey.ranges[0].high = keyDesc.ranges[0].high;

  m->origKey.ranges[1].low = HRS_INVALID;
  m->origKey.ranges[1].high = HRS_INVALID;
  m->origKey.ranges[2].low = HRS_INVALID;
  m->origKey.ranges[2].high = HRS_INVALID;

  m->blockSplit = blockSplit;
  m->blockP2 = isP2(blockSplit, &m->blockShift);
  m->stripeSplit = stripeSplit;
  m->stripeP2 = isP2(stripeSplit, &m->stripeShift);

  m->compressInfo = compressInfo;
  if ((compressInfo.type == HRS_GENERIC_COMPRESS) ||
      (compressInfo.type == HRS_USER_STRIPE_COMPRESS)) {
    struct extraCompress_s *extra = 
      (struct extraCompress_s *) HRSmalloc(sizeof(struct extraCompress_s), "");

    if (compressInfo.type == HRS_GENERIC_COMPRESS) {
      extra->cVcdisc.size = 0;
      extra->cVcdisc.data = "";
      extra->cVcdisc.offset = 0;
      extra->cVcdisc.eventf = NULL;
      extra->cvc = vcopen(&extra->cVcdisc, Vcrle, 0, 0);

      extra->dVcdisc.size = 0;
      extra->dVcdisc.data = "";
      extra->dVcdisc.offset = 0;
      extra->dVcdisc.eventf = NULL;
      extra->dvc = vcopen(&extra->dVcdisc, Vcunrle, 0, 0);

    }

    if (stripeSplit < 0)
      extra->tmp = NULL;
    else {
      /* enough space to hold one full stripe */
      extra->tmpSize = stripeSplit*entrySize;
      extra->tmp = (uint8 *) HRSmalloc(extra->tmpSize, "");
    }

    m->compressInfo.extra = extra;

  }

  if (keyComplete) {
    m->adjKey = adjKeyDesc(m, keyDesc);
    initBlock(m, entrySize);
  }

  if (defComplete)
    initDefault(m, entryDefault, entrySize);

  m->complete = keyComplete && defComplete;
 
  if (fileexists) {
    m->selfIDSize = readSelfID(m);
    /*    readIndex(m); */
    /* delay reading the index until we know 
       that we need it. */
    m->index.initialized = 0;
    m->index.updated = 0;
    m->index.index = NULL;
    m->index.scV = NULL;
  }
  else if (m->complete == 0) {
    m->index.initialized = 0;
    m->index.updated = 0;
    m->index.index = NULL;
    m->index.scV = NULL;
  }
  else {
    assert(m->fi.mode != READONLY);
    m->selfIDSize = writeSelfID(m);
    newIndex(m);
    readFreeList(m);
  }

  if (((m->type == LL_SPLIT_MAP) || (m->type == STRIPED_MAP)) && 
      (m->def.type == CONSTANTDEFAULT))
    m->updateSelfID = 1;
  else
    m->updateSelfID = 0;


  /*
    if ((m->complete) && (m->fi.mode!=READONLY))
    readFreeList(m); */

  if (m->complete) {
    m->uncompressedStripeInfo.data = 
      (uint8 *) HRSmalloc(m->block.stripeSize, fname);
    m->uncompressedStripeInfo.cdata = 
      (uint8 *) HRSmalloc(m->block.stripeSize*m->cf, fname);
    m->uncompressedStripeInfo.updated = 0;
    m->uncompressedStripeInfo.bn = HRS_INVALID;
    m->uncompressedStripeInfo.stripe = HRS_INVALID;
    m->uncompressedStripeInfo.bv = NULL;

    /* Sets the default access pattern and the default number 
       of blocks to be freed when running out of memory */
    m->accessOrder=OFFDIR;

    /*    if ((m->index.numEntries*m->block.maxBlockSizeinFile) <NUM_BYTES_2B_FREED_OFFDIRDEFAULT)
      m->num_bytes_2B_freed=HRS_FREEALL;
    else
      m->num_bytes_2B_freed=NUM_BYTES_2B_FREED_OFFDIRDEFAULT;
    */
    m->num_bytes_2B_freed=NUM_BYTES_2B_FREED_OFFDIRDEFAULT;

    /* start with an unbuffered stream */
    setFileBufferSize(m, 0);
  }


  m->isAccessOrderOffdir=0;
  m->numAccesses=0;
  m->lastaccessedblock = HRS_INVALID;

  m->backup = NULL;
  m->copyto = NULL;

  /* add map to the list of top-level data structures */
  /* if necessary                                     */
  if (toplevel)
    HRSaddPDS(MAP, m);

  /* add map to list of maps */
  m->next = headMapList;
  headMapList = m;
  return m;
}

/* complete a NEW map using information from an old map */
void completeLLMap(HRSmap_t new, HRSmap_t old)
{
  int32 errLen = 50+strlen(new->fi.name)+1;
  char *fname = "completeLLMap";
  char keySet = 0;
  HRSmapCompress_s ci = new->compressInfo;
  char stripeSplitSet = 0;


  /* old map must be complete and an LL map */
  assert(old->complete);
  assert(old->type == LL_SPLIT_MAP);
  assert(old->origKey.levels == 1);
  
  assert(new->type == LL_SPLIT_MAP);

  assert(new->origKey.levels == 1);

  if (new->origKey.ranges[0].low < 0) {
    new->origKey.ranges[0].low = old->origKey.ranges[0].low; 
    keySet = 1;
  }
  else if (new->origKey.ranges[0].low != old->origKey.ranges[0].low)
    HRSerrorExit1("%s: key ranges must match\n", new->fi.name);

  if (new->origKey.ranges[0].high < 0) {
    new->origKey.ranges[0].high = old->origKey.ranges[0].high; 
    keySet = 1;
  }
  else if (new->origKey.ranges[0].low != old->origKey.ranges[0].low)
    HRSerrorExit1("%s: key ranges must match\n", new->fi.name);

  if (new->blockSplit < 0) {
    new->blockSplit = old->blockSplit;
    new->blockP2 = isP2(new->blockSplit, &new->blockShift);
    keySet = 1;
  } else if (new->blockSplit != old->blockSplit)
    HRSerrorExit1("%s: block splits must match\n", new->fi.name);

  if (new->stripeSplit < 0) {
    new->stripeSplit = old->stripeSplit;
    new->stripeP2 = isP2(new->stripeSplit, &new->stripeShift);
    keySet = 1;
    stripeSplitSet = 1;
  } else if (new->stripeSplit != old->stripeSplit)
    HRSerrorExit1("%s: stripe splits must match\n", new->fi.name);

  if (keySet) {
    new->adjKey = adjKeyDesc(new, new->origKey);
    initBlock(new, old->block.entrySize);
  }

  if (stripeSplitSet) {
    if ((ci.type == HRS_GENERIC_COMPRESS) ||
	(ci.type == HRS_USER_STRIPE_COMPRESS)) {
      struct extraCompress_s *extra = ci.extra;

    /* enough space to hold one full stripe */
    extra->tmpSize = new->stripeSplit*new->block.entrySize;
    extra->tmp = (uint8 *) HRSmalloc(extra->tmpSize, "");
    new->compressInfo.extra = extra;
    }
  }


  if (new->def.type == MISSINGCONSTANTDEFAULT) {
    new->def.type = CONSTANTDEFAULT;
    new->def.constant = (void *) HRSmalloc(new->block.entrySize, fname);
    new->def.f = NULL;
    memcpy(new->def.constant, old->def.constant, new->block.entrySize);
  }

  new->selfIDSize = writeSelfID(new);
  newIndex(new);


  if (new->def.type == CONSTANTDEFAULT)
    new->updateSelfID = 1;
  else
    new->updateSelfID = 0;

  if (new->fi.mode!=READONLY)
    readFreeList(new);

  new->uncompressedStripeInfo.data = 
    (uint8 *) HRSmalloc(new->block.stripeSize, fname);
  new->uncompressedStripeInfo.cdata = 
    (uint8 *) HRSmalloc(new->block.stripeSize*new->cf, fname);
  new->uncompressedStripeInfo.updated = 0;
  new->uncompressedStripeInfo.bn = HRS_INVALID;
  new->uncompressedStripeInfo.stripe = HRS_INVALID;
  new->uncompressedStripeInfo.bv = NULL;

  /* Sets the default access pattern and the default number 
     of blocks to be freed when running out of memory */
  new->accessOrder=OFFDIR;
  if ((new->index.numEntries*new->block.maxBlockSizeinFile) <NUM_BYTES_2B_FREED_OFFDIRDEFAULT)
    new->num_bytes_2B_freed=HRS_FREEALL;
  else
    new->num_bytes_2B_freed=NUM_BYTES_2B_FREED_OFFDIRDEFAULT;

  /* start with an unbuffered stream */
  setFileBufferSize(new, 0);
  new->complete = 1;
}



/* Closes the in-memory representation of the maps in headMapList. */
void HRSmapDone()
{
  HRSmap_t m, tmp;

  m = headMapList;
  while (m != NULL) {
    COMPLETE_check(m);
    if (m->type != OLD_STRIPED_MAP) {
      assert(m->block.numStripeEntries!=HRS_INVALID);
      if (m->uncompressedStripeInfo.updated) {
	/* Setting m->lastaccessedblock tells flushSomeStripes 
	   that k.block is the current block so it doesn't flush 
	   it before we are done with it. This is needed since 
	   we can run out memory, and therefore start flushing 
	   stripeCaches, at any point in time. */
	m->lastaccessedblock=m->uncompressedStripeInfo.bn;

	if (m->index.scV[m->uncompressedStripeInfo.bn]==NULL)
	  createStripeCache(m,m->uncompressedStripeInfo.bn);
	flushStripe(m);
      }
      if (m->index.initialized)
	closeStripeCaches(m);
      if (m->index.updated) {
	assert(m->fi.mode!=READONLY);
	writeFreeList(m);
	/* writeIndex(m) destroys m->index.index */
	writeIndex(m);
      }
      HRSfree(m->uncompressedStripeInfo.data);
      HRSfree(m->uncompressedStripeInfo.cdata);
    }

    HRSclosefile(m->fi.file);
    HRSfree(m->fi.name);
    if (m->index.initialized) {
      HRSfree(m->index.index);
      HRSfree(m->index.scV);
    }
    HRSfree(m->block.tmpSpace);
    HRSfree(m->newStripeLocations);

    tmp = m;
    m = m->next;
    HRSfree(tmp);
  }
}

/* Closes the in-memory representation of a map. */
void HRSmapClose(HRSmap_t m)
{
  HRSmap_t tmp, prev;

  COMPLETE_check(m);
  if (m->type != OLD_STRIPED_MAP) {
    assert(m->block.numStripeEntries!=HRS_INVALID);
    if (m->uncompressedStripeInfo.updated) {
      /* Setting m->lastaccessedblock tells flushSomeStripes 
	 that k.block is the current block so it doesn't flush 
	 it before we are done with it. This is needed since 
	 we can run out memory, and therefore start flushing 
	 stripeCaches, at any point in time. */
      m->lastaccessedblock=m->uncompressedStripeInfo.bn;

      if (m->index.scV[m->uncompressedStripeInfo.bn]==NULL)
	createStripeCache(m,m->uncompressedStripeInfo.bn);
      flushStripe(m);
    }
    if (m->index.initialized)
      closeStripeCaches(m);
    if (m->index.updated) {
      assert(m->fi.mode!=READONLY);
      writeFreeList(m);
      /* writeIndex(m) destroys m->index.index */
      writeIndex(m);
    }
    HRSfree(m->uncompressedStripeInfo.data);
    HRSfree(m->uncompressedStripeInfo.cdata);
  }

  HRSclosefile(m->fi.file);
  HRSfree(m->fi.name);
  if (m->index.initialized) {
    HRSfree(m->index.index);
    HRSfree(m->index.scV);
  }
  HRSfree(m->block.tmpSpace);
  HRSfree(m->newStripeLocations);

  /* find iterm in map list, plus find the entry before it */
  prev = NULL;
  for (tmp = headMapList; ((tmp != NULL) && (tmp != m)); tmp = tmp->next)
    prev = tmp;
  
  if (prev == NULL)
    headMapList = m->next;
  else
    prev->next = m->next;

  HRSfree(m);
}


/* converts new, an empty SM map, into a CSIDBV map */
/* that has the same type as old. called only by HRSmapCopy */
static void cvtSMtoCSIDBV(HRSmap_t new, HRSmap_t old) {
  char *fname = "cvtSMtoCSIDBV";

  assert(new->fi.mode!=READONLY);
  assert(new->type == STRIPED_MAP);
  assert(old->type == COMPRESSED_SELF_ID_BV);

  matchKeyDesc(new->origKey, old->origKey);

  new->type=COMPRESSED_SELF_ID_BV;
  new->cf = MAX_COMPRESSION;

  /* re-adjust the key */
  new->adjKey = adjKeyDesc(new, new->origKey);

  HRSfree(new->uncompressedStripeInfo.cdata);
  HRSfree(new->newStripeLocations);
  HRSfree(new->block.tmpSpace);
  initBlock(new, new->block.entrySize);

  new->selfIDSize = writeSelfID(new);
  new->headerSize = old->headerSize;

  HRSfree(new->index.index);
  newIndex(new);

  HRSfree(new->fl.fl);
  readFreeList(new);
  
  HRSfree(new->uncompressedStripeInfo.data);
  new->uncompressedStripeInfo.data = 
    (uint8 *) HRSmalloc(new->block.stripeSize, fname);
  new->uncompressedStripeInfo.cdata = 
    (uint8 *) HRSmalloc(new->block.stripeSize*new->cf, fname);
  new->uncompressedStripeInfo.updated = 0;
  new->uncompressedStripeInfo.bn = HRS_INVALID;
  new->uncompressedStripeInfo.stripe = HRS_INVALID;
  new->uncompressedStripeInfo.bv = NULL;

  new->updateSelfID = 1;
}

/*
  Starts the lazy copy of old to new. Or in other words
  sets old to be the backup map of new. The actual copying is
  done in closeStripeCaches.
*/
void HRSmapCopy(HRSmap_t old, HRSmap_t new) {
  newList_t *temp;
  int i;
  char *fname = "HRSmapCopy";

  OSM_check(old);
  COMPLETE_check(old);
  OSM_check(new);

  if ((old != NULL) && (old->index.initialized == 0)) {
    readIndex(old);
    if (old->fi.mode != READONLY)
      readFreeList(old);
  }

  if (old == NULL)
    HRSerrorExit1("HRSmapCopy failed with old == NULL, new == %s\n", 
		  new->fi.name);
  else if (new == NULL)
    HRSerrorExit1("HRSmapCopy failed with old == %s, new == NULL\n", 
		  old->fi.name);
  else if (new->backup != NULL)
    HRSerrorExit1("HRSmapCopy: %s: Cannot have more than one backup per map\n", 
		  new->fi.name);
  else if ((new->complete == 1) && (!mapIsEmpty(new)))
    HRSerrorExit1("HRSmapCopy: Cannot copy map into a non-empty map (%s).\n", 
		  new->fi.name);
  else if (old->type == COMPRESSED_SELF_ID_BV) {
    matchKeyDesc(new->origKey, old->origKey);

    resetIndex(new);

    if (new->type == STRIPED_MAP)
      /* convert the new STRIPED map into a COMPRESSED_SELF_ID_BV map */
      cvtSMtoCSIDBV(new, old);
  }
  else if (new->complete == 0)
    completeLLMap(new, old);
  else {
    /* in striped case. match the adjusted keys */
    if ((new->adjKey.ranges[BLOCK].high != old->adjKey.ranges[BLOCK].high) ||
	(new->adjKey.ranges[STRIPE].high != old->adjKey.ranges[STRIPE].high) ||
	(new->adjKey.ranges[ENTRY].high != old->adjKey.ranges[ENTRY].high))
      HRSerrorExit1("HRSmapCopy: %s: Key types must match in map copy",
		    new->fi.name);    
    resetIndex(new);
  }

  new->backup = old;

  temp=(newList_t*) HRSmalloc(sizeof(newList_t), fname);
  temp->map=new;
  temp->next=old->copyto;
  old->copyto=temp;

  resetIndex(old);

  for (i=0; i < old->index.numEntries; i++) {
    assert(new->index.index[i].loc == HRS_INVALID);
    if (old->index.index[i].loc != HRS_INVALID) {
      assert(old->index.index[i].loc >= old->headerSize);
      new->index.index[i].loc = HRS_IN_BACKUPFILE;
    }
  }

  new->index.maxBSseen = HRS_INVALID;
}

/*
  Put the data associated with this line from this HRSmap_t
  into the data parameter
*/
void *HRSmapGet(HRSmap_t m, void *k, void *data) {
  HRS3Key_s adjK3;
  int32 eSize;

  OSM_check(m);
  COMPLETE_check(m);

  if (m->index.initialized == 0) {
    readIndex(m);
    if (m->fi.mode != READONLY)
      readFreeList(m);

  }

  adjKey(m, k, &adjK3);

  eSize=m->block.entrySize;
  if (keyExists(m, adjK3)) {
    unpackStripe(m, adjK3.block, adjK3.stripe);
    assert(m->uncompressedStripeInfo.data!=NULL);
    assert(m->uncompressedStripeInfo.stripe==adjK3.stripe);
    assert(m->uncompressedStripeInfo.bn==adjK3.block);
    memcpy(data,m->uncompressedStripeInfo.data+eSize*adjK3.entry, eSize);
  } else {
    /* use default value */
    if (m->def.type == CONSTANTDEFAULT) 
      memcpy(data, m->def.constant, eSize);
    else if (m->def.type == PARAMFUNCTIONDEFAULT) {
      if ( ((HRSmapFunLLParam_t)m->def.f)(m->set, m->pt, *(HRSLLKey_t *)k, data) != HRS_OK) {
	char *tmp;
        int s = strlen(m->fi.name) + 100;
	
	tmp = (char *)HRSmalloc(s, "HRSmapGet");
	sfsprintf(tmp, s, "%s: Default function failed on key %lld\n", m->fi.name, 
		  *(HRSLLKey_t *)k);
	HRSerrorExit(tmp);
      }
    }
    else if (LLKey(m)) {
      if (((HRSmapFunLL_t)m->def.f)(*(HRSLLKey_t *)k, data) != HRS_OK) {
	char *tmp;
        int s = strlen(m->fi.name) + 100;
	
	tmp = (char *)HRSmalloc(s, "HRSmapGet");
	sfsprintf(tmp, s, "%s: Default function failed on key %lld\n", m->fi.name, 
		  *(HRSLLKey_t *)k);
	HRSerrorExit(tmp);
      }
    }
    else if (L2Key(m)) {
      if (((HRSmapFun2K_t)m->def.f)(*(HRSkey_s *)k, data) != HRS_OK) {
	char *tmp;
        int s = strlen(m->fi.name) + 100;
	
	tmp = (char *)HRSmalloc(s, "HRSmapGet");
	sfsprintf(tmp, s, "%s: Default function failed on key %d %d\n", m->fi.name, ((HRSkey_s *)k)->block, ((HRSkey_s *)k)->line);
	HRSerrorExit(tmp);
      }
    }
    else {
      if (((HRSmapFun3K_t)m->def.f)(*(HRS3Key_s *)k, data) != HRS_OK ) {
	char *tmp;
        int s = strlen(m->fi.name) + 100;
	
	tmp = (char *)HRSmalloc(s, "HRSmapGet");
	sfsprintf(tmp, s, "%s: Default function failed on key %%d %d %d\n", 
		  m->fi.name, 
		  ((HRS3Key_s *)k)->block,
		  ((HRS3Key_s *)k)->stripe,
		  ((HRS3Key_s *)k)->entry);

	HRSerrorExit(tmp);
      }
    }

  }
  return data;
}


void copyoldtonew(HRSmap_t m, int32 bn, int32 stripe) {
  newList_t *temp=m->copyto;

  while (temp!=NULL) {
    if (temp->map->index.index[bn].loc==HRS_IN_BACKUPFILE)
      findCompressedStripe(temp->map, bn, stripe);
    temp=temp->next;
  }
}


/* Put the current line */
void HRSmapPut(HRSmap_t m, void *k, void *data) {
  int32 eSize;
  HRS3Key_s adjK3;

  OSM_check(m);
  COMPLETE_check(m);

  assert(m != NULL);
  assert(m->fi.mode!=READONLY);

  if (m->index.initialized == 0) {
    readIndex(m);
    if (m->fi.mode != READONLY)
      readFreeList(m);
  }

  adjKey(m, k, &adjK3);
  eSize=m->block.entrySize;

  if (m->copyto!=NULL)
    copyoldtonew(m,adjK3.block, adjK3.stripe);

  if (m->updateSelfID) {
    int rv = writeEntryToSelfID(m, data, val2Loc(m), SYNC, 0);
    if (rv != HRS_REMOVEENTRY)
      m->updateSelfID=0;
  }

  findCompressedStripe(m, adjK3.block, adjK3.stripe);
  unpackStripe(m, adjK3.block, adjK3.stripe);

  assert(m->uncompressedStripeInfo.bn==adjK3.block);
  assert(m->uncompressedStripeInfo.stripe==adjK3.stripe);
  assert(m->uncompressedStripeInfo.bv!=NULL);

  memcpy(m->uncompressedStripeInfo.data+adjK3.entry*eSize, data, eSize);
  m->uncompressedStripeInfo.updated=1;
  set_bv(m, m->uncompressedStripeInfo.bv, adjK3.entry);
}


/* Checks in m->file and m->backup->file to see if block exists */
int32 HRSmapBlockExists(HRSmap_t m, int32 block) {

  OSM_check(m);
  assert(m->type != LL_SPLIT_MAP);

  if (m->index.initialized == 0) {
    readIndex(m);
    if (m->fi.mode != READONLY)
      readFreeList(m);
  }

  if (blockIsEmpty(m, adjBlock(m, block)))
    return 0;
  else 
    return 1;
}


int32 HRSmapQuery(HRSmap_t m, void *k) {
  HRS3Key_s adjK3;

  OSM_check(m);
  COMPLETE_check(m);

  if (m->index.initialized == 0) {
    readIndex(m);
    if (m->fi.mode != READONLY)
      readFreeList(m);
  }

  adjKey(m, k, &adjK3);
  return keyExists(m, adjK3);

}

int32 HRSmapTestKey(HRSmap_t m, void *k) {
  OSM_check(m);
  COMPLETE_check(m);

  if (LLKey(m)) {
    HRSLLKey_t llk = *((HRSLLKey_t *) k);

    if ((llk < m->origKey.ranges[FULLKEY].low) ||
	(llk > m->origKey.ranges[FULLKEY].high))
      return 0;
    else return 1;
  }
  else if (L2Key(m)) {
    HRSkey_s k2 = *((HRSkey_s *) k);

    if ((k2.block < m->origKey.ranges[BLOCK].low) ||
	(k2.block < m->origKey.ranges[BLOCK].high))
      return 0;
    else if ((k2.line < m->origKey.ranges[LINE].low) ||
	     (k2.line < m->origKey.ranges[LINE].high))
      return 0;
    else 
      return 1;
  }
  else { 
    HRS3Key_s k3 = *((HRS3Key_s *) k);

    if ((k3.block < m->origKey.ranges[BLOCK].low) ||
	(k3.block < m->origKey.ranges[BLOCK].high))
      return 0;
    else if ((k3.stripe < m->origKey.ranges[STRIPE].low) ||
	     (k3.stripe < m->origKey.ranges[STRIPE].high))
      return 0;
    else if ((k3.entry < m->origKey.ranges[ENTRY].low) ||
	     (k3.entry < m->origKey.ranges[ENTRY].high))
      return 0;
    else 
      return 1;
  }
}

void HRSmapRemove(HRSmap_t m, void *k) {
  HRS3Key_s adjK3;

  OSM_check(m);
  COMPLETE_check(m);

  if (m->index.initialized == 0) {
    readIndex(m);
    if (m->fi.mode != READONLY)
      readFreeList(m);
  }

  adjKey(m, k, &adjK3);

  if (m->copyto!=NULL)
    copyoldtonew(m, adjK3.block, adjK3.stripe);

  if (keyExists(m, adjK3)) {
    unpackStripe(m, adjK3.block, adjK3.stripe);
    unset_bv(m, m->uncompressedStripeInfo.bv, adjK3.entry);
    assert(test_bv(m, m->uncompressedStripeInfo.bv, adjK3.entry) == 0);
    m->uncompressedStripeInfo.updated=1;
  }
}

void HRSmapRanges(HRSmap_t m, long long *low, long long *high) {
  assert(LLKey(m));

  *low = m->origKey.ranges[FULLKEY].low;
  *high = m->origKey.ranges[FULLKEY].high;
}


/*******************************************************
**                                                    **
** Functions to convert between various map types.    **
**                                                    **
*******************************************************/

/* converts an old-style (OLD_STRIPED_MAP) block into a
   new style (STRIPED_MAP) block.

   OLD_STRIPED_MAP blocks in file:
      bitvector + stripe location vector + (compressed stripes)*
*/

#define OLDBVBASE 32
static void cvtOSMBlocktoSM(HRSmap_t old, HRSmap_t new, int bn)
{
  int32 oldBlockSize;
  loc_t oldBlockLoc;
  int32 newBlockSize;
  loc_t newBlockLoc;
  int32 *oldBV;
  bv_t newBV;
  uint8 *oldData;
  stripeSize_t *oldSL;
  stripeSize_t *newSL;
  uint8 *newStripes;
  uint8 *newBlock;
  uint8 *oldBlock;
  uint8 *currentStripe;
  stripeSize_t ss;
  int s;
  int l;
  int oldNumEntries, oldBVSize;
  char *fname = "cvtOSMBlocktoSM";

  oldBlockLoc = old->index.index[bn].loc;
  if (oldBlockLoc == HRS_INVALID)
    return;
  assert(oldBlockLoc >= old->headerSize);

  oldBlockSize = old->index.index[bn].size;  
  assert(oldBlockSize >= 0);

  oldBlock = (uint8 *) HRSmalloc(oldBlockSize, fname);
  oldBV = (int32 *) oldBlock;
  assert(L2Key(old));
  oldNumEntries = old->origKey.ranges[LINE].high - old->origKey.ranges[LINE].low + 1;
  oldBVSize = bytealign(oldNumEntries/BVBYTES8);
  oldSL = (stripeSize_t *)(oldBlock + oldBVSize);
  oldData = (uint8 *)oldSL + old->block.SLSize;

  readArrayOfInts(old->fi, (uint8 *)oldBV, oldBlockLoc, oldBVSize);
  readArrayOfInts(old->fi, (uint8 *)oldSL, oldBlockLoc+oldBVSize, old->block.SLSize);
  HRSreadBytes(old->fi, oldData, 
	       oldBlockLoc+oldBVSize+old->block.SLSize, 
	       oldBlockSize-oldBVSize-old->block.SLSize);

  assert(new->index.index[bn].loc == HRS_INVALID);
  assert(new->index.scV[bn] == NULL);

  newBlock = (uint8 *) HRSmalloc(old->cf*oldBlockSize, fname);
  newSL = (stripeSize_t *)newBlock;
  newSL[0] = new->block.SLSize;
  newStripes = newBlock + new->block.SLSize;
  currentStripe = newStripes;
  newBlockSize = new->block.SLSize;

  for (s = 0; s < old->block.numStripes-1; s++) {
    ss = oldSL[s+1]-oldSL[s];

    if (ss > 0) {
      newBV = currentStripe;
      bzero(newBV, new->block.BVSize);
      /* copy bits */
      for (l=s*old->block.numStripeEntries; l < (s+1)*old->block.numStripeEntries; l++) {
	
	if ((oldBV[l/OLDBVBASE] & (0x1 << l%OLDBVBASE)) != 0) {
	  int e = l%new->block.numStripeEntries;
	  newBV[e/BVBASE] = newBV[e/BVBASE] | (0x1 << e%BVBASE);
	}
      }
      currentStripe = currentStripe+new->block.BVSize;
      memcpy(currentStripe, oldData+oldSL[s], ss);
      currentStripe += ss;
      newSL[s+1] = newSL[s]+new->block.BVSize + ss;
      newBlockSize = newBlockSize + new->block.BVSize + ss;
    } else 
      newSL[s+1] = newSL[s];
  }

  /* last stripe */
  ss = oldSL[s+1]-oldSL[s];

  if (ss > 0) {
    newBV = currentStripe;
    bzero(newBV, new->block.BVSize);
    /* copy bits */
    for (l=s*old->block.numStripeEntries; l < oldNumEntries; l++)
      if ((oldBV[l/OLDBVBASE] & (0x1 << l%OLDBVBASE)) != 0) {
	int e = l%new->block.numStripeEntries;
	newBV[e/BVBASE] = newBV[e/BVBASE] | (0x1 << e%BVBASE);
      }
    currentStripe = currentStripe+new->block.BVSize;
    memcpy(currentStripe, oldData+oldSL[s], ss);
    currentStripe += ss;
    newSL[s+1] = newSL[s]+new->block.BVSize + ss;
    newBlockSize = newBlockSize + new->block.BVSize + ss;
  } else 
    newSL[s+1] = newSL[s];

  assert(newBlockSize > new->block.SLSize);

  newBlockLoc = allocFileSpace(new, newBlockSize);
  new->index.index[bn].loc = newBlockLoc;
  new->index.index[bn].size = newBlockSize;
  new->index.updated = 1;

  portSL(new, newSL, newSL);

  HRSwriteBytes(new->fi, newBlock, newBlockLoc, newBlockSize, SYNC);
      
  HRSfree(newBlock);
  HRSfree(oldBlock);
}


/* converts COMPRESSED_SELF_ID_MAPS to STRIPED_MAPS.
   Used largely for testing purposes (to get our
   hands on large STRIPED_MAPS.  But a map that
   will be used for random access would probably
   benefit from being converted. 
*/
  
#define min(a, b) ((a) < (b) ? (a) : (b))

static void cvtCSIDBVBlocktoSM(HRSmap_t old, HRSmap_t new, int bn)
{
  int32 oldBlockSize;
  loc_t oldBlockLoc;
  int32 newBlockSize;
  loc_t newBlockLoc;
  int32 *oldBV;
  bv_t newBV;
  uint8 *oldData;
  stripeSize_t *newSL;
  uint8 *newStripes;
  uint8 *newBlock;
  uint8 *oldBlock;
  uint8 *currentStripe;
  int s;
  int l;
  int oldNumEntries, oldBVSize;
  uint8 *ignoreVal;
  int32 oldBytesLeft;
  uint8 *newData;
  char *fname = "cvtCSIDBVBlocktoSM";
  char fullSet;
  int32 bvExtraBytes;

  oldBlockLoc = old->index.index[bn].loc;
  if (oldBlockLoc == HRS_INVALID)
    return;
  assert(oldBlockLoc >= old->headerSize);

  oldBlockSize = old->index.index[bn].size;  
  assert(oldBlockSize >= 0);

  fullSet = ((old->type == COMPRESSED_SELF_ID_BV) &&
	     (old->index.fbV[bn] == 1)) ? 1 : 0;
  bvExtraBytes = (fullSet) ? old->block.BVSize : 0;

  oldBlock = (uint8 *) HRSmalloc(oldBlockSize+bvExtraBytes, fname);

  if (fullSet) {
    memset(oldBlock, 0xffff, bvExtraBytes);
  }

  HRSreadBytes(old->fi, oldBlock+bvExtraBytes, oldBlockLoc, oldBlockSize);

  oldBV = (int32 *) oldBlock;
  assert(L2Key(old));
  oldNumEntries = 
    old->origKey.ranges[LINE].high - old->origKey.ranges[LINE].low + 1;

  oldBVSize = ((oldNumEntries%BVBASE8 == 0)
		     ? oldNumEntries/BVBASE8 * BVBYTES8
		     : (oldNumEntries/BVBASE8 + 1) * BVBYTES8);

  oldData = oldBlock + oldBVSize;
  oldBytesLeft = oldBlockSize + bvExtraBytes - oldBVSize;

  assert(new->index.index[bn].loc == HRS_INVALID);
  assert(new->index.scV[bn] == NULL);

  newBlock = (uint8 *) HRSmalloc(new->block.maxBlockSizeinFile, fname);
  newSL = (stripeSize_t *)newBlock;
  newSL[0] = new->block.SLSize;
  newStripes = newBlock + new->block.SLSize;
  currentStripe = newStripes;
  newBlockSize = new->block.SLSize;

  ignoreVal = (uint8 *) HRSmalloc(old->block.entrySize, fname);

  for (s = 0; s < new->block.numStripes; s++) {
    int stripeHasData = 0;
    for (l=s*new->block.numStripeEntries; 
	 l < min(oldNumEntries, (s+1)*new->block.numStripeEntries); 
	 l++) {
	
      if ((oldBV[l/OLDBVBASE] & (0x1 << l%OLDBVBASE)) != 0)
	stripeHasData = 1;
    }

    if (stripeHasData) {
      int newStripeSize;

      newBV = currentStripe;
      newData = currentStripe + new->block.BVSize;
      bzero(newBV, new->block.BVSize);
      newStripeSize = new->block.BVSize;

      /* copy bits/data */
      for (l=s*new->block.numStripeEntries; 
	   l < min(oldNumEntries, (s+1)*new->block.numStripeEntries); 
	   l++) {
	
	if ((oldBV[l/OLDBVBASE] & (0x1 << l%OLDBVBASE)) != 0) {
	  int e = l%new->block.numStripeEntries;
	  int es;
	  HRSentryDecompressFn_t olducrWrapper = 
	    (HRSentryDecompressFn_t) old->compressInfo.df;


	  newBV[e/BVBASE] = newBV[e/BVBASE] | (0x1 << e%BVBASE);

	  es = olducrWrapper(old->set, old->pt, oldData, oldBytesLeft, 
			       ignoreVal);
	  memcpy(newData, oldData, es);
	  newData = newData + es;
	  oldData = oldData + es;
	  oldBytesLeft = oldBytesLeft - es;
	  newStripeSize += es;
	}
      }
      currentStripe = currentStripe + newStripeSize;
      newSL[s+1] = newSL[s]+ newStripeSize;
      newBlockSize = newBlockSize + newStripeSize;
    } else 
      newSL[s+1] = newSL[s];
  }

  assert(newBlockSize > new->block.SLSize);

  newBlockLoc = allocFileSpace(new, newBlockSize);
  new->index.index[bn].loc = newBlockLoc;
  new->index.index[bn].size = newBlockSize;
  new->index.updated = 1;

  portSL(new, newSL, newSL);
  HRSwriteBytes(new->fi, newBlock, newBlockLoc, newBlockSize, SYNC);
      
  HRSfree(newBlock);
  HRSfree(oldBlock);
  HRSfree(ignoreVal);
}


/* converts OLD_STRIPED maps into STRIPED_MAPs
   or COMPRESSED_SELF_ID_BV maps into STRIPED_MAPs.

   new map MUST be empty.
 */
void HRScvtOldMaptoNew(HRSmap_t old, HRSmap_t new) 
{
  int i;

  assert(mapIsEmpty(new) != 0);
  assert(new->type == STRIPED_MAP);

  if (old->type == OLD_STRIPED_MAP) {
    matchKeyDesc(new->origKey, old->origKey);

    for (i=0; i < new->index.numEntries; i++) {
      sfprintf(sfstdout, "%06d\n", i);
      cvtOSMBlocktoSM(old, new, i);
    }

  } else {
    assert(old->type == COMPRESSED_SELF_ID_BV);

    for (i=0; i < new->index.numEntries; i++) {
      sfprintf(sfstdout, "%06d\n", i);
      cvtCSIDBVBlocktoSM(old, new, i);
    }
  }
}


/*******************************************************
**                                                    **
** Map iterations functions.                          **
**   Call HRSnextEntryInit once to set up iteration   **
**   Call HRSgetNextExistingEntry to get the next     **
**     key in the sequence.  Returns a 0 to signal    **
**     the end of the key stream.                     **
**                                                    **
*******************************************************/

HRSnextEntry_t HRSnextEntryInit(HRSiterate_s mapDescr) {
  HRSnextEntry_t handle;
  char *fname = "HRSnextEntryInit";
  int32 nse = mapDescr.it_map->block.numStripeEntries;

  handle=
    (HRSnextEntry_t) HRSmalloc(sizeof(HRSnextEntry_s), fname);

  OSM_check(mapDescr.it_map);
  COMPLETE_check(mapDescr.it_map);

  if (mapDescr.it_map->index.initialized == 0) {
    readIndex(mapDescr.it_map);
    if (mapDescr.it_map->fi.mode != READONLY) 
      readFreeList(mapDescr.it_map);
  }

  handle->themap=mapDescr.it_map;
  /* make the stream buffered */
  if (handle->themap->fi.buffered == 0)
    setFileBufferSize(handle->themap, 1);

  adjKey(mapDescr.it_map, mapDescr.low, &handle->next);
  adjKey(mapDescr.it_map, mapDescr.high, &handle->high);

  if (LLKey(handle->themap)) {
    handle->resultKey = HRSmalloc(sizeof(HRSLLKey_t)*nse, "HRSnextEntryInit");
    handle->resultK3 = HRSmalloc(sizeof(HRS3Key_s)*nse, "HRSnextEntryInit");
  }
  else if (L2Key(handle->themap))
    handle->resultKey = HRSmalloc(sizeof(HRSkey_s)*nse, "HRSnextEntryInit");
  else 
    handle->resultKey = HRSmalloc(sizeof(HRS3Key_s)*nse, "HRSnextEntryInit");

  handle->nextSlot = 0;
  handle->numSlots = -1;

  handle->themap->accessOrder = ONDIR;

  return handle;
}

void HRSnextEntryDone(HRSnextEntry_t handle) {
  HRSfree(handle->resultKey);
  HRSfree(handle);
}


/* returns the number of entries stored in the result vector */
/* zero means that there are no more keys left in the range */
int fillResultVector(HRSnextEntry_t handle) {
  HRS3Key_s curr=handle->next;
  HRS3Key_s ub=handle->high;
  HRSmap_t m=handle->themap;
  int entryHigh = m->adjKey.ranges[ENTRY].high;
  int stripeHigh = m->adjKey.ranges[STRIPE].high;
  
  if ((curr.block > ub.block))
    return 0;
  if ((curr.block == ub.block) && (curr.stripe > ub.stripe))
    return 0;
  if ((curr.block == ub.block) && (curr.stripe == ub.stripe)
      && (curr.entry > ub.entry))
    return 0;

  while (curr.block<=ub.block) {
    if ((m->index.index[curr.block].loc != HRS_INVALID) ||
	(m->index.scV[curr.block] != NULL)) {
      int32 tmpStripeHigh;

      if (curr.block == ub.block)
	tmpStripeHigh = ub.stripe;
      else
	tmpStripeHigh = stripeHigh;

      while (curr.stripe <= tmpStripeHigh) {
	stripeCache_t *cSC;
	bv_t bv;

	cSC = findCompressedStripe(m, curr.block, curr.stripe);
	bv = cSC->stripesInMem[curr.stripe].bv; 
	if (bv != NULL) {
	  int32 tmpEntryHigh;
	  int32 nextSlot = 0;

	  if ((curr.block == ub.block) &&
	      (curr.stripe == ub.stripe))
	    tmpEntryHigh = ub.entry;
	  else
	    tmpEntryHigh = entryHigh;

	  while (curr.entry <= tmpEntryHigh) {
	    if (test_bv(m, bv, curr.entry)) {
	      /* generate key/add to vector */
              if (LLKey(m)) {
		HRSLLKey_t *llk = &(((HRSLLKey_t *) handle->resultKey)[nextSlot]);
		*llk = (((long long) curr.block*m->blockSplit) 
			+ ((long long) curr.stripe*m->stripeSplit) +
			(long long) curr.entry) + m->origKey.ranges[FULLKEY].low;
		handle->resultK3[nextSlot] = curr;
              }
	      else if (L2Key(m)) {
		HRSkey_s *k2 = &(((HRSkey_s *)handle->resultKey)[nextSlot]);

		k2->block = curr.block + m->origKey.ranges[BLOCK].low;
		k2->line = (curr.stripe*m->block.numStripeEntries+curr.entry) 
		  + m->origKey.ranges[LINE].low;
	      } else {
		HRS3Key_s *k3 = &(((HRS3Key_s *)handle->resultKey)[nextSlot]);

		k3->block = curr.block + m->origKey.ranges[BLOCK].low;
		k3->stripe = curr.stripe + m->origKey.ranges[STRIPE].low;
		k3->entry = curr.entry + m->origKey.ranges[ENTRY].low;
	      }
	      nextSlot++;
	    }
	    curr.entry++;
	  }
	  if (curr.stripe < stripeHigh) {
	    handle->next.block = curr.block;
	    handle->next.stripe = curr.stripe+1;
	    handle->next.entry = 0;
	  } else {
	    handle->next.block=curr.block+1;
	    handle->next.stripe=0;
	    handle->next.entry=0;
	  }

	  if (nextSlot > 0)
	    return nextSlot;
	}
	curr.stripe++; curr.entry = 0;
      }
    }
    curr.block++; curr.stripe = 0; curr.entry = 0;
  }      
  return 0;
}

void *HRSgetNextExistingEntry(HRSnextEntry_t handle) {
  HRSmap_t m=handle->themap;
  
  if (handle->nextSlot >= handle->numSlots) {
    if ((handle->numSlots = fillResultVector(handle)) == 0)
      return NULL;
    else /* reset back to the beginning of the vector */
      handle->nextSlot = 0;
  }
  
  if (LLKey(m)) {
    HRSLLKey_t *llk = &(((HRSLLKey_t *) handle->resultKey)[handle->nextSlot]);

    /* cache the conversion between two views of the key for later use */
    handle->themap->lastSeen = *llk;
    handle->themap->lastSeen3 = handle->resultK3[handle->nextSlot];

    handle->nextSlot++;
    return llk;
  }
  else if (L2Key(m)) {
    HRSkey_s *k2 = &(((HRSkey_s *)handle->resultKey)[handle->nextSlot]);
    handle->nextSlot++;
    return k2;
  } else {
    HRS3Key_s *k3 = &(((HRS3Key_s *)handle->resultKey)[handle->nextSlot]);
    handle->nextSlot++;
    return k3;
  }
}


/** AUXILLIARY CODE -- HRSmapMakeSpace (near end) is
    called by HRSmalloc (in util.c) to retrieve some
    space from the map cache.
**/


static void setFileBufferSize(HRSmap_t m, int buffered)
{
  if (buffered == 0) {
    sfsetbuf(m->fi.file, NULL, 0);
    m->fi.buffered = 0;
  }
  else if (m->index.maxBSseen <= 0) {
    sfsetbuf(m->fi.file, NULL, 1024);
    m->fi.buffered = 1;
  }
  else {
    sfsetbuf(m->fi.file, NULL, m->index.maxBSseen);
    m->fi.buffered = 1;
  }
}


/* Sets the access order and the granularity of freeing */
static void setAccessOrder(HRSmap_t m) {
  char *temp;
  char *envVar;
  int32 numBytes;

  /* use default to start */
  if (m->isAccessOrderOffdir) {
    if (m->accessOrder != OFFDIR)
      setFileBufferSize(m, 0);
    m->accessOrder=OFFDIR;
    numBytes = NUM_BYTES_2B_FREED_OFFDIRDEFAULT;
    envVar="HRS_BYTES2BFREEDOFFDIR";
  } else {
    if (m->accessOrder != ONDIR)
      setFileBufferSize(m, 1);
    m->accessOrder=ONDIR;
    numBytes = NUM_BYTES_2B_FREED_ONDIRDEFAULT;
    envVar="HRS_BYTES2BFREEDONDIR";
  }

  /* use user-specified value, if available */
  temp=getenv(envVar);

  if (temp==NULL)
    temp=getenv("HRS_BYTES2BFREED");

  if (temp!=NULL) {
    int32 value = atoi(temp);
    if (((value>0)&&(value<=m->index.numEntries))||(value==HRS_FREEALL))
      numBytes = value;
  }

  /* if current value is too big, use HRS_FREEALL instead */
  if (numBytes > m->index.numEntries*m->block.maxBlockSizeinFile)
    numBytes = HRS_FREEALL;

  assert((numBytes > 0) || (numBytes == HRS_FREEALL));
  m->num_bytes_2B_freed = numBytes;
}

/* Sets the maximum memory usage of the current process */
static void setMemoryUsageLimit() {
  int32 value;
  char *temp;
  struct rlimit rlp;
  int32 errval;
  char *fname = "setMemoryUsageLimit";

  temp=getenv("HRS_MEMUSAGE");

  if (temp!=NULL) {
    value=atoi(temp);
    errval= getrlimit(RLIMIT_VMEM, &rlp);
    if (errval!=0) {
      int32 errLen = 150;
      char *errStr = (char *) HRSmalloc(errLen, fname);

      sfsprintf(errStr, errLen, "setMemoryUsage: getrlimit returned errorcode %d and errno=%s.\n", errval, strerror(errno));
      HRSerrorExit(errStr);
    }
    if (value>=MIN_MEMUSAGE)
      rlp.rlim_max = (rlim_t ) value;
    else
      rlp.rlim_max = (rlim_t ) MIN_MEMUSAGE;
    if (rlp.rlim_cur>rlp.rlim_max)
      rlp.rlim_cur=rlp.rlim_max;
    errval=setrlimit(RLIMIT_VMEM, &rlp);
    if (errval!=0) {
      int32 errLen = 150;
      char *errStr = (char *) HRSmalloc(errLen, fname);

      sfsprintf(errStr, errLen, "setMemoryUsage: setrlimit returned errorcode %d and errno=%s when setting virtual memory limit to %d\n", errval, strerror(errno),(uint32)rlp.rlim_max);
      HRSerrorExit(errStr);
    }
  } 

}

int32 flushStripeRange(HRSmap_t m, int32 low, int32 high)
{
  int i;
  stripeSize_t freedBytes = 0;
  
  /* flush from low to high-1 */
  for (i=low; i < high ;i++)
    if (m->index.scV[i]!=NULL)
      freedBytes+=flushNfreeStripeCache(m,i);
  return freedBytes;
}


/* if all is true, flush'em all.  otherwise, flush just
   up to m->lastaccessedblock */

int32 flushStripesInMap(HRSmap_t m, int all) {
  stripeSize_t freedBytes=0;
  int32 nono = m->uncompressedStripeInfo.bn;
  int32 bn = m->lastaccessedblock;

  if (nono==bn) {
    freedBytes += flushStripeRange(m, 0, bn);
    if (all)
      freedBytes += flushStripeRange(m, bn+1, m->index.numEntries);
  } else if (nono < bn) {
    freedBytes += flushStripeRange(m, 0, nono);
    freedBytes += flushStripeRange(m, nono+1, bn);
    if (all)
      freedBytes += flushStripeRange(m, bn+1, m->index.numEntries);
  } else {
    freedBytes += flushStripeRange(m, 0, bn);
    if (all) {
      freedBytes += flushStripeRange(m, bn+1, nono);
      freedBytes += flushStripeRange(m, nono+1, m->index.numEntries);
    }
  }

  return freedBytes;
}


/*
  Flushes and frees all stripeCaches.
  It is called from HRSmapMakeSpace
*/
/* flush from low to high-1 */
int32 flushStripeRangeBounded(HRSmap_t m, int32 low, int32 high,
				   stripeSize_t bytesWanted)
{
  int i;
  stripeSize_t freedBytes = 0;
  
  i = low;
  while ((i < high) && (freedBytes < bytesWanted)) {
    if (m->index.scV[i]!=NULL)
      freedBytes+=flushNfreeStripeCache(m,i);
    i++;
  }

  return freedBytes;
}

/*
  Flushes and frees some stripeCaches.
  It is called from HRSmapMakeSpace
  and which stripeCaches it throws out should 
  depend on the access pattern of that map. 
*/
void flushSomeStripesOFFDIR(HRSmap_t m) {
  stripeSize_t bytesLeft;
  int32 nono;
  int32 bn;

  if ((m->accessOrder==OFFDIR)&&(m->numAccesses!=0)) {
    bytesLeft=m->num_bytes_2B_freed;
    nono=m->uncompressedStripeInfo.bn;
    bn=m->lastaccessedblock;
  
    if (nono==bn) {
      bytesLeft -= flushStripeRangeBounded(m, 0, bn, bytesLeft);
      if (bytesLeft > 0)
	bytesLeft -= flushStripeRangeBounded(m, bn+1, m->index.numEntries,
					     bytesLeft);
    } else if (nono<bn) {
      bytesLeft -= flushStripeRangeBounded(m, 0, nono, bytesLeft);
      if (bytesLeft > 0)
	bytesLeft -= flushStripeRangeBounded(m, nono+1, bn, bytesLeft);
      if (bytesLeft > 0)
	bytesLeft -= flushStripeRangeBounded(m, bn+1, m->index.numEntries,
					     bytesLeft);
    } else {
      bytesLeft -= flushStripeRangeBounded(m, 0, bn, bytesLeft);
      if (bytesLeft > 0)
	bytesLeft -= flushStripeRangeBounded(m, bn+1, nono, bytesLeft);
      if (bytesLeft > 0)
	bytesLeft -= flushStripeRangeBounded(m, nono+1, m->index.numEntries,
					     bytesLeft);
    }
  } 
  
  return;
}


/* Called by HRSmalloc to retrieve space from
   the stripe cache.
*/

void HRSmapMakeSpace(int16 urgent) {
  HRSmap_t m=headMapList;
  while ((m!=NULL) && (m->numAccesses!=0)) {
    if ((urgent) || (m->num_bytes_2B_freed==HRS_FREEALL))
      flushStripesInMap(m, 1);
    else if (m->accessOrder==ONDIR)
      flushStripesInMap(m, 0);
    else 
      flushSomeStripesOFFDIR(m);
    m=m->next;
  }

  return;
}
