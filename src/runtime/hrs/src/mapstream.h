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

