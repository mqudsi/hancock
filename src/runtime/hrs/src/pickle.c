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
#include "HRS-internal.h"

typedef struct HRSpickle {
  char *name;
  int32 mode;        /* NEW, EXISTS, or DONTCARE */
  int32 readonly;  
  int32 pickleSize;
  HRSpickleData_s pickleData;
  int32 paramSize;
  void *paramData;
  char paramSet;
  int32 (*initFn)(char, void *, char *, HRSpickleData_s *, int, char);
  int32 (*writeFn)(char, void *, char *, HRSpickleData_s *, char, char);
  struct HRSpickle *next;
} *HRSpickle_t;

HRSpickle_t headPickleList = NULL;

static HRSpickle_t findPickle(void *dp)
{
  HRSpickle_t ph = headPickleList;

  while ((ph != NULL) && (ph->pickleData.data != dp))
    ph = ph->next;

  return ph;
}


void *HRSpickleReg(char *name,
		   int32 mode,
		   int32 readonly,
		   char toplevel,
		   int32 pickleSize,
		   int32 paramSize,
		   char paramSet,
 		   void *paramData,
		   int32 (*initFn)(char, void *, char *, HRSpickleData_s *, 
				   int, char),
		   int32 (*writeFn)(char, void *, char *, HRSpickleData_s *, 
				    char, char))
{

  /* look up name in pickle list */
  HRSpickle_t tp = headPickleList;
  HRSpickle_t ph;
  char *funName = "HRSpickleReg";
  int rv;
  
  HRSexitSetup();

  while ((tp != NULL) && (strcmp(name,tp->name) != 0))
    tp=tp->next;

  if (tp != NULL) {
    if (tp->mode!=readonly) 
      HRSerrorExit1("HRSpickleReg: %s opened multiple times. \nPickles must either be opened as const all times or zero times.\n", name);
  
    if (mode==NEW)
      HRSerrorExit1("HRSpickleOpen: %s opened multiple times. \nA pickle can be opened as new only the first time.\n",name);

    if ((tp->pickleSize != pickleSize) ||
	(tp->initFn != initFn) ||
	(tp->writeFn != writeFn))
      HRSerrorExit1("HRSpickleOpen: reopening pickle %s with a different pickle type \n", name);

    /* types matched.  return pointer to the already open
       pickle */
    if (tp->paramSet) {
      if (paramSet && memcmp(tp->paramData, paramData, paramSize) != 0)
	HRSerrorExit1("HRSpickleOpen: reopening pickle %s with a different type parameters \n", name);
      return tp->pickleData.data;
    } else if (paramSet) {
      tp->paramSet = paramSet;
      memcpy(tp->paramData, paramData, paramSize);
      return tp->pickleData.data;
    } else {
      return tp->pickleData.data;
    }
  }

  ph = (HRSpickle_t) HRSmalloc(sizeof(struct HRSpickle), funName);

  ph->name = HRSmalloc(strlen(name)+1, funName);
  strcpy(ph->name, name);
  ph->mode = mode;
  ph->readonly = readonly;

  ph->pickleSize = pickleSize;
  ph->pickleData.file = NULL;
  ph->pickleData.data = (void *) HRSmalloc(pickleSize, funName);
  bzero(ph->pickleData.data, pickleSize);

  /* add pickle to the list of top-level data structures */
  /* if necessary                                     */
  if (toplevel)
    HRSaddPDS(PICKLE, ph->pickleData.data);

  ph->paramSize = paramSize;
  if (paramSize == 0) 
    ph->paramData = NULL;
  else {
    ph->paramData = (void *) HRSmalloc(paramSize, funName);
    if (paramSet) 
      memcpy(ph->paramData, paramData, paramSize);
    else
      bzero(ph->paramData, paramSize);
  }
  ph->paramSet = paramSet;

  assert(initFn != NULL);
  ph->initFn = initFn;

  if ((rv = initFn(ph->paramSet, ph->paramData, ph->name, &ph->pickleData, 
		   ph->mode, ph->readonly)) != HRS_OK) {
    char *errStr;
    int errLen = 100 + strlen(ph->name);

    errStr = (char *) HRSmalloc(errLen, funName);
    sfsprintf(errStr, errLen, "Init function for the pickle %s failed with error value %d.\n", ph->name, rv);
    HRSerrorExit(errStr);
  }

  /* once we have called the init function, the file will
     exist. */
  ph->mode = EXISTS;

  assert(writeFn != NULL);
  ph->writeFn = writeFn;

  ph->next = headPickleList;
  headPickleList = ph;

  return ph->pickleData.data;
}


void *HRSpickleParam(void *p)
{
  HRSpickle_t ph;

  if (p == NULL)
    HRSerrorExit("Cannot get parameters for a NULL pickle\n");

  ph = findPickle(p);

  return ph->paramData;
}

typedef int64 loc_t;

void HRSpickleCopy(void *fd, void *td)
{
  HRSpickle_t from, to;
  
  if ((fd == NULL) || (td == NULL)) 
    HRSerrorExit("Pickle values in :=: expressions must be connected to on-disk files\n");

  from = findPickle(fd);
  assert(from != NULL);

  to = findPickle(td);
  assert(to != NULL);
  assert(to->readonly == 0);

  /* flush pickle to disk without closing it. */
  if (from->readonly == 0) {
    if (from->writeFn(from->paramSet, from->paramData,
		      from->name, &from->pickleData, 0, 
		      from->readonly) != HRS_OK)
      HRSerrorExit1("Write function for pickle %s failed\n",
		   from->name);
  }

  /* copy flushed file */
  sfsync(from->pickleData.file);
  sfseek(from->pickleData.file, (Sfoff_t)0, 0);

  sfseek(to->pickleData.file, (Sfoff_t)0, 0);
  /* truncate to remove old data */
  ftruncate(sffileno(to->pickleData.file), 0);   

  if (sfmove(from->pickleData.file, to->pickleData.file, -1, -1) < 0)
    HRSerrorExit1("HRSpickleCopy: %s failed \n", to->name);

  /* initialize the "to" pickle */
  /*   init code will re-open the file */
  sfclose(to->pickleData.file);
  to->pickleData.file = NULL;

  if (to->initFn(to->paramSet, to->paramData, to->name, &to->pickleData, 
		 to->mode, to->readonly) != HRS_OK)
    HRSerrorExit1("Init function for pickle %s failed\n",
		 to->name);
}

void HRSpickleClose(void *dp)
{
  HRSpickle_t ph = headPickleList;
  HRSpickle_t prev = NULL;

  /* find the pickle and the entry in the list before the
     pickle */

  while ((ph != NULL) && (ph->pickleData.data != dp)) {
    prev = ph;
    ph = ph->next;
  }

  assert(ph != NULL);

  if (ph->writeFn(ph->paramSet, ph->paramData, 
		  ph->name, &ph->pickleData, 1, ph->readonly) != HRS_OK) {
    HRSerrorExit1("%s: Pickle write function failed\n", ph->name);
  }
		 
  if (prev == NULL) {
    assert(ph == headPickleList);
    headPickleList = ph->next;
  }
  else
    prev->next = ph->next;

  HRSfree(ph->name);
  HRSfree(ph->pickleData.data);
  HRSfree(ph);

}
