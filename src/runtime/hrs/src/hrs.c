#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <malloc.h>
#include <unistd.h>
#include <sys/stat.h>
#include <assert.h>
#include <HRS.h>
#include <sfio.h>
#include "util.h"
#include <stdlib.h>
#include <math.h>
#include "runtime.h"
#include "HRS-internal.h"


char *HRSprogramName = NULL;
int HRSpuntExit = 0;    /* will be set when the atexit code should be
			   ignored */

void HRSexitSetup()
{
  static int exitSET = 0;

  if (exitSET == 0) {
    atexit (HRSdone);
    exitSET = 1;
  }
}
  
HRSpt_t headPTList;

void HRSinit(char *pgname)
{

  HRSexitSetup();
  HRSprogramName = HRSmalloc(strlen(pgname)+1, "HRSinit");
  strcpy(HRSprogramName, pgname);
  headPTList = NULL;
}

void HRSaddPDS(char type, void *handle)
{
  HRSpt_t tmp;
  /* need to add an entry to the top-level list */
  tmp = (HRSpt_t)HRSmalloc(sizeof(struct HRSpt_s), "HRSpickleReg\n");
  tmp->type = type;
  tmp->handle = handle;
  tmp->next = headPTList;
  headPTList = tmp;
}


void HRSdone()
{
  HRSpt_t l;

  if (HRSpuntExit)
    return;

  HRSstreamDone();

  for (l = headPTList; l != NULL; l = l->next) {
    if (l->type == DIRECTORY)
      HRSdirClose(l->handle);
    else if (l->type == MAP)
      HRSmapClose(l->handle);
    else {
      assert(l->type == PICKLE);
      HRSpickleClose(l->handle);
    }
  }

  if (HRSprogramName != NULL)
    HRSfree(HRSprogramName);
}

