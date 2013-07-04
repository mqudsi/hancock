#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <assert.h>
#include <HRS.h>
#include "util.h"
#include "runtime.h"
#include "HRS-internal.h"

struct dir_s {
  char *dir;
  char **fileNames;
  int fnLength;
  void *dirStruct;
  int mode;      /* NEW, EXISTS, or DONTCARE */
  int readonly;  /* boolean */
  int extParamTableSize;
  void *extParamTable;
  char paramSet;
  void (*initFn)(char set, void *paramTable,
	         void *extParamTable, void *dirStruct,
		 char **fileNames, 
                 int mode, 
		 char readonly);

  void (*writeFn)(char set, void *extParamTable, 
		  void *dirStruct, char **fileNames, 
		  char close, char readonly);

  struct dir_s *next;
};

typedef struct dir_s *HRSdir_t;

HRSdir_t headDirList = NULL;

static HRSdir_t dirLookup(char *dirName)
{
  HRSdir_t h;

  for (h=headDirList; h != NULL; h=h->next) {
    if (strcmp(dirName, h->dir) == 0)
      return h;
  }

  return NULL;
}

void *HRSdirReg(char *dirName,
		char **fieldNames,
                int fnLen,
		int dirStructSize,
		int mode,
		int readonly,
		char toplevel,
                char paramSet,
		void *paramTable,
  		int extParamTableSize,
		void (*initFn)(char set, void *paramTable,
			       void *extParamTable, void *dirStruct,
			       char **fileNames, 
			       int mode,
			       char readonly),
		void (*writeFn)(char set, void *extParamTable, 
				void *dirStruct, char **fileNames, 
				char close, char readonly))
{
  int sDirLen = strlen(dirName);
  HRSdir_t h;
  struct stat stbuf;
  int fileexists;
  int i;

  HRSexitSetup();

  if ((h = dirLookup(dirName)) != NULL) {
    /* check to make sure that mode/readonly match */
    /* the opened file */

    if (readonly != h->readonly)
      HRSerrorExit1("Re-opening directory %s with non-matching qualifiers\n",
		 dirName);

    /* existing mode/new mode */
    /* DC/DC OK, DC/E OK, DC/N NOT */
    /* E/DC OK, E/E OK, E/N NOT */
    /* N/DC OK, N/E OK, N/N NOT */

    if (mode == NEW)
      HRSerrorExit1("Re-opening directory %s with non-matching qualifiers\n",
		 dirName);
    
    /* what kind of checking do we want to do on the directory
       descriptor? */
    return h->dirStruct;
  }

  fileexists = (stat(dirName, &stbuf) != -1);
  switch (mode) {
  case NEW:
    if (fileexists)
      HRSerrorExit1("%s: directory with mode NEW cannot exist\n", dirName);
    else {
      int v = mkdir(dirName, 00777);
      if (v != 0)
	HRSerrorExit1("%s: could not create directory\n", dirName);
    }
    break;

  case EXISTS:
    if (fileexists == 0)
      HRSerrorExit1("%s: directory with mode EXISTS must exist\n", dirName);
    else if (!(stbuf.st_mode & S_IFDIR))
      HRSerrorExit1("%s not a directory", dirName);
    break;

  case DONTCARE:
    if (fileexists == 0) {
      if (readonly) 
	HRSerrorExit1("%s: directory with mode CONST must exist\n", dirName);
      else {
	int v = mkdir(dirName, 00777);
	if (v != 0)
	  HRSerrorExit1("%s: could not create directory\n", dirName);
	mode = NEW;
      }
    }
    else { /* fileexists */
      if (!(stbuf.st_mode & S_IFDIR))
	HRSerrorExit1("%s not a directory", dirName);
      mode = EXISTS;
    }
    break;

  default:
    HRSerrorExit1("%s: unknown mode in HRSdirReg\n", dirName);
  }

  h = (HRSdir_t) HRSmalloc(sizeof(struct dir_s), "HRSdirReg");

  h->dir = (char *) HRSmalloc(sDirLen+1, "HRSdirReg");
  strcpy(h->dir, dirName);

  h->dirStruct = (char *) HRSmalloc(dirStructSize, "HRSmapOpen");
  h->mode = mode;
  h->readonly = readonly;

  /* add directory to the list of top-level data structures */
  /* if necessary                                     */
  if (toplevel)
    HRSaddPDS(DIRECTORY, h->dirStruct);

  h->fnLength = fnLen;
  h->fileNames = (char **) HRSmalloc(fnLen*sizeof(char *), "HRSdirReg");

  if (extParamTableSize > 0)
    h->extParamTable = (char *) HRSmalloc(extParamTableSize, "HRSmapOpen");
  else
    h->extParamTable = NULL;
    
  h->extParamTableSize = extParamTableSize;
  h->paramSet = paramSet;

  h->initFn = initFn;
  h->writeFn = writeFn;

  h->next = headDirList;
  headDirList = h;

  for (i=0; i < h->fnLength; i++) {
    int nl = strlen(fieldNames[i]);

    h->fileNames[i] = (char *) HRSmalloc(sDirLen+1+nl+1, "HRSdirReg");
    sfsprintf(h->fileNames[i], sDirLen+1+nl+1, "%s/%s", h->dir, 
	      fieldNames[i]);

  }

  initFn(paramSet, paramTable, h->extParamTable, h->dirStruct, 
	 h->fileNames, mode, readonly);

  return h->dirStruct;
}



void HRSdirClose(void *d)
{
  HRSdir_t dh = headDirList;   
  HRSdir_t prev;
  int i;

  prev = NULL;
  while ((dh != NULL) && (dh->dirStruct != d)) {
    prev = dh;
    dh = dh->next;
  }

  assert(dh != NULL);

  /* remove from dir list */
  if (prev == NULL)
    headDirList = dh->next;
  else
    prev->next = dh->next;

  dh->writeFn(dh->paramSet, dh->extParamTable,
	     dh->dirStruct, dh->fileNames, 1, dh->readonly);

  for (i=0; i < dh->fnLength; i++)
    HRSfree(dh->fileNames[i]);

  HRSfree(dh->fileNames);
  HRSfree(dh->dirStruct);
  HRSfree(dh->dir);
  HRSfree(dh->extParamTable);

  HRSfree(dh);
}


