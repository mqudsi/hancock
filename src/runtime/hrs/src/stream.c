#include <sfio.h>
#include <string.h>
#include <math.h>
#include <HRS.h>
#include "util.h"
#include <assert.h>
#include "mapstream.h"
#include "fixcut.h"
#include <dirent.h>
#include <sys/stat.h>
#include "HRS-internal.h"

/* all counts are in bytes unless other wise noted */

typedef struct {
  int (*translate)(char, void *, char *);
  int (*filter)(char, void *, char *);
} generativeStream_s;

typedef struct {
  int numFilesinStream;
  int count;
  int fpLen;
  Sfio_t *fp;  /* stdio FP used for general streams */

  char **fileNames;
  int numFileNames;

  char sio;  /* standard in stream */

  int (*translate)(char, void *, Sfio_t *, char *);
  int (*filter)(char, void *, Sfio_t *, char *);
} generalStream_s;


typedef struct {
  int numFilesinStream;
  int count;
  Sfio_t *pfp;

  char **fileNames;
  int numFileNames;

  int physicalSize;

  char *temp;

  int (*filter)(char, void *, char *, char *);
  int (*translate)(char, void *, char *, char *);

  char sio;  /* standard in stream */
} binaryStream_s;

typedef struct {
  HRSmap_t m;
  HRSnextEntry_t handle;
  int (*translate)(char, void *, char *, char *);
  int (*filter)(char, void *, char *, char *);
} mapStream_s;


#define HRS_MAPSTREAM 0

struct stream_s {
  int type;
  char filterOnly;  /* is this a filter-only (non-sorted) stream */
  int logicalSize;

  char paramSet;
  int paramTableSize;
  void *paramTable;

  Sfio_t *lfp;   /* holds sorted data */

  char *streamName;

  /* only one of gs, bs, and ms will have a non-null value */
  generalStream_s *gs;
  binaryStream_s *bs;
  mapStream_s *ms;
  generativeStream_s *gens;
  struct stream_s *next;

} HRSstream_s;

struct stream_s *HRSheadStream = NULL;

static int filterStream(HRSstream_t s, char *dataOUT);
static int getNextElement(HRSstream_t s, char *dataOUT);


/* some utilities */
static int countFiles (char *dir)
{
  DIR* dirp;
  struct dirent* entp;
  int count = 0;
  
  if (!(dirp = opendir(dir)))
    HRSerrorExit1("Could not find the directory %s\n", dir);

  while (entp = readdir(dirp))
    if ((strcmp(".", entp->d_name) != 0) &&
        (strcmp("..", entp->d_name) != 0))
      count++;

  closedir(dirp);

  return count;

}

/* get files names and check that they are all files (not directories)
   and that they all have the "right" size (that is, the size is
   a multiple of the physical record size) */

static void checkStreamFile(char *name, int phyRecSize)
{
  struct stat stbuf;
  int v = stat(name, &stbuf);

  if (v == -1)
    HRSerrorExit1("could not find file %s\n", name);

  if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
    HRSerrorExit1("%s is a directory, not a simple file\n", name);

  if ((phyRecSize != 0)  && (stbuf.st_size % phyRecSize) != 0)
    HRSerrorExit1("%s: file size is not a multiple of the physical record size\n",
               name);

}



static void getFileNames (char *dir, char **fileNames, int phyRecSize)
{
  DIR *dirp;
  struct dirent *entp;
  int i = 0;
  int extraLen = strlen(dir) + 1 + 1;
  
  if (!(dirp = opendir(dir)))
    HRSerrorExit1("Could not find the directory %s\n", dir);

  while (entp = readdir(dirp)) {
    if ((strcmp(".", entp->d_name) != 0) &&
        (strcmp("..", entp->d_name) != 0)) {
      int strsize = strlen(entp->d_name) + extraLen;

      fileNames[i] = (char *) HRSmalloc(strsize, "getFileNames");

      sfsprintf(fileNames[i], strsize, "%s/%s", dir, entp->d_name);
      checkStreamFile(fileNames[i], phyRecSize);
      i++;



      
    }
  }

  closedir(dirp);
}



static int isDir(char *fname)
{
  struct stat buf;
  int r;

  r = stat(fname, &buf);
  return ((r==0) && (buf.st_mode & S_IFDIR));
}



/* streamName can be "sfstdin" (or a variant), a single file name,
   or the name of a directory where all the files belong to
   the stream. */

static void binaryStreamReg(HRSstream_t s,int physicalSize, void *tf)
{
  s->type = HRS_BINARYSTREAM;

  s->bs = (binaryStream_s *) HRSmalloc(sizeof(binaryStream_s), 
				      "binaryStreamReg");
  s->gs = NULL;
  s->ms = NULL;

  s->bs->physicalSize = physicalSize;
  s->bs->temp = (char *) HRSmalloc(physicalSize, "binaryStreamReg");

  if (strcmp(s->streamName, "sfstdin") == 0 || 
      (strcmp(s->streamName, "stdin") == 0)) {
    s->bs->numFileNames = 0;
    s->bs->numFilesinStream = 1;  /* count standard in stream as one file */
    s->bs->fileNames = NULL;
    s->bs->sio = 1;  /* standard in stream */
  }
  else if (isDir(s->streamName)) {
    s->bs->numFileNames = countFiles(s->streamName);
    s->bs->fileNames = (char **) HRSmalloc(sizeof(char *)*s->bs->numFileNames,
				    "binaryStreamReg");
    getFileNames(s->streamName, s->bs->fileNames, physicalSize);
    s->bs->numFilesinStream = s->bs->numFileNames;
    s->bs->sio = 0;
  }
  else { /* single file stream */
    s->bs->numFileNames = 1;
    s->bs->fileNames = (char **) HRSmalloc(sizeof(char *), "binaryStreamReg");
    s->bs->fileNames[0] = (char *) HRSmalloc(strlen(s->streamName)+1, 
					    "binaryStreamReg");
    strcpy(s->bs->fileNames[0], s->streamName);
    checkStreamFile(s->bs->fileNames[0], physicalSize);
    s->bs->numFilesinStream = s->bs->numFileNames;
    s->bs->sio = 0;
  }
   
  s->bs->count = 0;
  s->bs->translate = (int (*)(char, void *, char *, char *))tf;
  s->bs->filter = NULL;
}


static void openFirstBinaryFile(binaryStream_s *bs)
{
  if (bs->sio)
    bs->pfp = sfstdin;
  else
    bs->pfp = HRSopenfile(bs->fileNames[0], "r");

  bs->count = 0;

}


static int binaryStreamAux(HRSstream_t s, char *dataOUT)
{ 
  binaryStream_s *bs = s->bs;
  int bytes = 0;
  int val = 0;

  assert(bs->temp != NULL);
  assert(bs->translate != NULL);

  do {
    bytes = sfread(bs->pfp, bs->temp, bs->physicalSize);
    /* files are checked to make sure that they a 
       multiple of the physical record size */
    if (bytes != bs->physicalSize) {
      if (++bs->count < bs->numFilesinStream) {
	HRSclosefile(bs->pfp);
	bs->pfp = HRSopenfile(bs->fileNames[bs->count], "r");
	continue;
      }
      else 
	return HRS_STREAM_STOP;
    }
    if (bs->filter != NULL) 
      val = bs->filter(s->paramSet, s->paramTable, bs->temp, dataOUT);
    else 
      val = bs->translate(s->paramSet, s->paramTable, bs->temp, dataOUT);

    if (val == HRS_STREAM_STOP)
      return val;

    else if (val < 0) {
      HRSerrorExit1("%s: translation function failed.\n",
		    s->streamName);
    }
    else if (val == HRS_STREAM_DROP_REC)
      continue;
    else return val;
  }
  while (1);

  /* should never get here */
}

static void generativeStreamReg(HRSstream_t s, void *tf)
{
  s->type = HRS_GENERATIVESTREAM;

  s->gens = (generativeStream_s *) HRSmalloc(sizeof(generativeStream_s), 
				       "generativeStreamReg");
  s->gs = NULL;
  s->bs = NULL;
  s->ms = NULL;
  
  s->gens->translate = (int (*)(char, void *, char *))tf;
  s->gens->filter = NULL;

}

static int generativeStreamAux(HRSstream_t s, char *dataOUT)
{
  generativeStream_s *gens = s->gens;
  int v;

  assert(gens->translate != NULL);

  do {
    if (gens->filter != NULL) 
      v = gens->filter(s->paramSet, s->paramTable, dataOUT);
    else
      v = gens->translate(s->paramSet, s->paramTable, dataOUT);
    if (v == HRS_STREAM_STOP)
      return HRS_STREAM_STOP;
    else if (v < 0) {
      HRSerrorExit1("%s: translation function failed.\n",
		    s->streamName);
    }
    else if (v == HRS_STREAM_KEEP_REC)
      return v;
    /* else continue around look */
  } while (1);

  /* should never get here */
}


static void generalStreamReg(HRSstream_t s, void *tf)
{
  s->type = HRS_GENERALSTREAM;

  s->gs = (generalStream_s *) HRSmalloc(sizeof(generalStream_s), 
				       "generalStreamReg");
  s->bs = NULL;
  s->ms = NULL;
  
  if (strcmp(s->streamName, "sfstdin") == 0 || 
      (strcmp(s->streamName, "stdin") == 0)) {
    s->gs->numFileNames = 0;
    s->gs->numFilesinStream = 1;  /* count standard in stream as one file */
    s->gs->fileNames = NULL;
    s->gs->sio = 1;  /* standard in stream */
  }
  else if (isDir(s->streamName)) {
    s->gs->numFileNames = countFiles(s->streamName);
    s->gs->fileNames = (char **) HRSmalloc(sizeof(char *)*s->gs->numFileNames,
					  "generalStreamReg");
    getFileNames(s->streamName, s->gs->fileNames, 0);
    s->gs->numFilesinStream = s->gs->numFileNames;
    s->gs->sio = 0;
  }
  else { /* single file stream */
    s->gs->numFileNames = 1;
    s->gs->fileNames = (char **) HRSmalloc(sizeof(char *)*s->gs->numFileNames,
				    "generalStreamReg");
    s->gs->fileNames[0] = (char *) HRSmalloc(strlen(s->streamName)+1, 
					     "generalStreamReg");

    strcpy(s->gs->fileNames[0], s->streamName);
    checkStreamFile(s->gs->fileNames[0], 0);
    s->gs->numFilesinStream = s->gs->numFileNames;
    s->gs->sio = 0;
  }
   
  s->gs->count = 0;
  s->gs->translate = (int (*)(char, void *, Sfio_t *, char *))tf;
  s->gs->filter = NULL;

}


/* is there stuff left?  if not, go to next file */
static int advFile(generalStream_s *gs)
{
  if (gs->fpLen == -1) {
    int c = sfgetc(gs->fp);
    if (c == EOF)
      return 0;
    sfungetc(gs->fp, c);
    return 1;
  } else {
    if (sftell(gs->fp) == gs->fpLen) {
      while (++gs->count < gs->numFilesinStream) {
	HRSclosefile(gs->fp);
	gs->fp = HRSopenfile(gs->fileNames[gs->count], "r");
	gs->fpLen = sfsize(gs->fp);
	if (gs->fpLen != 0)
	  return 1;
      }
      return 0;
    }

    return 1;
  }
  
}

static void openFirstGeneralFile(generalStream_s *gs)
{
  if (gs->sio) {
    gs->fp = sfstdin;
    gs->fpLen = -1;
  }
  else {
    gs->fp = HRSopenfile(gs->fileNames[0], "r");
    gs->fpLen = sfsize(gs->fp);
  }

  gs->count = 0;
}


static int generalStreamAux(HRSstream_t s, char *dataOUT)
{
  generalStream_s *gs = s->gs;
  int v;

  assert(gs->translate != NULL);

  if ((gs->fp == NULL) || (advFile(gs) == 0))
    return HRS_STREAM_STOP;

  do {
    if (gs->filter != NULL)
      v = gs->filter(s->paramSet, s->paramTable, gs->fp, dataOUT);
    else
      v = gs->translate(s->paramSet, s->paramTable, gs->fp, dataOUT);
	
    if (v == HRS_STREAM_STOP)
      return HRS_STREAM_STOP;
    else if (v < 0) {
      HRSerrorExit1("%s: translation function failed.\n",
		    s->streamName);
    }
    else if (v == HRS_STREAM_KEEP_REC) {
      return v;
    }
    else if (advFile(gs))
      continue;
    else  /* out of files */
      return HRS_STREAM_STOP;
  } while (1);

  /* should never get here */
}



HRSstream_t HRSstreamReg(char *streamName,
			 char paramSet,
			 void *paramTable,
			 int paramTableSize,
			 char streamType,    /* one of BINARY, GENERAL, GENERATIVE */
			 int physicalSize,
			 int logicalSize,
			 void *tf)
{
  HRSstream_t s;

  HRSexitSetup();

  s = (HRSstream_t) HRSmalloc(sizeof(struct stream_s), "HRSstreamReg");

  s->next = HRSheadStream;
  HRSheadStream = s;

  if (streamName == NULL) {
    assert(streamType == HRS_GENERATIVESTREAM);
    s->streamName = (char *) malloc (strlen("generativeStream") + 1);
    strcpy(s->streamName, "generativeStream");
  } else {
    s->streamName = (char *)HRSmalloc(strlen(streamName)+1, "HRSstreamReg");
    strcpy(s->streamName, streamName);
  }

  /* assume a non-sorted stream for now */
  s->filterOnly = 0;
  s->logicalSize = logicalSize;

  s->paramSet = paramSet;
  s->paramTableSize = paramTableSize;
  if (paramSet && (paramTableSize > 0)) {
    s->paramTable = (void *) HRSmalloc(paramTableSize, "HRSstreamReg");
    memcpy(s->paramTable, paramTable, paramTableSize);
  }
  else 
    s->paramTable = NULL;

  if (streamType == HRS_BINARYSTREAM)
    binaryStreamReg(s, physicalSize, tf);
  else if (streamType == HRS_GENERALSTREAM) {
    assert (physicalSize == 0);
    generalStreamReg(s, tf);
  }
  else {
    generativeStreamReg(s, tf);
  }

  return s;
}




#define numDigits(n) (((n) == 0) ? 1 : ceil(log10((double) (n)))+1)

static char **buildSortStr(HRSstream_t s, int keySize, int keyStart, int strip,
		    int *ac)
{
  char **av;
  int recSize = s->logicalSize+keySize;
  int ffSize = numDigits(recSize)+numDigits(keySize)+numDigits(keyStart)+6;

  if (strip == 1) {
    *ac = 3;
    av = (char **) HRSmalloc((*ac+1)*(sizeof(char *)), "bss");
    av[0] = "msort";

    av[1] = (char *) HRSmalloc(ffSize, "bss");
    sfsprintf(av[1], ffSize, "-ff%d:%d:%d", recSize, keySize, keyStart);

    av[2] = "-S";

    av[3] = NULL;
  }
  else {
    *ac = 2;
    av = (char **) HRSmalloc((*ac+1)*(sizeof(char *)), "bss");
    av[0] = "msort";
    av[1] = (char *) HRSmalloc(ffSize, "bss");
    sfsprintf(av[1], ffSize, "-ff%d:%d:%d", recSize, keySize, keyStart);
    av[2] = NULL;
  }
  return av;
}

void HRSstreamStart(HRSstream_t s,
		    void *filterFunction,
                    int numSlices,
		    HRSslice_t slices)
{
  assert(s != NULL);

  if (s->type == HRS_BINARYSTREAM) {
    s->bs->filter = (int (*)(char, void *, char *, char *))filterFunction;
    openFirstBinaryFile(s->bs);
  }
  else if (s->type == HRS_GENERALSTREAM) {
    s->gs->filter = (int (*)(char, void *, Sfio_t *, char *))filterFunction;
    openFirstGeneralFile(s->gs);
  }
  else if (s->type == HRS_GENERATIVESTREAM) {
    s->gens->filter = (int (*)(char, void *, char *))filterFunction;
  }
  else if (s->type == HRS_MAPSTREAM) {
    s->ms->filter = (int (*)(char, void *, char *, char *))filterFunction;
  }

  if (numSlices == 0)
    /* filter only stream */
    s->filterOnly = 1;
  else {
    int from_pipe[2];
    int to_pipe[2];
    int keySize;
    int recSize;
    Sfio_t *tp;
    fchandle *fch;
    int i;

    s->filterOnly = 0;

    /* set up fixcut to munge the data */
    fch = fcopen();
    for (i=0; i < numSlices; i++) {
      int bStrSize = numDigits(slices[i].begin);
      int eStrSize = numDigits(slices[i].end);
      int strSize = bStrSize + 1 + eStrSize + 
	                    strlen(slices[i].description) + 1;
      char *str = HRSmalloc(strSize, "HRSstreamStart");

      sfsprintf(str, strSize, "%d,%d%s", slices[i].begin,
		slices[i].end, slices[i].description);

      if (fcslice(fch, 'k', str) != 0)
	HRSerrorExit1("Fixcut error: %s\n" , fcerror(fch));

      /* do we need to/can we free str here? */
    }

    if ((recSize = fcready(fch, s->logicalSize)) < 0)
      HRSerrorExit1("Fixcut error: %s\n" , fcerror(fch));

    keySize = recSize - s->logicalSize;

    pipe(from_pipe);
    pipe(to_pipe);
    if (fork() == 0) {
      /* child */
      extern int HRSpuntExit;
      int ac;
      char **av;

      HRSpuntExit = 1;   /* don't do any work on exit in the child */

      av = buildSortStr(s, keySize, 0, 1, &ac);
      close(to_pipe[1]);
      close(from_pipe[0]);
      close(0); dup(to_pipe[0]); close(to_pipe[0]);
      close(1); dup(from_pipe[1]); close(from_pipe[1]);

      amrRSORT(ac, av);
      _exit(1);

    }
    else {
      /* parent */
      char *logicalData;
      uchar *mungedData;

      close(to_pipe[0]);
      close(from_pipe[1]);

      tp = sfnew(NULL, NULL, SF_UNBOUND, to_pipe[1], SF_WRITE | SF_SHARE);

      logicalData = HRSmalloc(s->logicalSize, "HRSstreamStart");

      while (getNextElement(s, logicalData) != 0) {
	int v;

	if ((mungedData = fcbuild(fch, (uchar *) logicalData, NULL)) == NULL)
	  HRSerrorExit1("Fixcut error: %s\n" , fcerror(fch));

        v = sfwrite(tp, mungedData, recSize);
	if (v == 0)
	  HRSerrorExit1("stream write to %s failed\n", s->streamName);
      }
      sfclose(tp);
      fcclose(fch);

      s->lfp = 
	sfnew(NULL, NULL, SF_UNBOUND, from_pipe[0], SF_READ | SF_SHARE);

      HRSfree(logicalData);
    }
  }
}

HRSstream_t HRSmapToStream(HRSmap_t m, void *low, void *high, int logicalSize)
{
  HRSstream_t s;
  HRSiterate_s is;

  HRSexitSetup();

  s = (HRSstream_t) HRSmalloc(sizeof(struct stream_s), "HRSstreamReg");
  s->next = HRSheadStream;
  HRSheadStream = s;

  s->type = HRS_MAPSTREAM;
  s->filterOnly = 0;
  s->streamName = (char *) HRSmalloc(sizeof(char), "HRSstreamReg");
  /* empty string */
  s->streamName[0] = 0;
  s->logicalSize = logicalSize;

  s->paramSet = 0;
  s->paramTableSize = 0;
  s->paramTable = NULL;

  s->ms = (mapStream_s *) HRSmalloc(sizeof(mapStream_s), "HRSstreamReg");

  is.it_map = m;
  is.low = low;
  is.high = high;
  s->ms->handle = HRSnextEntryInit(is);

  return s;
}

static int mapStreamAux(HRSstream_t s, char *data)
{
  mapStream_s *ms = s->ms;
  int v;
  void *phyKey;  /* translation function will know the
		    appropriate type */
  do {
    if ((phyKey = HRSgetNextExistingEntry(ms->handle)) == NULL)
      return HRS_STREAM_STOP;
    else { /* apply filter */
      if (ms->filter != NULL)
	v = ms->filter(s->paramSet, s->paramTable, (char *) phyKey, data);
      else
	v = ms->translate(s->paramSet, s->paramTable, (char *) phyKey, data);

      if (v == HRS_STREAM_STOP)
	return v;
      else if (v < 0) {
	HRSerrorExit1("(map to stream): translation function failed.\n", s->streamName);
      }
    }
  } while (v == HRS_STREAM_DROP_REC);

  return v;
      
}


static int getNextElement(HRSstream_t s, char *dataOUT)
{
  int rv;

  if (s->type == HRS_GENERALSTREAM) {
    rv = generalStreamAux(s, dataOUT);
    if (rv == HRS_STREAM_STOP) {
      s->gs->filter = NULL;
      return 0;
    }
    else return rv;
  }
  else if (s->type == HRS_GENERATIVESTREAM) {
    rv = generativeStreamAux(s, dataOUT);
    if (rv == HRS_STREAM_STOP) {
      s->gens->filter = NULL;
      return 0;
    }
    else return rv;
  }
  else if (s->type == HRS_BINARYSTREAM) {
    rv = binaryStreamAux(s, dataOUT);
    if (rv == HRS_STREAM_STOP) {
      s->bs->filter = NULL;
      return 0;
    }
    else
      return rv;
  }
  else { /* map stream */
    rv = mapStreamAux(s, dataOUT);
    if (rv == HRS_STREAM_STOP) {
      s->ms->filter = NULL;
      return 0;
    }
    else
      return rv;
  }
    
}

int HRSstreamNext(HRSstream_t s, char *data)
{
  int bytesread;

  if (s->filterOnly) {
    return getNextElement(s, data);    
  }
  else { /* sorted stream */
    bytesread = sfread(s->lfp, data, s->logicalSize);
    if (bytesread == 0)
      return 0;
    else if (bytesread != s->logicalSize) {
      HRSerrorExit1("stream read from %s failed.\n", s->streamName);
      /* won't get here */
      return 0;
    }
    else return 1;
  }
}

void HRSstreamDone()
{
  struct stream_s *s, *temp;
  int i;

    for (s=HRSheadStream; s != NULL; ) {
    if (s->streamName != NULL)
             HRSfree(s->streamName);
    if (s->type == HRS_BINARYSTREAM) {
      if (s->bs->temp != NULL)
	HRSfree(s->bs->temp);
      for (i=0; i < s->bs->numFileNames; i++)
	HRSfree(s->bs->fileNames[i]);
      if (s->bs->fileNames != NULL)
	HRSfree(s->bs->fileNames);
      HRSfree(s->bs);
    }
    else if (s->type == HRS_GENERALSTREAM) {
      for (i=0; i < s->gs->numFileNames; i++)
	HRSfree(s->gs->fileNames[i]);
      if (s->gs->fileNames != NULL)
	HRSfree(s->gs->fileNames);
      HRSfree(s->gs);
    }
    else if (s->type == HRS_GENERATIVESTREAM) {
             HRSfree(s->gens);
    }
    else {
      HRSfree(s->ms);
    }

    temp = s;
    s = s->next;
    HRSfree(temp);
  }
}
