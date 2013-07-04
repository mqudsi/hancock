#include <sfio.h>
#include <math.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <HRS.h>
#include "util.h"
#include "runtime.h"
#include "mapport.h"
#include "HRS-internal.h"

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <assert.h>

void HRSerrorExit(const char *s){
  if (HRSprogramName != NULL)
    sfprintf(sfstderr, "%s: ", HRSprogramName);
  else
    sfprintf(sfstderr, "No program name supplied: ");
  sfprintf(sfstderr, s);
  _exit(1);
}


void HRSerrorExit1(const char *s, char *d){
  if (HRSprogramName != NULL)
    sfprintf(sfstderr, "%s: ", HRSprogramName);
  else
    sfprintf(sfstderr, "No program name supplied: ");
  sfprintf(sfstderr, s, d);
  _exit(1);
}


Sfio_t *HRSopenfile(char *name, char *type)
{
  Sfio_t *f = sfopen(0, name, type);

  if (f == NULL)
    HRSerrorExit1("Open failed for file %s\n", name);

  return f;
}

Sfio_t *HRSopenFileMode(char *name, int mode, char readonly)
{
  char *type = (readonly) ? "r" : "rw";
  struct stat stbuf;
  int32 fileexists = (stat(name, &stbuf) != -1);
  Sfio_t *f;

  if ((fileexists == 0) && 
      ((mode == EXISTS) ||
       ((mode == DONTCARE) && (readonly))))
    HRSerrorExit1("%s: pickle declared with mode CONST must exist.\n",  name);
  else if ((fileexists == 1) && (mode == NEW))
    HRSerrorExit1("%s must not exist.\n", name);

  f = sfopen(0, name, type);

  if (f == NULL)
    HRSerrorExit1("Open failed for file %s\n", name);

  return f;
}

void HRSclosefile(Sfio_t *f)
{
  int v = sfclose(f);
  if (v < 0)
    HRSerrorExit("Close failed\n");

}

static void makeSpace(int16 urgent) {
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


void *HRSmalloc(int size, char *errorMsg)
{

  char *s;

  assert(size > 0);

  s = (char *) malloc(size);

  if (s == NULL) {
    makeSpace(0);
    s = (char *) malloc(size);
  }
    
  if (s == NULL) {
    makeSpace(1);
    s = (char *) malloc(size);
  }
    
  if (s == NULL) {
    char msg[100];
    struct rusage rusage;

    int val=getrusage(RUSAGE_SELF, &rusage);
    sfprintf(sfstdout,"ru_maxrss=\t%ld\n",rusage.ru_maxrss);
    sfprintf(sfstdout,"ru_ixrss=\t%ld\n",rusage.ru_ixrss);
    sfprintf(sfstdout,"ru_idrss=\t%ld\n",rusage.ru_idrss);
    sfprintf(sfstdout,"ru_isrss=\t%ld\n",rusage.ru_isrss);
    sfprintf(sfstdout,"ru_minflt=\t%ld\n",rusage.ru_minflt);
    sfprintf(sfstdout,"ru_majflt=\t%ld\n",rusage.ru_majflt);
    sfprintf(sfstdout,"ru_nswap=\t%ld\n",rusage.ru_nswap);
    sfprintf(sfstdout,"ru_inblock=\t%ld\n",rusage.ru_inblock);
    sfprintf(sfstdout,"ru_oublock=\t%ld\n",rusage.ru_oublock);
    sfprintf(sfstdout,"ru_msgsnd=\t%ld\n",rusage.ru_msgsnd);
    sfprintf(sfstdout,"ru_msgrcv=\t%ld\n",rusage.ru_msgrcv);
    sfprintf(sfstdout,"ru_nsignals=\t%ld\n",rusage.ru_nsignals);
    sfprintf(sfstdout,"ru_nvcsw=\t%ld\n",rusage.ru_nvcsw);
    sfprintf(sfstdout,"ru_nivcsw=\t%ld\n",rusage.ru_nivcsw);
    strcpy(msg, "Out of space in ");
    strcat(msg, errorMsg);
    HRSerrorExit(msg);
  }

  return (void *) s;
}

void *HRSresize(void *p, int size, char *errorMsg)
{

  char *s;

  s = (char *) realloc(p, size);

  if (s == NULL) {
    makeSpace(0);
    s = (char *) realloc(p, size);
  }
    
  if (s == NULL) {
    makeSpace(1);
    s = (char *) realloc(p, size);
  }
    
  if (s == NULL) {
    char msg[100];
    struct rusage rusage;

    int val=getrusage(RUSAGE_SELF, &rusage);
    sfprintf(sfstdout,"ru_maxrss=\t%ld\n",rusage.ru_maxrss);
    sfprintf(sfstdout,"ru_ixrss=\t%ld\n",rusage.ru_ixrss);
    sfprintf(sfstdout,"ru_idrss=\t%ld\n",rusage.ru_idrss);
    sfprintf(sfstdout,"ru_isrss=\t%ld\n",rusage.ru_isrss);
    sfprintf(sfstdout,"ru_minflt=\t%ld\n",rusage.ru_minflt);
    sfprintf(sfstdout,"ru_majflt=\t%ld\n",rusage.ru_majflt);
    sfprintf(sfstdout,"ru_nswap=\t%ld\n",rusage.ru_nswap);
    sfprintf(sfstdout,"ru_inblock=\t%ld\n",rusage.ru_inblock);
    sfprintf(sfstdout,"ru_oublock=\t%ld\n",rusage.ru_oublock);
    sfprintf(sfstdout,"ru_msgsnd=\t%ld\n",rusage.ru_msgsnd);
    sfprintf(sfstdout,"ru_msgrcv=\t%ld\n",rusage.ru_msgrcv);
    sfprintf(sfstdout,"ru_nsignals=\t%ld\n",rusage.ru_nsignals);
    sfprintf(sfstdout,"ru_nvcsw=\t%ld\n",rusage.ru_nvcsw);
    sfprintf(sfstdout,"ru_nivcsw=\t%ld\n",rusage.ru_nivcsw);
    strcpy(msg, "Out of space in ");
    strcat(msg, errorMsg);
    HRSerrorExit(msg);
  }

  return (void *) s;
}

void HRSfree(void *s)
{
  free(s);
}

int HRSisPositive(char *s)
{
  if (strlen(s) == 0) 
     return 1;
  else if (s[0] == ' ' || s[0] == '\t')
    return HRSisPositive(s++);
  else if (s[0] == '-') 
    return 0;
  else 
    return 1;
}

static int cnvHexDigit(char c, long long *result){
  switch (c){
  case '0' : case '1' : case '2' : case '3' : case '4' :
  case '5' : case '6' : case '7' : case '8' : case '9' : *result =  c - '0'; return 1;
  case 'a' : case 'A' : *result = 10; return 1;
  case 'b' : case 'B' : *result = 11; return 1;
  case 'c' : case 'C' : *result = 12; return 1;
  case 'd' : case 'D' : *result = 13; return 1;
  case 'e' : case 'E' : *result = 14; return 1;
  case 'f' : case 'F' : *result = 15; return 1;
  default : return 0;
  }
}

static int cnvHex(char *s, long long * result){
  int len = strlen(s);
  long long digit;
  int noerr;
  int i;

  if (len < 3) return 0;  /* can't be a hexadecimal encoding. \xhh */
  *result = 0;
  for (i = 2; i< len; i++){        /* string starts \x*/
     noerr = cnvHexDigit(s[i], &digit);
     if (!noerr) return 0;
     *result = 16 * (*result) + digit;
  }
  return 1;
}


static int cnvOct(char *s, long long * result){
  int len = strlen(s);
  long long digit;
  int noerr;
  int i;

  if (len < 2) return 0;  /* can't be a octal encoding. '\o' '\oo' or '\ooo' */
  *result = 0;
  for (i = 1; i< len; i++){        /* string starts \ */
     noerr = cnvHexDigit(s[i], &digit);
     if (!noerr) return 0;
     if (digit > 7) return 0;
     *result = 8 * (*result) + digit;
  }
  return 1;
}

int HRSgetChar(char *s, long long *result)
{
   if (strlen(s) == 0) 
     return 0;
   else if (s[0] == ' ' || s[0] == '\t') /* consume leading white space */
     return HRSgetChar(s++, result);
   else if  (strlen(s) == 1){ /* 'a' */
     *result = s[0];
     return 1;
   } else if ((strlen(s) == 2) && (s[0]== '\\')){ 
     /* '\n'  or '\o' */
     switch (s[1]){
       case 'a' : *result = '\a'; return 1;
       case 'b' : *result = '\b'; return 1;
       case 'f' : *result = '\f'; return 1;
       case 'n' : *result = '\n'; return 1;
       case 'r' : *result = '\r'; return 1;
       case 't' : *result = '\t'; return 1;
       case 'v' : *result = '\v'; return 1;
       case '\\' : *result = '\\'; return 1;
       case '\?' : *result = '\?'; return 1;
       case '\'' : *result = '\''; return 1;
       case '\"' : *result = '\"'; return 1;
       case '0' : case '1' : case '2' : case '3' : case '4' :
       case '5' : case '6' : case '7' : *result =  s[1] - '0'; return 1;
       default: return 0;
     }
   } else if ((strlen(s) >= 2) &&  (s[0]== '\\') && (s[1]== 'x')){ 
     /* \xhh  case */ 
       return cnvHex(s, result);
   } else if ((strlen(s) >= 2) && (s[0]== '\\')){ 
     /* \oo  case */ 
       return cnvOct(s, result);
   } else
    return 0;
}

long long int HRSstrtoll( char *__source, char **__endptr, int __base)
{
  return strtoll(__source, __endptr, __base);
}

unsigned long long int HRSstrtoull( char *__source, char **__endptr, 
			      int __base)
{
  return strtoull(__source, __endptr, __base);
}

/*******************************************************
**                                                    **
** Low-level functions reading and writing bytes      **
**                                                    **
*******************************************************/

#define MAX_IO_SIZE 2048000

/* Reads numBytes bytes from f at locations start and puts it in data. */
/* data will be portable bytes */
void HRSreadBytes(fileInfo_s fi, uint8 *data, Sfoff_t start, Sfoff_t numBytes)
{
  int64 bytesLeft = numBytes;
  int64 rval;
  Sfio_t *f = fi.file;
  char *filename = fi.name;
  char *fname = "HRSreadBytes";

  assert(data != NULL);

  if (sfseek(f, start, 0) < 0) {

    int32 errLen = strlen(filename) + 50 + 1;
    char *errStr = (char *) HRSmalloc(errLen, fname);
  
    sfsprintf(errStr, errLen, "Fseek failed in readbytes file = %s, loc = %lld\n", 
	      filename, start);
    HRSerrorExit(errStr);
  }

  while (bytesLeft >= MAX_IO_SIZE) {
    rval = sfread(f, data, MAX_IO_SIZE);
    if (rval != MAX_IO_SIZE) {
      int32 errLen = strlen(filename) + 50 + 1;
      char *errStr = (char *) HRSmalloc(errLen, fname);
  
      sfsprintf(errStr, errLen, "Error in readbytes file = %s\n", 
		filename);
      HRSerrorExit(errStr);
    }

    bytesLeft -= rval;
    data += rval;
  }

  if (bytesLeft) {
    rval = sfread(f, data, bytesLeft);
    if (rval != bytesLeft) {
      int32 errLen = strlen(filename) + 50 + 1;
      char *errStr = (char *) HRSmalloc(errLen, fname);
  
      sfsprintf(errStr, errLen, "Error in readbytes file = %s\n", 
		filename);
      HRSerrorExit(errStr);
    }
  }
}


/* writes numBytes bytes of data to file f at location start */
/* data should be portable bytes */
void HRSwriteBytes(fileInfo_s fi, uint8 *data, Sfoff_t start, Sfoff_t numBytes, char syncRequired)
{
  int64 bytesLeft = numBytes;
  Sfio_t *f = fi.file;
  char *filename = fi.name;
  char *fname = "HRSwriteBytes";

  assert(data != NULL);

  if (sfseek(f, start, 0) < 0) {
    int32 errLen = strlen(filename) + 50 + 1;
    char *errStr = (char *) HRSmalloc(errLen, fname);
  
    sfsprintf(errStr, errLen, "Fseek failed in HRSwriteBytes file = %s, loc = %lld\n", 
	      filename, start);
    HRSerrorExit(errStr);
  }

  while (bytesLeft >= MAX_IO_SIZE) { 
    if (sfwrite(f, data, MAX_IO_SIZE) != MAX_IO_SIZE) {
      int32 errLen = strlen(filename) + 50 + 1;
      char *errStr = (char *) HRSmalloc(errLen, fname);
  
      sfsprintf(errStr, errLen, "Error HRSwriteBytes file = %s\n", 
		filename);
      HRSerrorExit(errStr);
    }
    bytesLeft -= MAX_IO_SIZE;
    data += MAX_IO_SIZE;
  }

  if (bytesLeft) {
    if ((sfwrite(f, data, bytesLeft)) != bytesLeft) {
      int32 errLen = strlen(filename) + 50 + 1;
      char *errStr = (char *) HRSmalloc(errLen, fname);
  
      sfsprintf(errStr, errLen, "Error in HRSwriteBytes file = %s \n", 
	      filename, start);
      HRSerrorExit(errStr);
    }
  }

  if (syncRequired) 
    sfsync(f);
}

char isP2(int32 v, int32 *sb)
{
  int32 b;
  int32 i;

  assert(v != 0);

  if (v == 1) {
    *sb = 0;
    return 1;
  }

  b = 1;
  for (i = 1; i < 32; i++) {
    b = b << 1;
    if (v == b) {
      *sb = i;
      return 1;
    } else if ((v & b) && (v > b))
      return 0;
  }

  /* should never get here */
  return 0;
}
