/*******************************************************************
*                                                                  *
*           This software is part of the hancock package           *
*                      FOR NONCOMMERCIAL USE                       *
*                                                                  *
*                Copyright (c) 2000-2001 AT&T Corp.                *
*                                                                  *
*             This software is licensed by AT&T Corp.              *
*         under the terms and conditions of the license in         *
* www.research.att.com/projects/hancock/distribution/LICENSE.html  *
*                                                                  *
*                 This software was created by the                 *
*                 Network Services Research Center                 *
*                        AT&T Labs Research                        *
*                         Florham Park NJ                          *
*                                                                  *
*                Anne Rogers <amr@research.att.com>                *
*             Karin Hogstedt <karin@research.att.com>              *
*******************************************************************/
#ifndef __HRS_H
#define __HRS_H

#include <sfio.h>
#include <stdlib.h>
#include <errno.h>
#include <autoconfig.h>
#include <strings.h>
#include <limits.h>
#include <unistd.h>

void bzero(void *s, size_t n);

#define HRS_ERROR -1
#define HRS_OK 0


/* Notes: modes should be one of NEW, EXISTS, or DONTCARE */ 
/*        readonly is a boolean */


/* Types for signatures */

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
			       char **fileNames, int mode, char readonly),
		void (*writeFn)(char set, void *extParamTable, 
				void *dirStruct, char **fileNames, 
				char close, char readonly));
void HRSdirClose(void *d);

#define HRS_STREAM_ERROR HRS_ERROR
#define HRS_STREAM_STOP 2
#define HRS_STREAM_KEEP_REC 1
#define HRS_STREAM_DROP_REC 0

#define HRS_GENERALSTREAM 1
#define HRS_BINARYSTREAM 2
#define HRS_GENERATIVESTREAM 3

typedef struct stream_s *HRSstream_t;

extern HRSstream_t HRSstreamReg(char *callDir,
				char paramSet,
				void *paramTable,
				int paramTableSize,
				char streamType, /* HRS_BINARYSTREAM,
						    HRS_GENERALSTREAM,
						    HRS_GENERATIVESTREAM */
						   
				int physicalSize,
				int logicalSize,
				void *transFunction);

/* extern HRSstream_t HRSstreamReg2(char *callDir,
				 char paramSet,
				 void *paramTable,
				 int extParamTableSize,
				int physicalSize,
				int logicalSize); */

typedef struct {
  int begin;
  int end;
  char *description;
} HRSslice_s, *HRSslice_t;

extern void HRSstreamStart(HRSstream_t s,
			   void *filterFunction,
                           int numSlices,
                           HRSslice_t slices);

extern int HRSstreamNext(HRSstream_t s, char *data);



typedef struct entry *HRSmap_t;

/* readonly */
#define READONLY 1
#define READWRITE 0

/* mode */
#define DONTCARE 0
#define EXISTS 1
#define NEW 2

typedef struct {
  int64 low;
  int64 high;
} range_desc_s;

#define MAXLEVELS 3
typedef struct {
  int levels;
  range_desc_s ranges[MAXLEVELS];
} HRSkey_desc_s;

extern void HRSgetSelfID(HRSmap_t m);

#define FUNCTIONDEFAULT 0
#define CONSTANTDEFAULT 1
#define MISSINGCONSTANTDEFAULT -1
#define PARAMFUNCTIONDEFAULT 2
#define HRS_REMOVEENTRY -2

typedef void *HRSmapFunDefault_t;

typedef struct {
  signed char type;
  HRSmapFunDefault_t f;
  char *constant;
} HRSmapDefault_s;

#define HRS_GENERIC_COMPRESS 0
#define HRS_USER_ENTRY_COMPRESS 1
#define HRS_USER_STRIPE_COMPRESS 2

typedef struct {
  signed char type;
  void *cf;
  void *df;
  void *extra;
} HRSmapCompress_s;


extern int HRSmapFlushEager;

extern HRSmap_t HRSLLmapOpen(char *filename, 
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
			     void *pt); 

void HRSmapClose(HRSmap_t m);

extern void *HRSmapGet(HRSmap_t m, void *pn, void *data);
extern void HRSmapPut(HRSmap_t m, void *pn, void *data);

extern int HRSmapQuery(HRSmap_t m, void *k);
extern int HRSmapTestKey(HRSmap_t m, void *k);
void HRSmapRemove(HRSmap_t m, void *k);

void HRSmapRanges(HRSmap_t m, long long *low, long long *high);

void HRSmapCopy(HRSmap_t from, HRSmap_t to);

HRSstream_t HRSmapToStream(HRSmap_t m, void *low, void *high,
			   int logicalSize);

void HRScvtOldMaptoNew(HRSmap_t from, HRSmap_t to); 

extern char *HRSprogramName;
extern void HRSinit(char *);
extern void HRSdone();

Sfio_t *HRSopenfile(char *name, char *type);
Sfio_t *HRSopenFileMode(char *name, int mode, char readonly);
void HRSclosefile(Sfio_t *f);

int HRSgetChar(char *s, long long *result);
int HRSisPositive(char *s);

long long int HRSstrtoll(char *__source, char **__endptr, int __base);
unsigned long long int HRSstrtoull(char *__source, char **__endptr,
			      int __base);
long double HRSstrtold(char *__source, char **__endptr);

void *HRSmalloc(int size, char *errorMsg);
void *HRSresize(void *p, int size, char *errorMsg);

/* pickles */

typedef struct {
  Sfio_t *file;
  void *data;
} HRSpickleData_s;

void *HRSpickleReg(char *name,
		   int32 mode,
		   int32 readonly,
		   char toplevel,
		   int32 pickleSize,
		   int32 paramSize,
		   char paramSet,
 		   void *paramData,
		   int32 (*initFn)(char set, void *paramTable, char *fn, 
				   HRSpickleData_s *d, int mode, 
				   char readonly),
		   int32 (*writeFn)(char set, void *paramTable, char *fn, 
				    HRSpickleData_s *d, char close, 
				    char readonly));
void HRSpickleCopy(void *from, void *to);
void *HRSpickleParam(void *p);
void HRSpickleClose(void *dp);

#endif


#ifndef LONGLONG_MAX
#define LONGLONG_MAX        9223372036854775807LL /* max "long long int" */
#endif
