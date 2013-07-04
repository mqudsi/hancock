#include <assert.h>

#ifndef __HANCOCK_LIBRARY_
#define __HANCOCK_LIBRARY_

/* map-to-stream stream definition */

stream HLmapStream_s(int (*fn)(char set, void *v, long long *pn), 
		     void *v)
       { fn(:v:) : void => long long; };


HLmapStream_s HLcreateMapStream(HRSmap_t m, char all,
				long long low, long long high);

#define OR_MERGE 0
#define AND_MERGE 1

HLmapStream_s HLcreateMapMerge2(HRSmap_t m1, HRSmap_t m2, 
			       char all, long long low, long long high,
			       char operation);

HLmapStream_s HLcreateMapMerge3(HRSmap_t m1, HRSmap_t m2, HRSmap_t m3,
			       char all, long long low, long long high,
			       char operation);



/* stream of keys definition */

int HLgetValidKey(char set, long long low, long long high,
		  Sfio_t *input, long long *pn);
stream HLkey_s(long long low, long long high) { HLgetValidKey(: low, high :) : Sfio_t => long long; };

#endif

