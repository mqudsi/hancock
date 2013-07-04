#include <vcodex.h>

#define HRS_INVALID -1
#define HRS_IN_FILE -2
#define HRS_IN_BACKUPFILE -3

#define HRS_FAILURE -1
#define HRS_ERROR -1
#define HRS_FREEALL -1


/* File modes */
#define READONLY 1
#define READWRITE 0

#define SUBBLOCKSIZE 256
#define MIN_MEMUSAGE 150000000

#define NUM_BYTES_2B_FREED_ONDIRDEFAULT (1000*10000)
#define NUM_BYTES_2B_FREED_OFFDIRDEFAULT (500*10000)
#define ACCESS_ORDER_WINDOW 100

#define COMPRESSED_SELF_ID_BV 4
#define STRIPED_MAP 6
#define OLD_STRIPED_MAP 5
#define LL_SPLIT_MAP 7

#define OSM_check(m) if ((m)->type == OLD_STRIPED_MAP) \
                        HRSerrorExit1("OLD_STRIPED_MAP error: Map %s can only be used with HRScvtOldtoNew\n", (m)->fi.name)

#define COMPLETE_check(m) if ((m)->complete == 0) \
                        HRSerrorExit1("incomplete map error: Map %s must have a complete type\n", (m)->fi.name)

#define numSubblocks(m, size) (ceil((double) (size)/(double) (m)->block.subBlockSize))


#define MAX_COMPRESSION 2
#define ONDIR 0
#define OFFDIR 1

extern HRSmap_t headMapList;
extern HRSmap_t LRUmapList;

typedef int64 loc_t;

typedef int32 stripeSize_t;

typedef uint8 *bv_t;

typedef struct {
  stripeSize_t size;
  bv_t bv;     /* bv == sp */  /* uint32?*/
  uint8 *cdata; /* cdata = sp + BVSizeinBytes */
  uint8 *sp; 
} stripe_t;

/* OLD:  sizeandstripe_t **stripesInMem; */
/* OLD: int32 *bitvector; */
typedef struct {   
  stripeSize_t *stripeLocations;
  stripe_t *stripesInMem;
  uint8 blockUpdated;
} stripeCache_t;

typedef struct {   /* block locations */
  loc_t loc;       /* where in file is this block (=start of BV) located?*/
  stripeSize_t size;        /* compressed size of this block in file */
} index_entry_s;


/* index of locations for the blocks in the file */
typedef struct {        
  int32 initialized;      /* has the index been initialized? */
  int32 numEntries;       /* number of blocks */
  int32 updated;          /* Has the index changed? */
  int32 maxBSseen;        /* maximum block size seen in index */
                          /* this will be an upper bound, since
			     blocks can shrink. */   
  index_entry_s *index; 
  uint8 *fbV;             /* full bit vector used only for
			     COMPRESSED_SELF_ID_BV maps */
  stripeCache_t **scV;
} index_s;

/* integer based bit vector */

#define BVBASE 8

#define BVBYTES8 8
#define BVBASE8 64
#define CSIDBV_BVBASE 32

#define BYTEALIGNMENT 8
#define bytealign(b) ((((b)%BYTEALIGNMENT) == 0) ? (b) : ((((b)/BYTEALIGNMENT)+1)*BYTEALIGNMENT))


typedef struct {        /* field of consequtive stripes  */
  int32 numEntries;       /* total number of entries in a block */

  int32 numStripes;       /* number of stripes */
  int32 numStripeEntries; /* number of entries in a stripe */

  int32 BVSize;    /* ceil(numEntries / BVBASE) bytealigned */
  int32 SLSize;   /* numStripes*sizeOf(stripeSize_t) bytealigned*/

  int32 entrySize;        /* size of uncompressed entry in a stripe */
  stripeSize_t stripeSize;       /* size of uncompressed stripe */
  int32 subBlockSize;     /* file sub-block size */

  int32 maxBlockSizeinFile;  /* maxium number bytes required to */
                                 /* hold this kind of block. */
  int32 nsb;              /* max number of subblocks.  used for the
			     free list stuff */
  loc_t end;            /* end of blocks in the file */

  uint8 *tmpSpace;   /* used to construct blocks, or to move
			     them from a backup file */
} block_s;

typedef struct newListEle {
  HRSmap_t map;
  struct newListEle *next;
} newList_t;

typedef struct fle {
  loc_t loc;
  struct fle *next;
} freeListEntry_s, *freeListEntry_t;

typedef struct {
  loc_t blockEnd;
  freeListEntry_t *fl;
} freeList_t;

#define FLBUCKETSIZE 50
#define FLMINSIZE 50

typedef struct {
  int32 bn;
  int32 stripe;
  bv_t bv;  
  uint8 *data;     /* uncompressed data for the stripe */
  int32 updated;
  uint8 *cdata;    /* compressed data */
} stripeID_t;



struct extraCompress_s {
  Vcdisc_t	cVcdisc;    /* for generic compression using vcrle */
  Vcodex_t*	cvc;

  Vcdisc_t	dVcdisc;    /* for generic decompression using vcrle */
  Vcodex_t*	dvc;

  uint8 *tmp;
  int32 tmpSize;
};



typedef struct {
  int block;
  int line;
} HRSkey_s;

typedef struct {
  int block;
  int stripe;
  int entry;
} HRS3Key_s;

typedef long long HRSLLKey_t;

typedef struct HRSnextEntry_s {
  HRSmap_t themap;
  HRS3Key_s high;
  HRS3Key_s next;
  int numSlots;    
  int nextSlot;
  void *resultKey;   /* used to pass data back to 
			mapStreamAux */
  HRS3Key_s *resultK3;  /* use to cache 3-level keys for LL keys */
  int numMaps;
} HRSnextEntry_s;


struct entry {
  fileInfo_s fi;

  char complete;     /* do we have all the necessary information for 
			this map type? */

  int32 selfIDSize;  /* selfID consists of type+block.entrySize+defaults */
  int32 headerSize;  /* sizeIDSize + indexSize */

  int32 type;        /* type of map */

  HRSmapDefault_s def;
  index_s index;
  block_s block;
  freeList_t fl;

  HRSmapCompress_s compressInfo;

  int32 cf;

  /* original key.  use this one for validating keys */
  HRSkey_desc_s origKey;
  char blockP2;
  int32 blockSplit;   /* used only for LL_SPLIT  */
  int32 blockShift;   /* use for blockSplits that are powers of 2 */   
  char stripeP2;
  int32 stripeSplit;  /* used only for LL_SPLIT */
  int32 stripeShift;  /* use for stripeSplits that are powers of 2 */   
  HRSkey_desc_s adjKey;

  char set;           
  int32 ptSize;       
  void *pt;           

  stripeID_t uncompressedStripeInfo;
  int16 accessOrder;
  int32 lastaccessedblock;

  int16 isAccessOrderOffdir;
  int32 numAccesses;
  int32 updateSelfID;

  int32 num_bytes_2B_freed;
  stripeSize_t *newStripeLocations;

  struct entry *backup;  /* for copy on access stuff */
  newList_t *copyto; 

  HRSLLKey_t lastSeen;
  HRS3Key_s lastSeen3; 

  struct entry *next;
};

#define SYNC 1
#define NOSYNC 0



/* default fn types */
typedef int (*HRSmapFun2K_t)(HRSkey_s pn, char *space);
typedef int (*HRSmapFun3K_t)(HRS3Key_s pn, char *space);
typedef int (*HRSmapFunLL_t)(HRSLLKey_t pn, char *space);
typedef int (*HRSmapFunLLParam_t)(char set, void *pt,
				   HRSLLKey_t pn, char *space);


/* compression fn types */
typedef int32 (*HRSentryCompressFn_t)(char, void *, uint8 *, uint8 *, int32);
typedef int32 (*HRSentryDecompressFn_t)(char, void *, uint8 *, int32, uint8 *);

typedef int32 (*HRSstripeCompressFn_t)(char set,  
				       void *pt, 
				       uint8 *from, 
				       int32 numFrom,
				       uint8 *to, 
				       int32 to_size);

typedef int32 (*HRSstripeDecompressFn_t)(char set, 
					 void *pt, 
					 uint8 *from, 
					 int32 from_size, 
					 uint8 *to,
					 int32 numTo);

