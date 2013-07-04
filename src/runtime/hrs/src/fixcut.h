/* CRC=0028177574 */

/* Link in chain of things to be freed at close */
typedef struct freelink {
    struct freelink *next;
    void *mustfree;
} freelink;


/* Link in chain of operators */
typedef	struct oplink {
    struct oplink *next;
    int	operator;
} oplink;


/* Keep all boundaries as bit measures */

typedef	unsigned long	boundary;

typedef struct {
    boundary	start;			/* where the slice begins */
    boundary	stop;			/* ... and where it has ended */
} stst;

typedef unsigned char	uchar;

typedef struct seg {
    struct seg	*next;			/* next link in chain of slices */
    uchar	*base;			/* area in which slices occur */
    stst	bounds;			/* current and absolute ends */
    oplink	*chain;			/* optional list of operations */
    char	key;			/* what keyletter? */
} seg;


#define	BITSPERCHAR	(8)
#define	BITSHIFT	(3)
#define	BYTEMASK	((1<<BITSPERCHAR)-1)
#define	HIGHBIT		(1<<(BITSPERCHAR-1))

#define	BYTES(B)	((B)>>(BITSHIFT))
#define	BITS(B)		((B)&(BITSPERCHAR-1))
#define	BUILD(BY,BI)	(((BY)<<BITSHIFT)+(BI))


/* Summary structure */
typedef struct {
    seg		*SegList;	/* chain of slices to move into key */
    seg		*SegChain;	/* chain of available seg links */
    oplink	*OpChain;	/* chain of available operator links */
    freelink	*FreeChain;	/* stuff we must free upon closing */
    uchar	*KeyBase;	/* space for key if we must supply it */
    uchar	*TmpBase;	/* scratch space for intermediate ops */
    boundary	KeyBound;	/* size of key */
    boundary	RecBound;	/* size of input */
    char	Error[200];	/* space for error message */
} fchandle;


void * fcalloc(fchandle *fcp, size_t size);
fchandle * fcopen(void);
void fcclose(fchandle *fcp);
int fcalign(fchandle *fcp, long align);
int fcslice(fchandle *fcp, char keyletter, char *description);
long fcready(fchandle *fcp, long reclen);
char * fcerror(fchandle *fcp);
void bitcat(seg *tsp, seg *fsp);
uchar * fcbuild(fchandle *fcp, uchar *record, uchar *key);
boundary strtoboundary(const char *str, char **ptr);
char * boundarytostr(boundary b);
