/* External sort based on Peter McIlroy's binary merge internal sort
** and two-array radix sort as modified by jpl to sort (only)
** arrays of pointers to records with a single, lexicographical key.
*/

static	char	RcsID[] = "CRC=4193038878 $Id: rsort.c,v 1.3 2001/03/16 22:41:21 amr Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#ifdef	MMAP
#include <sys/mman.h>
#endif

#ifdef	HUGE
typedef	long long	verylong;
typedef	off64_t		offset;
typedef struct stat64	filestatus;
#define	STAT(PATH,ST)	stat64(PATH,ST)
#define	FSTAT(FD,ST)	fstat64(FD,ST)
#define	LSEEK(FD,OFF,W)	lseek64(FD,(offset)(OFF),W)
#define	MAP(ADDR,LEN,PT,FL,FD,OFF)	(uch*)mmap64(ADDR,LEN,PT,FL,FD,(offset)(OFF))
#else
typedef	long		verylong;
typedef	off_t		offset;
typedef struct stat	filestatus;
#define	STAT(PATH,ST)	stat(PATH,ST)
#define	FSTAT(FD,ST)	fstat(FD,ST)
#define	LSEEK(FD,OFF,W)	lseek(FD,(offset)(OFF),W)
#define	MAP(ADDR,LEN,PT,FL,FD,OFF)	(uch*)mmap(ADDR,LEN,PT,FL,FD,(offset)(OFF))
#endif

#ifdef	DOSLIKE
#include <io.h>
#include <process.h>
#else
#include <unistd.h>
#endif


/* Make sure a few error codes are defined, as 0 if necessary */
#ifndef	ENOMEM
#define	ENOMEM	0
#endif
#ifndef	EIO
#define	EIO	0
#endif
#ifndef	ENOTDIR
#define	ENOTDIR	0
#endif


/* Although this started as a sort capable of handling what will be called
** ``header delimited'' records, it generalized nicely to something that
** would also work on ``newline delimited'' or fixed length records.
** We'll start by defining the less familiar header-delimited records,
** since they introduce some concepts that are useful for the simpler forms.
**
** A header-delimited record is a sequence of bytes starting with an
** ASCII ``header'', followed by arbitrary bytes whose length, and, to a
** very minor extent, contents, are defined by the header.
** Here's a picture of the most general form currently supported:
**
** <reclen>:<keylen>:<keyoff>;{ record     {    key     }        }
**                            |<- keyoff ->|<- keylen ->|
**                            |<-           reclen             ->|
**
** <reclen>, <keylen> and <keyoff> are (ASCII) digit-strings that will
** be converted to long integers internally.  (The conversion happens
** only when the records are read, not on every comparison, so the
** extra overhead costs relatively little compared to the benefits.)
**
** The non-header part of the record is the <reclen> bytes that follow
** the semicolon that always terminates the header part.  Note that the
** length of the header itself is NOT figured in the <reclen>.
** Records of length 0 are legitimate, if rather uninteresting.
**
** There is a single key, with comparisons made lexicographically.
** No multiple fields, no fancy types.  Those can be supported externally.
**
** In the simplest case, the key is located at the start of the non-header
** part of the record, <keyoff> == 0.  In this case, the :<keyoff> portion
** of the header can be omitted.
**
** If the entire non-header part of the record is to be treated as a key,
** the :<keylen> portion of the header can also be omitted,
** with <keylen> implicitly equal to <reclen> (and <keyoff> == 0).
**
** Note that per-record overhead need not be high.  For example,
** if records are less than 100 bytes long, there are no more
** than three bytes of overhead associated with storing reclen alone,
** and no more than six for reclen and keylen.
*/


/* A few words of explanation about header-delimited records.
**
** The non-header portion of the record can contain ANYTHING.
** Multiple newlines, null characters, binary data of any sort.
** The only part that is interpreted in any way is the key,
** which is, itself, identified by offset and length, not contents.
** This format is well suited to sorting complex data, such as
** records from ``real'' databases, or multiline ASCII records.
**
** In a sense, support for a non-zero key offset is unnecessary.
** Some preprocessor stage will generally be needed to generate
** the sort key so that lexicographical comparison ``does the right thing''.
** This key can be placed at the start of the non-header record data,
** followed by the original record from which it was derived,
** and a postprocessor can separate the key from the original record.
** The non-zero offset IS supported, in part because a lengthy sort key
** may already be part of the original record data, but not at the very start.
** Duplicating the data would increase record size.  Furthermore,
** support for the option costs very little.  Internally, records
** are summarized by two pointer/length pairs, roughly like:
**
**   key-pointer/keylen -------------------------+
**                                               |
**                                         +-----+------+
**                                         |            |
** <reclen>:<keylen>:<keyoff>;{ record     {    key     }        }
** |                                                             |
** +-------------------------------+-----------------------------+
**                                 |
**   record-pointer/reclen' -------+
**
** Where the key-related pair is used for comparisons, and the record-related
** pair, with <reclen'> modified to include both header and non-header parts
** of the record, is used for I/O.  Neither pair can be deduced from the
** other without re-parsing the header information, which we prefer not to do.
** Constraining the key to occur at offset 0 wouldn't simplify the summary,
** since the header itself is of variable length.
*/


/* Other record ``formats''.
**
** Having considered the general case of header-delimited records,
** let's consider some simpler record formats.  Among the simplest
** is records of fixed length.  Now record length is ``global'',
** rather than being defined for individual records.  And it's
** easy enough to support global key offset and key length as well.
** This format doesn't require the key and record pointer/length pairs
** that the header-delimited records use.  An array of pointers to
** the records, together with global length and offset values, suffice.
** Since (jpl's version of Peter McIlroy's) merge and radix sorts require
** two pointer arrays to address the array of records to be sorted,
** the use of main memory for sorting such records is nearly optimal.
** Of course, the internal comparison routines must compensate for the
** different internal storage representations, but, like qsort,
** merge sort invokes a user-supplied routine to compare records,
** so this is easily accomplished, with the arsenal of comparison
** functions defined below.  And it is worthwhile, because a lot
** of the sort functionality, like file manipulation, is independent
** of the comparison routines.  So we can get an ``external qsort()''
** (limited to lexicographical comparison, of course) almost free.
**
** Midway in complexity is newline-delimited records, the bread and butter
** of UNIX files.  As with fixed-length records, accommodating these
** adds very little to overall complexity when compared to the gain
** in generality, and the possibility of skipping the key-building process.
** And, as with fixed-length records, support for global key offset and
** key length is reasonable.  Newline-delimited records are summarized
** with a single pointer/length pair if the entire record is used as key.
** Of course, this format is only useful if a single lexigocraphical key
** occurs at a fixed offset in each record, rather like the system sort
** restricted to no arguments.
**
** We will also allow globally-specified key offset and/or key length
** in conjunction with newline-delimited files.  In their absence,
** the entire record is treated as the key.  If an offset is specified,
** the key will be regarded as starting that number of bytes from the
** start of the record, as long as the record is at least that long.
** Otherwise, the key will be treated as having 0 length.  If a positive
** key length is specified, it becomes an upper limit on the key length.
** The key length may be adjusted down if that is necessary to confine
** the key to the record.  Two pointer/length pairs are used to summarize,
** one pair for the record, the other for the key.
*/


/* Process model.
**
** jpl was never happy about the role of the sort routine as a parser
** and interpreter.  There is the philosopher's quandary of
** ``Why should I be able to sort by month but not by weekday?''.
** Adding support for weekday-sort (Wednesday < Thursday) is possible,
** of course, but this approach never stabilizes, because people
** will always think of new options that are not supported.
** The other alternative, of eliminating virtually ALL interpretation
** from sort, has a better defined endpoint.
**
** Add to this the hacker's disgust with parsing and interpreting records
** AT EVERY COMPARISON.  This simultaneously has a wretched impact on
** performance, when sort fields are complex and deeply embedded,
** and on record format, which must be limited to be parsed.
** Keys can be built once on input, of course, but that means the
** temporary files are structurally different from the inputs,
** something that makes jpl vaguely uneasy.
**
** By separating the role of key BUILDING from key COMPARISON,
** one ends up with a sort that is potentially more capable AND
** more efficient.  So this sort, in the best tradition of UNIX tools,
** is good at one thing, comparing keys, and it relies on other tools
** to do the key construction.  Awk and Perl are very well suited
** to this task.
**
** The general processing model is therefore:
**
** Generate-records-with-keys | sort-keys | separate-records-from-keys
**
** Clearly, anything with three processes involved is not going to
** uniformly out-perform the standard system sort.  In many cases,
** though, the final separation process can be performed by the sort.
** In particular, when the first phase has generated input of the form:
**
** <header-info><key-info><original-record>
**
** (with the key at offset 0), then the header contains all the
** information necessary to strip off header and key on final output,
** yielding only a sorted stream of record information.
** In many cases, a two-process model will be adequate.
*/


/* Lessons learned along the way:
**
** A stdio-based, record-at-a-time version was observed (on an SGI)
** to spend approximately 50% of the CPU time in fread, fwrite and
** memcpy calls on their behalf.  Although simpler and more portable,
** this overhead was unacceptable.  The current version uses read
** and write directly (or mmap, where that is supported), and
** parses whole blocks of records at a time.  Although considerably
** more complicated, it is also considerably faster.
**
** Radix sort outperforms merge sort on compact, disorderly keys.
** The current version uses a radix sort if too many runs are
** observed while preparing for a merge sort.
**
** Both of these lessons were learned at the knees of Glenn Fowler
** and Phong Vo, whose sfio-based implementations outstripped the
** original stdio version.
**
** Overall performance is likely to depend more than one would like
** on system-specific differences.  For example, on the SGI challenge
** used for most development and testing, there was a 2-level memory
** heirarchy.  An on-chip 16K data cache was too small to have much
** obvious impact, but the per-processor 4 Meg cache made its presence
** known if working sets got too large.  We observed cases where it
** was better to do two merge passes, and keep memory sizes small
** enough to fit in the per-processor cache, than to exceed that
** cache size and do only one merge pass.  The default ``memory size''
** (and the -y option) are intended to keep the working set bounded,
** for just this type of behavior.  Older implementations of /bin/sort
** used the memory size parameter to control differing amounts of
** addressable memory on differing systems.  This sort is more
** concerned with bounding working sets than with bounding addressable
** memory.  In particular, when memory mapping files, dynamically
** allocated ``real memory'' is not returned, but it is ``taken out
** of circulation'' so the total memory use is bounded.
**
** Memory mapping is not an unvarnished win.  On output on the SGIs,
** jpl measured normal writes to be significantly better, an observation
** that Phong agreed with (sfio doesn't use memory mapping on output).
** Even on input, where memory mapping should save a copy from kernel
** buffers to user space, a reduction in sys+user time was more than
** compensated for by an increase in wall-clock time, apparently due
** to page-at-a-time faulting as pages were mapped in.  This is an area
** that needs a lot more study.  jpl is mostly leaving MMAP undefined.
*/


/* Drop the typedefs, so they can appear in subroutine definitions */

typedef void * gptr;			/* generic pointer */
typedef unsigned char uch;		/* how keys compare */

/* For newline-delimited and header-delimited records,
** we need pointer/length structures to summarize records and keys.
*/

typedef struct {
    uch *	ptr;		/* Pointer to key or record */
    long 	len;		/* Length of string found there */
} ptrlen;


/* Round integer N down or up to the nearest multiple of integer M */
#define ROUNDDOWN(N,M) (((N)/(M))*(M))
#define ROUNDUP(N,M) ROUNDDOWN(((N)+(M)-1),(M))
#define	CEILING(N,M) (((N)+(M)-1)/(M))


#ifndef	DEFAULT_AUNIT
#define	DEFAULT_AUNIT	(128)
#endif

#ifndef	DEFAULT_TUNIT
#define	DEFAULT_TUNIT	(1024)
#endif

#ifndef	REGFILE_TUNIT
#define	REGFILE_TUNIT	(4096)
#endif


/* Temporary files
**
** We (may) create temporary files in the sort phase
** corresponding to sorted batches of input.
** Others may be created in the merge phase,
** as multiple input files are merged into a single combined file.
**
** Thanks to Don Caldwell for suggesting that a directory be created
** to hold all the temp files for a particular run.  This helps
** mitigate the limited file name length in certain feeble contexts,
** since the directory name can encode just the process ID,
** and the temp file names can encode just the temp sequence number.
** It also makes security a bit tighter, since the mkdir should fail
** if a symlink by the same name already exists, and the directory
** can be created with limited write access, to preclude other mischief.
**
** To further hold the line on file name length, we'll encode process id
** and index sequence number as alpha strings.
**
** With an alphabet of size 26 we have, for various encoding lengths,
**
**    26**2 =             676
**    26**3 =          17,576
**    26**4 =         456,976
**    26**5 =      11,881,376
**    26**6 =     308,915,776
**    26**7 =   8,031,810,176
**    26**8 = 208,827,064,576
**
** For merge tiebreakers, we'll be using index number, possibly signed.
** So we want the maximum sequence number to stay under 31 bits
** (2,147,483,648).  We'll limit SEQENCODE to 6 bytes.
*/


/* A typical tempfile under directory TDIR will have name
**	TDIR/stmPPPPP.dir/tXXXXXX
**      ^                 ^
**      |                 +------------ gp->tbase
**      +------------------------------ gp->tname
*/


/* Component parts from which to construct our private subdirectory name
**    stmPPPPP.dir
** where PPPPP is the encoded process id.
*/

#define	DIRPREFIX	"stm"
#define	DIRPREFLEN	(sizeof(DIRPREFIX)-1)
#define	PIDENCODE	(5)
#define	DIREXTENSION	".dir"
#define	DIREXTLEN	(sizeof(DIREXTENSION)-1)
#define DIRTOTALLEN	(DIRPREFLEN+PIDENCODE+DIREXTLEN)

/* Component parts from which to construct our base tempfile name
**    tXXXXXX
** where XXXXXX is the encoded index sequence number, 0 - ?????
*/

#define	BASEPREFIX	"t"
#define	BASEPREFLEN	(sizeof(BASEPREFIX)-1)
#define	SEQENCODE	(6)
#define	BASEEXTENSION	""
#define	BASEEXTLEN	(sizeof(BASEEXTENSION)-1)
#define BASETOTALLEN	(BASEPREFLEN+SEQENCODE+BASEEXTLEN)

/* Keep RESTART and RESTARTEMP no longer than BASETOTALLEN,
** either by choosing them to be no longer than the tempfile names,
** or by boosting BASETOTALLEN.  If exactly the same length as
** the tempfile names, be sure not to start the names with BASEPREFIX,
** or the tempfile for some suitable large index might be the same as
** the restart name.
*/

#define	RESTART		"restart"
#define RESTARTEMP	"restemp"


#define	ALPHABET	("abcdefghijklmnopqrstuvwxyz")


/* If we are memory mapping files, sort input files must stay open
** until their contents have been sorted, so we need an array of open files.
** In the merge phase, memory mapped or not, we have an array of merge
** inputs being merged onto a single output.  MAXBATCH defines the
** maximum number of such simultaneously open inputs.  There's no
** point making it any larger than the number of file descriptors.
** It might be made smaller, to increase the buffer space available
** to each input.
**
** In the merge phase, we do a binary insertion to establish the proper
** place for a new record in the array of inputs.  Pointers to the MAXBATCH
** (or fewer) inputs ``float'' in a larger array, so we can, in general,
** shift elements right or left to make room for the element to be inserted.
** Elements to be inserted start to the left of the floating array,
** so the element to be inserted always makes room for a left shift.
** If we run out of room on the right, we pack everything against the
** left end, making room for subsequent right-shifts.
*/

#ifndef	MAXBATCH
#define	MAXBATCH	(200)
#endif

#define	BIGBATCH	(MAXBATCH*3/2)


enum formats { FMTHDR, FMTFIX, FMTNEW };
#define	LARGERECLEN	(8192)			/* Generous guess at reclen */


/* There will only be one output file open at any given time,
** so we can access it globally instead of passing pointers to it.
*/

#ifndef	OBUFSIZE
#define	OBUFSIZE	(65536)
#endif
enum { COPY_BYTES, COPY_LONGS, COPY_MEMCPY };

#ifndef	LONG_THRESH
#define LONG_THRESH	(36)
#endif

#ifndef	BYTE_THRESH
#define BYTE_THRESH	(5)
#endif


/* ``Duff devices'' for shifting
** N must always start out greater than (never equal to) 0.
** It must be signed, since going negative must be detectable.
** All parameters will be re-evaluated and changed.
** TO will be left pointing to the first unused slot,
** FROM just past the last item copied.
**
** When rightshifting, TO and FROM should start just PAST the
** destination and target arrays.
**
** A few optimizing compilers have trouble with the Duff device,
** typically yielding something like
**    warning(1127): loop is not reachable from preceding code
** If you see that, try compiling with -NODUFF, which is a hair
** cleaner in any event, since it deals with 0 bytes better.
*/

#ifdef	NODUFF
#define SLAMLEFT(TO,FROM,N) \
    { \
	while ((N)>=8) { \
	    *(TO)++ = *(FROM)++; *(TO)++ = *(FROM)++; \
	    *(TO)++ = *(FROM)++; *(TO)++ = *(FROM)++; \
	    *(TO)++ = *(FROM)++; *(TO)++ = *(FROM)++; \
	    *(TO)++ = *(FROM)++; *(TO)++ = *(FROM)++; \
	    (N) -= 8; \
	}; \
	switch((N)%8) { \
	    case 7: *(TO)++ = *(FROM)++; \
	    case 6: *(TO)++ = *(FROM)++; \
	    case 5: *(TO)++ = *(FROM)++; \
	    case 4: *(TO)++ = *(FROM)++; \
	    case 3: *(TO)++ = *(FROM)++; \
	    case 2: *(TO)++ = *(FROM)++; \
	    case 1: *(TO)++ = *(FROM)++; \
	}; \
    }
#else
#define	SLAMLEFT(TO,FROM,N) \
    switch((N)%8) { \
	do { \
	case 0: *(TO)++ = *(FROM)++; \
	case 7: *(TO)++ = *(FROM)++; \
	case 6: *(TO)++ = *(FROM)++; \
	case 5: *(TO)++ = *(FROM)++; \
	case 4: *(TO)++ = *(FROM)++; \
	case 3: *(TO)++ = *(FROM)++; \
	case 2: *(TO)++ = *(FROM)++; \
	case 1: *(TO)++ = *(FROM)++; \
	} while (((N)-=8) > 0); \
    }
#endif

#ifdef	NODUFF
#define	SLAMRIGHT(TO,FROM,N) \
    { \
	while ((N)>=8) { \
	    *--(TO) = *--(FROM); *--(TO) = *--(FROM); \
	    *--(TO) = *--(FROM); *--(TO) = *--(FROM); \
	    *--(TO) = *--(FROM); *--(TO) = *--(FROM); \
	    *--(TO) = *--(FROM); *--(TO) = *--(FROM); \
	    (N) -= 8; \
	}; \
	switch((N)%8) { \
	    case 7: *--(TO) = *--(FROM); \
	    case 6: *--(TO) = *--(FROM); \
	    case 5: *--(TO) = *--(FROM); \
	    case 4: *--(TO) = *--(FROM); \
	    case 3: *--(TO) = *--(FROM); \
	    case 2: *--(TO) = *--(FROM); \
	    case 1: *--(TO) = *--(FROM); \
	}; \
    }
#else
#define	SLAMRIGHT(TO,FROM,N) \
    switch((N)%8) { \
	do { \
	case 0: *--(TO) = *--(FROM); \
	case 7: *--(TO) = *--(FROM); \
	case 6: *--(TO) = *--(FROM); \
	case 5: *--(TO) = *--(FROM); \
	case 4: *--(TO) = *--(FROM); \
	case 3: *--(TO) = *--(FROM); \
	case 2: *--(TO) = *--(FROM); \
	case 1: *--(TO) = *--(FROM); \
	} while (((N)-=8) > 0); \
    }
#endif


/* In support of checkpointing and diagnostics,
** we amplify file descriptors with associated names and byte counts.
** Names of temp files do not persist, so must be regenerated
** (from indx) if the name is to be used in an unknown context.
**
** File names passed as arguments will have a negative indx,
** with earlier arguments more negative than later ones.
*/

typedef struct {
    verylong	posn;		/* cleario(0), extend_batch, writeptrlen */
    long	indx;		/* Tempfile index, if non-negative */
    int		fd;		/* File descriptor */
    char	*name;		/* File name */
} ios;


/* When stdio-style record-at-a-time i/o was replaced by read/write/mmap i/o,
** the ``batch'' structure was defined to capture important state information.
** Its contents are as follows:
**
** com
** Just a means of getting to the ``global data'' starting from any batch.
**
** batch
** The ``record space'' available for use after a call to extend_batch.
** The next call will attempt to extend beyond where the last left off,
** so batch must not be changed between calls.  The batch may not start
** at the very beginning of bufp.  In particular, mapped data are
** contrained to be aligned on page boundaries.
**
** batcho
** The offset of the first byte in the batch from the start of the file.
** We need this for memory map positioning, diagnostics, and restarts.
**
** thiso
** Offset of ``this record'' when we get in trouble.  Sometimes this can
** be caluculated (in which case it starts as -1), but sometimes the
** notion of a current record is just too obscure, and it is coerced to
** a meaningful value.
*/


/* fsize
** The total length of the corresponding file, if known.
** (BATCH_SIZ will be on in flag when the size is known).
**
** bufp
** The area of memory, mapped or real, ``near the start of'' the batch.
** Memory mapped space is constrained to page boundaries, so bufp is always
** page-aligned for mapped files.  We even align it a for ordinary files,
** in hopes of making the copy from kernel space as efficient as possible.
**
** auxbp
** In the merge phase, dynamically allocated memory is carved up into
** buffers before all merge inputs may have been opened.  Many of the
** merge inputs (all of them, in most cases), may be ordinary files
** suitable for memory mapping, in which case ``buffers'' come from
** ``memory map space'' rather than the preallocated space.  But we
** could happen along a merge input (like standard in) that cannot be
** memory mapped, so we can't lose track of the preallocated buffers.
** We keep a permanent record in auxbp.
**
** plast
** For newline and header-delimited formats, where records and keys are
** identified by ptrlen structures, plast is the top of a stack of such
** structures, available for use in parsing the records in the batch.
** In the sort phase, this stack grows up from the end of dynamically
** allocated space.  In the merge phase, each buffer area addressed by
** auxbp above has its own ptrlen stack growing from the back of the area.
** This is true even for memory mapped files, where records and ptrlens
** occupy quite different memory areas.
*/


/* aunit
** The ``alignment unit'' for i/o.  Memory mapped i/o is aligned on
** page boundaries.  Even normal i/o is aligned, to make copying data
** as efficient as possible (although it is unclear whether the kernel
** takes advantage of this when copying from kernel space to user space).
**
** tunit
** The ``transfer unit'' for i/o.  This must be at least a page for
** memory mapped i/o.  Ordinary i/o is done in multiples of this size,
** so some file-system efficient value should be chosen.
**
** maxio
** Originally:
**   For ``direct io'', the maximum transfer size.
**   We will confirm that the default aunit and regular file tunit
**   are compatible with the minimum transfer size and alignment unit,
**   but will do no heroic efforts to adjust them if they are not.
**   When arming DIRECTIO, it may be necessary to modify
**   DEFAULT_AUNIT and REGFILE_TUNIT to comply.
** Now a general upper bound on read size.  See commentary that follows.
*/


/* The way this program has evolved SHOULDN'T matter, but several bugs
** that trace back to its early stages have already cropped up,
** so a bit of history may be helpful to someone hunting down new bugs.
**
** The sort phase has always had a general model of reading records
** into one end of an arena, while summary structures addressing those
** records grew backwards from the other end.  When everthing was stdio
** based, this was pretty much how things worked, and it was hard to
** get wrong.  Except it was horridly slow, so reads of records one
** at a time using stdio turned into reads of blocks of data.
** The upside is a speedup in the reading and parsing of the records.  
** Bigger reads mean fewer system calls, and per-read parsing
** overhead gets amortized over more records.  The downside is that,
** except for fixed-length records, you don't know how many summary
** structures will be needed to address the records until after they
** have been read and parsed.  So it is generally necessary to
** guess at how many summaries will be needed, read a bunch of
** records, and start parsing them using the preallocated summaries.
**
** Allocating too many summaries tends to be harmless.  We can parse
** ALL the records read, (with the possibility of a fractional record
** left over at the end).  As long as we get even one complete record,
** ``merge-style'' reading (which also applies to the -c check option)
** can make progress.  In the sort phase, or when merge-style
** reading encounters a record so large that it wasn't complete in the
** space allocated, we can usually ``return'' some of the excess
** summaries to make room for another read to continue filling memory
** (in the sort phase) or complete the honker record (in the merge phase).
** The possibility of a record so large that it simply won't fit is remote,
** but must be handled with greater grace than a coredump.
**
** Allocating too few summaries has led to many more problems.
** For example, detection of the end of file may be logically and
** temporally remote from completing the batch of records read.
** Flag BATCH_ALL records the fact that no more data will be forthcoming
** from the file itself, but there may be a generous supply of records
** already in memory, waiting for enough summaries to be parsed.
** Unfortunately, the worst-case ratio of summary structures to
** record data is pretty grim.  For example, the 2 ptrlen summary
** structures for header-delimited records consume at least 16 bytes,
** whereas the record itself may occupy as few as 2.  We'd have to
** allocate 8 times as much storage for summaries as for data to
** guarantee that we had enough space for summaries.  This would
** lead to very poor storage utilization for non-extreme cases.
** So we are more or less forced to deal with the possibility of
** underallocating summaries.
**
** The original logic of allocate/read/parse did NOT do an
** acceptable job.  This showed up in two nasty bugs.  One was
** encountering a long string of empty lines in a newline-delimited
** sort, inevitable in the merge phase if lots of empty lines are
** present in the input, since the sort brings them together.
** Allocation based on average record length led to far too few
** summaries to even parse a block's-worth of records, so the
** sort choked on the next allocation cycle.  A similar problem
** arose in the sort phase, with many short records.  Although
** avaliable summaries were adequate to parse a block-worth of
** (very short records), the subsequent read filled the freed
** space with many more records, and eventually there was no room
** for summaries.  The solution is to parse everything remaining
** BEFORE reading more.  In effect, the logic is now
** parse/allocate/read/parse.
**
** Although the parse-before-reading logic prevents the sort phase
** from choking, over-aggressive reading could cause poor performance.
** For example, if we nearly fill memory with what turns out to tiny
** records, we can only sort and write a few at a time as we free up
** enough space to allocate summaries and parse records.  This would
** create many more temporary files than desirable.  We can limit the
** potential for damage by limiting the maximum read size in the sort
** phase, in effect, allowing for worst-case behavior on any given read.
** If we actually see worst-case behavior, we have done the best we could have.
** If we don't, the limit isn't so severe as to do much damage to performance.
*/


/* left
** The number of bytes at the end of the area addresses by the batch ptrlen
** that have not been parsed into records.  This often is the start of a
** single, incomplete, record, but it may include many records if space
** for summary structures ran out before the batch was parsed into records.
**
** parsed
** The number of bytes in the batch that belong to complete, parsed, records.
** The batch always starts with a complete record, so the first ``parsed''
** bytes of the batch are complete records (followed by ``left'' bytes
** of incomplete records, starting, however, at a record boundary).
**
** scanned
** The number of bytes among those represented by ``left'' that have been
** scanned for newlines or a header (fixed length records need no scanning).
** When the leftovers are copied to the start of the buffer area,
** we need not rescan these bytes.  Often, scanned == left, when the
** buffer is exhausted in mid-record.  On header-delimited records,
** scanned is the number of bytes of header, complete or incomplete,
** already seen.
*/


/* tbkr
** In the merge phase, to preserve stability, ties among input records
** are broken by the order of the files from which the records were read.
** The indx field determines this tiebreaking order, but the sense of
** tiebreaker also depends on the -r flag.  To avoid repeated tests of
** this flag, the tbkr field is set to indx or -indx, as determined by
** the -r flag, so a comparison of the values will break ties correctly.
*/


/* grecz
** gklen
** gkoff
** For header-delimited records, the encoded header values are stored
** in these fields while the record is being parsed.  grecz also serves
** as a state variable, being -1 until a complete header has been scanned.
*/


/* sumx
** sumc
** couldbe
** sumi
** Except for fixed length records, we don't know in advance how many
** summary structures will be needed while parsing a bufferload of raw data.
** We make an (educated) guess, based on average record length so far,
** and allocate sumx summaries.  The number of those summaries used to
** address the parsed records is maintained in sumc.  If this number
** was known (by the parse routines) to be adequate to summarize all
** the complete records in the buffer, couldbe will be set to 0.
** If there might be a complete record in the buffer, couldbe will be 1.
** If we guess too high on the need for summary structures, we may be
** able to return some of the summary space to make room for additional
** record space.  In the merge phase, sumi is used to keep track of the
** ``current'' record in the batch.  It starts at -1 and is preincremented
** in readmerge, so it remains less than sumc when valid.
*/


/* basep
** curp
** In the merge phase, we sometimes do ``random access'' on the records
** in a single merge input batch.  Pointer basep addresses an array of
** somethings, records for fixed-length format, summary structures for
** other formats, and the (possibly negative) size of these somethings
** is kept in field gstep in the glob (global data) structure.
** curp is set to the I-th element of the array by the SETCURP macro,
** which follows.
*/

#define	SETCURP(BP,I) \
	((BP)->curp = (gptr)((char *)((BP)->basep) + ((I) * (BP)->com->gstep)))


/* iofile
** flag
** Basic information about an i/o file is kept in the iofile structure
** (see the ios structure) and the flag field (see the BATCH_ flags).
*/


typedef struct batch {
    struct glob	*com;
    ptrlen	batch;
    verylong	batcho;
    verylong	thiso;
    verylong	fsize;
    uch *	bufp;
    uch *	auxbp;
    ptrlen *	plast;
    long	total;
    long	aunit;
    long	tunit;
    long	maxio;
    long	left;
    long	parsed;
    long	scanned;
    long        tbkr;
    long	grecz;
    long	gklen;
    long	gkoff;
    long	sumx;
    long	sumc;
    long	couldbe;
    long	sumi;
    gptr	basep;
    gptr	curp;
    ios		iofile;
    int		flag;
} batch;


/* ``Global'' information.
**
** This once was a collection of global variables, but, for general
** cleanliness, it was changed to a structure whose address is passed,
** either explicitly, or through the com pointer in the batch structure.
** The comparison routines ``cheat'', since the original qsort-like
** interface provides no hook for passing through a pointer
** (besides which, we want to minimize overhead on the compare routines,
** the busiest routines in the application).
*/

/* nbyte
** nrecs
** brecs
** avgrz
** The number of bytes (and number of records) of non-temporary input
** that have been read.  Useful for diagnostics, restart and for
** calculating avgrz, average record size.  In the sort phase, brecs is
** the number of records in the current input file batch.
*/


/* prohd
** ptohd
** gstep
** prohd is the per-record overhead.  It varies with format, and phase.
** In the sort phase, there are two pointers per record in addition
** to whatever ptrlen structure overhead applies (this being stored in
** the ptohd field).  In the merge phase, only the ptrlen structure
** overhead applies.  The gstep field is used in the merge phase to
** index the array of somethings (see the basep and curp fields in the
** batch structure above).  It is the record size for fixed-length records,
** and negative ptohd for the others (which grow backwards).
*/


/* maxrz
** minrz
** grecz
** gklen
** gkoff
** maxio
** maxrz is the size of the largest input record seen so far
** (or value from the -z option, whichever is greater).
** minrz is the smallest record of the given format that could be observed.
** The grecz, gklen and gkoff hold the global record length,
** key length, and key offset from the -f option for fixed-length records,
** and are used to store the corresponding values from the header
** while parsing header-delimited records.  If a global key length
** or key offset are specified for newline-delimited records,
** the values are stored in these fields.
** maxio is an upper limit on read size.
*/


/* memax
** dsize
** drecs
** dleft
** The amount of main memory from the -y option (or the default),
** is stored in memax, in part for use in reconstructing the
** original arguments when restarting.  It may not have been possible
** to acquire that much memory when initializing for the sort.
** Some shortfall may be tolerated, and the actual amount of
** dynamic storage reserved is stored in dsize.  The number of
** bytes of that dynamic storage used for records (only) by the
** current batch in the sort phase is stored in drecs.  This will
** be 0 for memory mapped batches, since the records don't occupy
** space reserved for dynamic storage.  The dleft field starts at dsize,
** and is reduced for the space occupied by records (mapped or not),
** and summary structures (which always come out of dynamically
** allocated space) for the current batch.  It confines unmapped
** i/o to the dynamically allocated space, and prevents the working
** set from getting too large.
*/


/* btchi
** atbta
** inbta
** atbtp
** atbtq
** btcha
** btchb
** btchp
** btchq
** btcht
** obtch
** There is at most one output file open at any time, and it is associated
** with obtch.  The real action is with input files, of which there will
** be many open at any given time in the merge phase.  When memory mapping
** is being done, there may be multiple files open in the sort phase, too.
** All open activity is done by routine openin(), employing the (fixed)
** array of batch structures at btcha.  There is no point in making the
** dimension of btcha greater than the number of files that can be open
** simultaneously.  It may make sense to make the dimension smaller.
** In particular, each open file should have enough buffer space to
** keep i/o efficient, whatever that means.  In general, the next
** available batch structure is indexed by btchi.  At the start of the
** merge phase, in the process of discovering how many file descriptors
** really ARE available, we may open more inputs than we want to consume
** on the first merge step.  The total number of such open, unmerged files
** is maintained in atbta, and the index of the next such file is in inbta.
** During the merge phase, it is more convenient to swap pointers to batch
** structures rather than the (bulky) batches themselves.  btchb is an
** array of pointers large enough to ``float'' pointers to all the structures
** at btcha.  In general, there are two subarrays in the btchb array,
** one (of size atbtp) at btchp, of unordered merge inputs, another
** (of size atbtq) at btchq of ordered merge inputs.  btcht is just a
** convenience pointer to the top of the btchb array, used to prevent
** the btchq array from straying out of limits.
*/


/* nexti
** nexto
** lastg
** argnd
** Input files are identified by index number.  Negative indexes refer
** to the files named as arguments (relative to argument pointer argnd).
** The next input file is indexed by nexti, the next output file by nexto.
** The number of inputs (starting at nexti) that must be merged to
** finish the current ``generation'' of inputs is maintained in lastg.
*/


typedef struct glob {
    verylong	nbyte;		/* *parse */
    verylong	nrecs;		/* *parse */
    long	brecs;		/* *parse */
    long	avgrz;		/* doargs, *parse */
    long	tunit;
    long	prohd;		/* sort */
    long	ptohd;		/* doargs */
    long	gstep;		/* doargs */
    long	maxrz;		/* doargs, *parse */
    long	minrz;		/* doargs */
    long	grecz;		/* doformat, doargs(-1 for hdr) */
    long	gklen;		/* doformat */
    long	gkoff;		/* doformat */
    long	maxio;		/* arenalloc */
    long	memax;		/* globinit, doargs */
    long	dsize;		/* arenalloc */
    long	drecs;		/* arenareset(0), includebatch */
    long	dleft;		/* arenareset(dsize), includebatch */
    long	btchi;		/* 0, openin, includebatch */
    long	atbta;		/* # of files opened but not used */
    long	inbta;		/* index of next unused open file */
    long	atbtp;		/* how many batches at btchp */
    long	atbtq;		/* how many batches at btchq */
    long	nexti;		/* doargs, sort */
    long	nexto;		/* 0, opennewtmp */
    long	lastg;		/* merge XXX */
    long	focpy;		/* 0, finishfinalout */
    long	clean;		/* dopolicy */
    long	mxrun;		/* max # runs for merge algorithm */
    long	fanin;		/* for reporting */
    long	fanmx;		/* for reporting */
    uch		obuff[OBUFSIZE];		/* long-aligned, 0 */

    uch *	dynsp;		/* arenalloc */
    gptr *	m1ptr;		/* prepsaptrs */
    gptr *	m2ptr;		/* prepsaptrs */

    ptrlen *	plast;		/* arenalloc */
    ptrlen *	plcur;		/* arenareset(gp->plast), includebatch */

    verylong	ckoff;		/* offset in first file  */
    long	cknot;		/* turn off checkpointing */
    char *	ckfmt;		/* original format argument */
    char *	ckmem;		/* original memory (-y) argument */
    char *	ckpol;		/* original policy (-p) argument */
    char *	ckopt;		/* checkpoint options (-k) */

    int		fmtyp;		/* globinit, doformat */
    int		revrs;		/* 0, doargs */
    int		uniqu;		/* 0, doargs */
    int		merge;		/* 0, doargs, mergephase */
    int		nomrg;		/* main, sort */
    int		justs;		/* just sort, don't merge */
    int		repti;		/* report summary info at exit */
    int		igerr;		/* ignore close errors */
    int		mpass;		/* number of merge passes (for reporting) */
    int		srtng;
    int		strip;		/* 0, doargs */
    int		check;		/* 0, doargs */
    int		safst;		/* globinit, doargs, main(ckpt), openin */
    int		cptyp;		/* setcopytype */

    int		ecode;		/* 0, diag, check */
    int		abetz;		/* globinit */
    int		order;		/* insert, merge */

    char*const*	argnd;		/* doargs */
    char *	whomi;		/* main */
    char *	tname;		/* dotempdir */
    char *	tnam2;		/* dotempdir */
    char *	tbase;		/* dotempdir */
    char *	dlist;		/* dotempdir, doargs */
    char *	dname;		/* 0, doargs */
    char *	final;		/* 0, doargs */

    uch *	obufp;		/* globinit, writeptrlen, closeio */

    long 	(*parsf)(batch *bp);		 	/* doargs */
    void	(*writf)(struct glob *gp, gptr desc);	/* doargs, beginfinalout */
    void	(*strpf)(struct glob *gp, gptr desc);	/* doargs */
    int		(*cmprf)(gptr a, gptr b);		/* doargs */

    filestatus	finst;				/* doargs */
    FILE *	ckiop;				/* openckpt, closeckpt */
    batch	obtch;				/* opennewtmp, openfinalout */
    batch	btcha[MAXBATCH];		/* openin */
    batch *	btchb[BIGBATCH];		/* globinit */
    batch **	btchp;				/* globinit */
    batch **	btchq;				/* globinit */
    batch **	btcht;				/* globinit */
    char	emesg[200];			/* 0 */
    char	recon[100];			/* 0 */
    char	bname[BASETOTALLEN + 1];	/* dotempdir */
} glob;

glob	G;

#define	PARSEB(BP)	(*((BP)->com->parsf))(BP)
#define	WRITEG(GP, D)	(*((GP)->writf))(GP, (gptr) D)
#define CMPRG(GP,G1,G2)	(*((GP)->cmprf))(G1,G2)

enum {
    BATCH_ALL = 0x01,		/* All bytes have been read/mapped */
    BATCH_EOF = 0x02,		/* All bytes have been parsed */
    BATCH_CLS = 0x04,		/* File has been closed */
    BATCH_SIZ = 0x08,		/* File size is known */
    BATCH_OUT = 0x10,		/* Batch is being used for output */
    BATCH_MAP = 0x20,		/* Use memory mapping */
    BATCH_DIO = 0x40		/* Direct I/O - not always available */
};


/* Subroutine declarations
**
** Drop everything here, so order of definitions is not important.
*/

#ifdef DOSLIKE
extern int mkdir(char const *);
#define	MKDIR(D)	mkdir(D)
#else
extern int mkdir(char const *, mode_t);
#define	MKDIR(D)	mkdir(D, 02770)
#endif

extern int rmdir(char const *);
#define	SLASH	('/')

static	long	strtolk(const char *str, char **ptr, int base);
static	void	diag(glob *gp, int die);
#define	fatal(G)	diag(G, 1)
static	void	arenalloc(glob *gp);
static	void	arenareset(glob *gp);
static	void	parserror(batch *bp, uch *p);
static	void	parseheader(batch *bp, uch *hdr);
static	void	setcopytype(glob *gp, int strip);
static	void	writefatal(glob *gp, size_t want, size_t got);
static	void	writeptrlen(glob *gp, ptrlen *ptr);
static	void	writefix(glob *gp, gptr p);
static	void	stripfix(glob *gp, gptr p);
static	void	writehdr(glob *gp, gptr p);
static	void	striphdr(glob *gp, gptr p);
static	void	trimhdr(glob *gp, gptr p);
static	void	writenew(glob *gp, gptr p);
static	void	stripnew(glob *gp, gptr p);
static	void	loadM1(glob *gp, uch *start, long len, int reverse);
static	void	prepsaptrs(glob *gp);
static	int	fwdfix(gptr a, gptr b);
static	int	fwdfixoff(gptr a, gptr b);
static	int	fwdhdrnew(gptr a, gptr b);
static	int	dynprep(glob *gp, int (*cmp)(gptr, gptr));
static	void	undupe(glob *gp, int (*cmp)(gptr, gptr));
static	gptr *	bmerge(glob *gp, int(*cmp)(gptr, gptr));
static	void	closeio(batch *bp);
static	long	encode(glob *gp, long num, int wid, char *endp);
static	void	dotempdir(glob *gp);
static	void	settmpname(batch *bp);
static	void	setoffname(batch *bp);
static	void	summarize(glob *gp);
static	void	cleario(batch *bp);
static	int	closeit(batch *bp);
static	int	openin(glob *gp, int sort);
static	void	openfinalout(glob *gp);
static	void	beginfinalout(glob *gp, int tmpopen);
static	void	finishfinalout(glob *gp);
static	void	copyfile(glob *gp, char *from, char *to);
static	void	opennewtemp(glob *gp);
static	void	docleanup(glob *gp);
static	gptr *	reversebatch(glob *gp, gptr *p);
static	void	sortbatch(glob *gp);
static	batch *	newbatch(glob *gp);
static	void	batchreset(batch *bp);
static	void	includebatch(batch *bp);
static	void	sort(glob *gp);
static	void	striperr(glob *gp);
static	int	doformat(glob *gp, char *p);
static	void	doargs(glob *gp, int argc, char *const *argv);
static	void	undoargs(glob *gp);
static	void	addfiles(glob *gp);
static	int	dopolicy(glob *gp, char *p);
static	void	setckstate(glob *gp);
static	void	getckstate(glob *gp);
static	void	openckpt(glob *gp);
static	void	closeckpt(glob *gp);
static	void	ckptsort(glob *gp);
static	void	ckptmerge(glob *gp);
static	void	ckptcheck(glob *gp, ios *io);
static	void	insert(glob *gp, int knownhi);
static	long	mergeopens(glob *gp, long n);
static	void	mustopens(glob *gp, long n);
static	void	makeopen(glob *gp, long n);
static	void	mergeclean(glob *gp, long n);
static	int	readmerge(batch *bp);
static	void	merge(glob *gp);
static	long	fiddlimit(long limit, long inputs);
static	void	mergephase(glob *gp);
static	void	copyrec(batch *top, batch *fromp);
static	void	copyref(batch *top, batch *fromp);
static	void	globinit(glob *gp);
static	gptr *	frsort(glob *gp);
static	gptr *	vrsort(glob *gp);
static	void	extend_batch(batch *bp);
static	long	carve(batch *bp, long avail);
static	long	fixparse(batch *bp);
static	long	newparse(batch *bp);
static	long	hdrparse(batch *bp);


/* Flags associated with checkpoint/restart
** and cleaning up the temporary directory.
**
** We want to provide a measure of control over whether checkpointing
** is done, and how thoroughly the temporary directory is cleaned.
** XXX Until checkpointing is repaired or removed, default to ALWAYS.
*/

#define	DEFAULTCLEAN	-1
#define	ALWAYSCLEAN	0
#define	NEVERCLEAN	1


/* When we reconstruct the command for checkpoint/restart,
** some arguments are easily reconstructed from flags and values,
** while others are more easily copied from the original arguments.
** Those that we want to be able to copy will have Orig* pointers
** set by doargs.  We save the format spec, because it is ugly
** to reconstruct.  We save gp->memax because we may have to tamper
** with it, and would prefer to restart from the original value.
*/


#ifdef	HUGE
static char *
verylongtoa(verylong n)
{
    static char numbuf[50];
    char *p = numbuf + sizeof(numbuf);
    int i, neg;

    if (neg = (n < 0)) n = -n;

    do {
	i = n % 10;
	n /= 10;
	*--p = "0123456789"[i];
    } while (n > 0);
    if (neg) *--p = '-';
    return (p);
}
#endif


/* Sometimes it's nice to be able to say 16k instead of 16348,
** or 8M instead of 8388608.  Accept k(1024) and m(1024*1024),
** (b would be ambiguous in a hex specification, and isn't
** much used any more since 512-byte blocks are pathetically small),
** independent of case.
*/

static long
strtolk(const char *str, char **ptr, int base)
{
    char *p;

    long val = strtol(str, &p, base);
    switch (*p) {
	case 'k': case 'K':
	    val *= 1024;
	    p++;
	    break;
	case 'm': case 'M':
	    val *= 1024 *1024;
	    p++;
	    break;
    }
    if (ptr) *ptr = p;
    return (val);
}


/* Generic error reporting
**
** A descriptive message should be in emesg.
** When file-oriented problems are involved,
** offset in file, and file name
** may be included as part of that message.
*/

#ifdef	HUGE
static char OffNameFmt[] = "offset %s, file ``%.50s''";
#else
static char OffNameFmt[] = "offset %ld, file ``%.50s''";
/*                          1234567   890123456     78
** 18 + (<30)-digit number + up to 50 bytes of name  => 100 is adequate
*/
#endif

static void
diag(glob *gp, int die)
{
    fprintf(stderr, "%s: ", gp->whomi);
    if (errno) perror(gp->emesg);
    else fprintf(stderr, "%s\n", gp->emesg);
    if (!die) return;
    gp->ecode = 1;
    docleanup(gp);
    exit(gp->ecode);
    /* NOTREACHED */
}


/* Main memory in the sort phase.
**
** During the sort phase, main memory is treated as one huge ``arena''
** that is subdivided into other areas depending on the record types.
** In all cases, records are read into the start of the arena,
** working their way towards the end of the arena, as is ``natural''
** and convenient.
**
** And, in all cases, two arrays of pointers, whose dimension will
** (each) be the same as the number of records read, will be
** reserved following the records, as arguments to merge sort.
** What these pointers point TO, and what, if anything, follows them
** depends on the record type.  To understand how these pointers
** are ultimately initialized, it is necessary to consider ``stability''
** and the option to sort in ``reverse order''.
**
** Merge sort is ``stable'', which means, among other things, that the
** order of elements that compare equal is preserved.  To support an
** option to sort in reverse order, we choose to reverse the order in
** which equal elements are written, so the effect is identical to
** sorting ``normally'', then walking through the sorted output from
** the end to the beginning, which we will accomplish in the sort phase
** by reversing the pointers after the sort has taken place.
**
** For fixed length records, the pointers will point to the records
** themselves, and no addition structures are necessary.  Since the
** size of the records and record pointers are known in advance,
** the largest number of records that could fit in a given arena
** can be precomputed, and the arena can be preallocated for
** records and the pointer arrays with no loss in space efficiency.
**
** For newline-delimited records, we chose to allocate a pointer/length
** structure for each record, growing from the end of the arena towards
** the beginning.  These summaries are filled in as records are read,
** so the summaries appear in ``reverse order'' from the record input,
** since the summary array grows backwards.  Space for the pointer arrays
** is ``reserved'', but not initialized, since the actual location of the
** arrays will not be known until a variable number of variable-length
** records have been read.  When there is not enough space left to
** complete reading a record, the pointer arrays are allocated in the
** reserved ``middle ground'', pointing to the array of summary
** structures.
**
** Header-delimited records are similar to newline-delimited records,
** but the summary structures contain 2 pointer/length pairs,
** one for keys, one for records.  The reservation and initialization
** of the pointer arrays is entirely analogous.
**
** Fancy newline-delimited records, where the key is not the same as
** the entire record, are summarized like header-delimited records,
** with a key descriptor followed by a record descriptor.
*/


/* A few options that this version of sort WILL support.
**
** gp->revrs is non-zero if we are doing reverse comparisons.
** To ``adjust'' the sense of a comparison for normal or reverse order,
** unconditionally multiplying by an int that was 1 for normal
** and -1 for reverse generated WORSE code than the conditional below.
** But make it a macro, so it is easy to change.
**
** gp->uniqu is non-zero if duplicate sort keys should be suppressed
** on (or before) final output.  The first record with a given key
** is easy and logical to distinguish, but the last key could also
** be made the survivor, perhaps in conjunction with the gp->revrs option.
** We'll worry about that later.
**
** Merge means skip the sort phase, assume all inputs are already sorted.
**
** gp->strip is used to postprocess records on the final output pass.
** The effect is format-dependent:
**     For fixed-length records and newline-delimited records,
**         only the bytes following the key are written.
**         This also applies to header-delimited records, if gp->strip > 1.
**     For header-delimited records, and gp->strip == 1, the header
**         will be stripped, but key and data will be written.
**
** Check causes the order of input to be checked.
** Any disorder will be reported (on standard error),
** and cause the command to exit with non-zero exit code, gp->ecode.
*/

#define	REVERSENSE(GP, R)	((GP)->revrs ? (-(R)) : (R))


/* When the output file is made explicit (via the -o option),
** we want to be careful if it is also among the input files.
** At minimum, it would be a disaster to open and truncate the output
** if it was still in use as an input.  More generally, it seems
** unwise to overwrite an input, even if we are done with it,
** in case the machine crashes part way through the overwrite.
** It is fundamentally risky to overwrite an input, but we can
** help reduce the risk by doing a rename instead of a copy,
** when that works.
**
** Job 1 is to recognize when the output file is among the inputs.
** To that end, we'll keep track of device and inode in POSIX worlds,
** and remember what we know in all worlds.  The ``safety status''
** begins as safe, while output defaults to standard out.
** It may change if -o redirects output to a file that already exists.
** While status is uncertain, it will be 0.  Once established,
** it will turn non-zero (see SAFEOUT_ defines), and stay there.
*/

#define	SAFEOUT_SAFE (1)
#define	SAFEOUT_UNKNOWN (0)
#define	SAFEOUT_UNSAFE (-1)


/* Format-dependent routines for allocating and initializing the arena.
**
** gp->memax is the maximum amount of memory that can be allocated.
** It may be reduced some if we cannot get all we need,
** but will never be increased.
**
** We allocate first, and divide up later, since there's no point
** in dividing things up precisely if we can't get that much memory anyway.
*/

#ifndef	DEFMEM
#define	DEFMEM	(4L * 1024L * 1024L)
#endif

/* In the event that the default value of MINMEM isn't perfectly clear,
** see the discussion about maxio.  In brief, the +1L accounts for a
** byte of record, and the 2L*sizeof term accounts for worst-case sort
** phase overhead for a 1-byte record.  Multiply by gp->tunit
** to get the requirements for a block of 1-byte records and the
** associated overhead.  Multiply by 2 because we may have one block
** from leftovers, and one from a new read.  For 4-byte pointers and
** lengths, and a 4K TUNIT, this works out to 104,800 bytes,
** a perfectly reasonable lower bound on space.
*/

#define	MINMEM(gp) (2L*((2L*(sizeof(ptrlen)+sizeof(gptr))+1L)*(gp)->tunit))

/* We would have sign problems on more than 31-bit's worth */

#ifndef MAXMEM
#define	MAXMEM	(0x7fffffff)
#endif


/* Allocate the initial arena.
**
** Try for gp->memax, but if we cannot get it, keep backing off to 90%
** of the previous request until we either get what we asked for,
** or have dropped below the threshhold of even bothering to press on.
*/

static void
arenalloc(glob *gp)
{
    long n, o;
    void *p;

    n = gp->memax;
    for (;;) {
	n = ROUNDDOWN(n, sizeof(ptrlen));
        if (((p = malloc((size_t) n)) != NULL) || (n <= MINMEM(gp))) break;
	n = (n / 10) * 9;
    }
    if (p == NULL) {
	errno = ENOMEM;
	sprintf(gp->emesg,
	    "Out of space allocating %ld, max %ld", n, gp->memax);
	fatal(gp);
    }
    /* align p to AUNIT boundary */
    if (o = ((uch *)p - (uch *)0) % DEFAULT_AUNIT) {
	o = DEFAULT_AUNIT - o;
	p = ((uch *)p) + o;
	n -= o;
    }
    gp->dynsp = p;
    gp->dsize = n;
    gp->plast = (ptrlen *)(((uch *)p) + n);
    /* The maximum number of records that could possibly fit,
    ** along with their summaries, in the space allowed,
    ** would be observed for records of minimal size.
    ** The total per-record space consumed is the (minimal) record size,
    ** plus the per-record parsing structure overhead,
    ** plus two pointers for the sort phase arrays.
    */
    o = n / (gp->minrz + gp->ptohd + 2 * sizeof(uch *));
    /* Figure how many transfer units that many minimal-sized records
    ** would occupy.  For variable-length formats, we only want to allow
    ** half that many units per read, in case we have half that many left
    ** over unparsed from the previous batch, and we append that many more
    ** to their end before parsing and discovering the tiny records.
    */
    o *= gp->minrz;
    if (gp->fmtyp != FMTFIX) o /= 2;
    if ((o = ROUNDDOWN(o, gp->tunit)) <= 0) o = gp->tunit;
    gp->maxio = o;
    return;
}


/* Arena reset
**
*/

static void
arenareset(glob *gp)
{
    gp->brecs = gp->drecs = 0;
    gp->dleft = gp->dsize;
    gp->plcur = gp->plast;
    return;
}


/* Record I/O
**
** There are many possible program architectures for the sort phase.
** One can imagine hiding the arena-management differences within
** the record reading subroutines, for example.  But, if we look past
** the sort phase to the merge phase, the use of memory changes a lot,
** but the need to read different types of records does not.
** So we build the program around low-level read routines that will
** be useful in both the sort and merge phases.
**
** The model for record reading will be as follows.  The read routine
** will be passed a ptrlen describing an area available for reading into.
** In the sort phase, that will be the unreserved area of the arena.
** In the merge phase, it will be a buffer area of some size.
** The job of the read routine will be to read an entire record
** into the specified area.  If that can be successfully done,
** the routine will set Recptrlen and Keyptrlen to the record just read
** and the key thereof, and some record-type-specific data for later use,
** and return +1.
**
** If the record could not fit, a negative number (-1) is returned.
** In this case, private data that can be used to identify the
** part of the record already read will be recorded before returning.
** For its part, the calling context will preserve the area
** originally passed to the read routine, so the partial record
** will not be damaged.
**
** If end of file is reached, the routine will return 0.
**
** The read routines may be called with 0 or negative space available,
** but this should happen only after a complete read, never after a partial.
** They should respond with the usual ``out of space'' return.
**
** After a negative (``won't fit'') return (and perhaps a great deal
** of processing that doesn't involve the original area),
** the next read call may offer a different, and larger,
** area into which the record can be read.  It is the responsibility
** of the read routines to keep track of the won't-fit condition,
** to copy any partially read information from the old area into the new,
** and to attempt to complete the read.
**
** After a won't-fit condition, it will be the responsibility of the
** calling context to provide a more promising area, or to exit.
** We can assume that the new area doesn't overlap the previous,
** without much loss of generality.  In the sort phase, a record
** that takes up more than half the entire arena is pretty certain
** to cause problems when we enter the merge phase, so bailing out
** is fair play.
*/


/* Separator and terminator characters for header-delimited records */

#define	SEP	(':')
#define	TERM	(';')

/* Newline (so as to be easily redefinable) */

#define	NEWLINE	('\n')


/* Error in header */

static void
parserror(batch *bp, uch *p)
{
    bp->thiso = bp->batcho + (p - bp->batch.ptr);
    setoffname(bp);
    errno = EIO;
    sprintf(bp->com->emesg, "Invalid header starting ``%.20s'' at %s",
			p, bp->com->recon);
    fatal(bp->com);
}


/* Parse a (complete) header starting at p.
** The header is known to be terminated with the TERM character.
** Insist on a record length, and accept, or assign default values to,
** a key length and key offset.  Parse or logic errors are not tolerated.
*/

#define	SKIPWHITE(P)	while (isspace(*P)) P++

static void
parseheader(batch *bp, uch *hdr)
{
    uch *p;
    char *stop;
    int  saverr;

    errno = 0;
    bp->gkoff = 0;
    bp->grecz = bp->gklen = strtol((char *)hdr, &stop, 0);
    saverr = errno;
    p = (uch *) stop; SKIPWHITE(p);
    if (*p == SEP) {
	bp->gklen = strtol((char *)(p+1), &stop, 0);
	saverr |= errno;
	p = (uch *) stop; SKIPWHITE(p);
	if (*p == SEP) {
	    bp->gkoff = strtol((char *)(p+1), &stop, 0);
	    saverr |= errno;
	    p = (uch *) stop; SKIPWHITE(p);
	}
    }
    /* Insist that everything be sensible */
    if ((*p != TERM)  ||		/* junk in header */
	(saverr != 0) ||		/* strtol had a problem */
	(bp->grecz < 0)  ||		/* negative record length */
	(bp->gklen < 0)  ||		/* negative key length */
	(bp->gkoff < 0)  ||		/* negative key offset */
	((bp->gkoff + bp->gklen) > bp->grecz))	/* key outside of record */
	    parserror(bp, hdr);
    return;
}


static void
setcopytype(glob *gp, int strip)
{
    long size;

    switch (gp->fmtyp) {
    case FMTFIX:
	size = gp->grecz;
	if ((size % sizeof(long)) == 0) {	/* records long-aligned */
	    /* If stripping, what remains must also be long-sized */
	    if ((!strip) ||
		(((size -= (gp->gkoff + gp->gklen)) % sizeof(long)) == 0)) {
	        gp->cptyp = (size > LONG_THRESH) ? COPY_MEMCPY : COPY_LONGS;
		return;
	    }
	}
	break;
    default:
	if ((size = gp->avgrz) <= 0) {
	    gp->cptyp = COPY_MEMCPY;
	    return;
	}
    }
    gp->cptyp = (size > BYTE_THRESH) ? COPY_MEMCPY : COPY_BYTES;
    return;
}


static void
writefatal(glob *gp, size_t want, size_t got)
{
    batch *bp = &(gp->obtch);
    if (bp->thiso < 0) {
	bp->thiso = (bp->iofile.posn < 0) ? 0 : bp->iofile.posn;
    }
    setoffname(bp);
    sprintf(gp->emesg, "Expected %ld, got %ld on write following %s",
		    (long) want, (long) got, gp->recon);
    fatal(gp);
}


/* Record output
**
** For fancy diagnostics that we hope will never be issued,
** it is convenient to have a single write utility
** that accepts an io pointer and a ptrlen describing the record
** whereabout and length.  This is readily constructed for
** fixed-length records, and is already available for other formats.
*/

static void
writeptrlen(glob *gp, ptrlen *ptr)
{
/* The COPY_AS macro is ``tuned'' somewhat for writes shorter that
** the buffer.  When the record to be written fits in the buffer
** without filling it, the record is simply copied in.  If the record
** won't fit, or just fills the buffer, we fill the buffer, write it,
** and reset the pointer to the beginning.  If we were expecting records
** larger than a buffer, we'd add another loop, doing buffer-sized writes
** directly from the record (saving a copy to the buffer).  But such
** records will probably never be seen, so we avoid the overhead of testing.
** In any event, we eventually run the remaining record size below the
** size of the buffer, and copy it into place, as we do for most short
** records.
*/

#define	MEMCPY(TO, FR, LN) memcpy((void *)(TO), (void *)(FR), (size_t)(LN))
#define COPY_AS(TYPE) \
    TYPE ## tp = (TYPE *) gp->obufp; \
    TYPE ## fp = (TYPE *) (ptr->ptr); \
    for (;k > 0;) { \
	if (j > k) { \
	    gp->obtch.iofile.posn += k; \
	    do { *(TYPE ## tp)++ = *(TYPE ## fp)++; } while ((k -= sizeof(TYPE)) > 0); \
	    break; \
	} \
	k -= j; \
	/* fill buffer and write */ \
	do { *(TYPE ## tp)++ = *(TYPE ## fp)++; } while ((j -= sizeof(TYPE)) > 0); \
	got = write(gp->obtch.iofile.fd, (void *) (gp->obuff), OBUFSIZE); \
	if (got != OBUFSIZE) writefatal(gp, OBUFSIZE, got); \
	gp->obtch.iofile.posn += j = OBUFSIZE; \
	TYPE ## tp = (TYPE *) (gp->obuff); \
    } \
    gp->obufp = (uch *) TYPE ## tp

    long *longtp, *longfp;
    char *chartp, *charfp;
    long j, k, got;

    k = ptr->len;				/* # bytes to be copied */
    j = OBUFSIZE - (gp->obufp - gp->obuff);	/* # bytes left in buffer */
    switch (gp->cptyp) {
    case COPY_LONGS:
	COPY_AS(long); break;
    case COPY_BYTES:
	COPY_AS(char); break;
    default:
	chartp = (char *) gp->obufp;
	charfp = (char *) (ptr->ptr);
	for (;k > 0;) {
	    if (j > k) {
		gp->obtch.iofile.posn += k;
		MEMCPY(chartp, charfp, k);
		chartp += k;
		break;
	    }
	    k -= j;
	    /* fill buffer and write */
	    MEMCPY(chartp, charfp, j);
	    got = write(gp->obtch.iofile.fd, (void *) (gp->obuff), OBUFSIZE);
	    if (got != OBUFSIZE) writefatal(gp, OBUFSIZE, got);
	    charfp += j;
	    gp->obtch.iofile.posn += j = OBUFSIZE;
	    chartp = (char *) (gp->obuff);
	}
	gp->obufp = (uch *) chartp;
    }
    return;
}


static void
writefix(glob *gp, gptr p)
{
    ptrlen ptrlen;

    ptrlen.ptr = (uch *)p;
    ptrlen.len = gp->grecz;
    writeptrlen(gp, &ptrlen);
    return;
}

static void
stripfix(glob *gp, gptr p)
{
    ptrlen ptrlen;
    long prefix = gp->gkoff + gp->gklen;

    ptrlen.ptr = ((uch *) p) + prefix;
    ptrlen.len = gp->grecz - prefix;
    writeptrlen(gp, &ptrlen);
    return;
}


static void
writehdr(glob *gp, gptr p)
{
    ptrlen *ptrp = ((ptrlen *) p) + 1;

    writeptrlen(gp, ptrp);
    return;
}

static void
striphdr(glob *gp, gptr p)
{
    ptrlen *ptrp = (ptrlen *) p;
    ptrlen ptrlen;

    ptrlen.ptr = ptrp->ptr + ptrp->len;	/* byte past key from key ptrlen */
    ptrp++;				/* advance to record ptrlen */
    if ((ptrlen.len = ((ptrp->ptr + ptrp->len) - ptrlen.ptr)) > 0)
	writeptrlen(gp, &ptrlen);
    return;
}

static void
trimhdr(glob *gp, gptr p)
{
    ptrlen *ptrp = ((ptrlen *) p) + 1;
    uch *q, *r;
    ptrlen ptrlen;

    q = r = ptrp->ptr;
    while (*q++ != TERM);
    if ((ptrlen.len = ptrp->len - (q - r)) > 0) {
	ptrlen.ptr = q;
	writeptrlen(gp, &ptrlen);
    }
    return;
}


static void
writenew(glob *gp, gptr p)
{
    ptrlen *ptrp = (ptrlen *) p;

    if (gp->ptohd > sizeof(ptrlen)) ptrp++;
    ptrp->len++;
    writeptrlen(gp, ptrp);
    ptrp->len--;
    return;
}

static void
stripnew(glob *gp, gptr p)
{
    ptrlen *ptrp = (ptrlen *) p;
    ptrlen ptrlen;

    /* There MUST be a fancy key for the newline delimited records,
    ** or we would have bailed while processing the arguments.
    ** The result cannot be empty, because we didn't count the newline
    ** in the key, but we put it back for the write.  So an otherwise
    ** empty record will end up as an empty line.
    */

    ptrlen.ptr = ptrp->ptr + ptrp->len;	/* byte past key from key ptrlen */
    ptrp++;				/* advance to record ptrlen */
    ptrlen.len = (ptrp->ptr + ptrp->len + 1) - ptrlen.ptr;
    writeptrlen(gp, &ptrlen);
    return;
}


/* Preparing to sort what is in memory
**
** When a read routine produces the not-enough-space return,
** or when all inputs have been exhausted, we want to sort the
** records records currently in memory.  We must be prepared
** for a brecs of 0, if there was no input at all,
** or if we hit EOF immediately after sorting another batch.
**
** ``start'' addresses an array of brecs ``somethings'',
** each of length ``len''.  Could be fixed length records,
** could be a ptrlen or two describing records of variable length.
**
** Sometimes the array of somethings grew backwards, so must be reversed.
*/

static void
loadM1(glob *gp, uch *start, long len, int reverse)
{
    long n = gp->brecs;
    gptr *p = gp->m1ptr;

    if (n < 1) return;
    if (reverse) {
	start = start + (len * (n-1));
	len = -len;
    }
    do {
	*p++ = (void *) start;
	start += len;
    } while (--n);
    return;
}


/* Prepare the m1ptr and m2ptr arrays for a sort.
**
** For fixed length records, includebatch() has been building an
** array of pointers to the records, working backwards,
** with the current top at gp->plcur.  We can just reverse
** these into m1ptr, for which space has been reserved,
** and aim m2ptr at the original array.
**
** For the other formats, space for both the m1ptr and m2ptr
** arrays has been reserved just above the ptrlens that start
** at gp->plcur.  Aim the m1ptr's at these ptrlens.  Since the ptrlens grew
** backwards, reverse the order of the m1ptr's, so they
** address the records in the order they were read.
*/

static void
prepsaptrs(glob *gp)
{
    long n = gp->brecs;
    gptr *p;

    if (n < 1) return;
    switch (gp->fmtyp) {
    case FMTFIX:
	gp->m2ptr = (gptr *)(gp->plcur);
	gp->m1ptr = gp->m2ptr - n;
	p = reversebatch(gp, gp->m2ptr);
	if (gp->m1ptr != p) {
	    gp->m2ptr = gp->m1ptr;
	    gp->m1ptr = p;
	}
	break;
    default:
	gp->m2ptr = ((gptr *)(gp->plcur)) - n;
	gp->m1ptr = gp->m2ptr - n;
	loadM1(gp, (uch *)(gp->plcur), gp->ptohd, 1);
    }
    return;
}


/* Comparison
**
** All comparison routines build on the comparison of two strings
** of unsigned characters for a specified length, lexicographical order.
**
** ``In-line'' versions, to hold down overhead.
** Argument R is set to the sense of the comparison.
**
** BEWARE: Arguments are re-evaluated and modified.
**
** The Borland C compiler promises that memcmp will use unsigned char
** comparisons, and can be inlined with pragmas or command line options.
** The possibility of a carefully tuned, inline comparison makes a
** little additional ifdef-ing worthwhile.  When in doubt,
** generate pretty-good code ourselves, with no subroutine overhead.
*/

#ifdef	INLINEMEMCMP
#define	CMPN(A,B,N,R) R=memcmp(A,B,N)
#else

#ifndef	UNROLL
#define	CMPN(A,B,N,R) for(R=0;(--N>=0)&&((R=(*A++ - *B++))==0);)
#else
#define	CMPN(A,B,N,R) \
    R=0; \
    switch (N&03) { \
    case 3: if((R=(*A++ - *B++)))goto rset; N--; \
    case 2: if((R=(*A++ - *B++)))goto rset; N--; \
    case 1: if((R=(*A++ - *B++)))goto rset; N--; \
    } \
    for (;N>0;N-=4) { \
	if((R=(*A++ - *B++)))break; \
	if((R=(*A++ - *B++)))break; \
	if((R=(*A++ - *B++)))break; \
	if((R=(*A++ - *B++)))break; \
    } \
rset:
#endif

#endif


/*
** Something similar, where the lengths of A and B may differ.
**
** Argument N is a temporary.
** Argument AN is set to the shorter of the original AN and BN.
** Argument R is set to the sense of the comparison.
** Note that AN and BN are longs, whereas R (the return value) is an int.
**	This means we cannot always just set R to AN - BN to break ties.
*/

#if INT_MAX == LONG_MAX
#define	CMP2N(A,AN,B,BN,N,R) \
	{ \
	    if ((N = (AN) - (BN)) > 0) AN = BN; \
	    CMPN(A,B,AN,R); \
	    if (R == 0) { R = N; } \
	}
#else
#define	CMP2N(A,AN,B,BN,N,R) \
	{ \
	    if ((N = (AN) - (BN)) > 0) AN = BN; \
	    CMPN(A,B,AN,R); \
	    if (R == 0) { R = (N == 0) ? 0 : ((N > 0) ? 1 : -1); } \
	}
#endif

/* Comparison routines:
**
** We're a little nutso here about keeping the comparison code compact,
** because it gets called so often.  So we try to avoid nested subroutine
** calls, and tests that don't change during the run.  We cannot save
** code by using macros like
**
** #define	revfix(A,B)	fwdfix(B,A)
**
** because the argument to the internal sort routine must be the
** address of a subroutine, not a snippet of code.  And something like
**
** int revfix(void *a, void *b)
** {
**     return (fwdcmp(b, a));
** }
**
** is correct, but adds a subroutine call to every reverse comparison,
** and we promised not to penalize the internal sort for ``fancy'' options.
*/

/* When comparing fixed length records, the array of pointers
** passed to the merge sort routine will address the records directly.
** The length of the comparison is taken from the global gklen, which may or
** may not be identical to grecz.  Global gkoff is handled elsewhere.
*/

static int
fwdfix(gptr a, gptr b)
{
    int r;
    long n = G.gklen;
    uch *ap = (uch *) a;
    uch *bp = (uch *) b;

    CMPN(ap,bp,n,r);
    return (r);
}


/* Fixed-length fields with a non-zero gp->gkoff.  */

static int
fwdfixoff(gptr a, gptr b)
{
    int r;
    uch *ap = ((uch *)a) + G.gkoff;
    uch *bp = ((uch *)b) + G.gkoff;
    long n = G.gklen;

    CMPN(ap,bp,n,r);
    return (r);
}


/* For newline-delimited records, the arrays passed to merge sort
** address ptrlen structures, which, in turn, address the records.
** The basic routine is used where no global gp->gklen or gp->gkoff exists.
** The same routines apply to header-delimited records,
** where another ptrlen structure follows the one describing the keys,
** but that makes no difference in this context.
*/

static int
fwdhdrnew(gptr a, gptr b)
{
    uch *ap, *bp;
    long an, bn, n;
    int r;

    ap = ((ptrlen *)a)->ptr; an = ((ptrlen *)a)->len;
    bp = ((ptrlen *)b)->ptr; bn = ((ptrlen *)b)->len;
    CMP2N(ap,an,bp,bn,n,r);
    return (r);
}


/* Binary merge internal sort, with a few special mods
** for the special environment it now finds itself in.
**
** The ``limitation'' to sort only pointers, not objects of
** arbitrary size, was made by jpl long ago.  Things that were
** once options in what was a sort testbed have been hotwired
** to values suitable for this use.  In particular, we'll always
** initialize looking for natural runs, we'll always produce stable
** output, and we'll always do Peter McIlroy's binary merge.
*/

/* Pointer types for arithmetic and storage and convenience casts */
typedef uch * aptr;
#define	APTR(P)	((aptr)(P))
#define	GPTP(P)	((gptr *)(P))
#define GPPP(P) ((gptr **)(P))


/* byte offset from pointer P to (larger) pointer Q */
#define	BYTEOFF(P, Q) (APTR(Q) - APTR(P))

#define PSIZE sizeof(gptr)

/* If PSIZE is power of 2, make PSHIFT that power, if that helps */

#ifdef	PSHIFT
#define	PNELEM(P, Q)	(BYTEOFF(P,Q) >> (PSHIFT))
#define	PNBYTE(N)	((N) << (PSHIFT))
#define	PINDEX(P, N)	(GPTP(APTR(P) + PNBYTE(N)))
#else
/* Leave optimization to compiler */
#define	PNELEM(P, Q)	(GPTP(Q) - GPTP(P))
#define	PNBYTE(N)	((N) * (PSIZE))
#define	PINDEX(P, N)	(GPTP(P) + (N))
#endif

/* Pointer into other corresponding to pointer into this */
#define	POTHER(P, THIS, OTHER) GPTP(APTR(OTHER) + BYTEOFF(THIS,P))

#define FROMTOUPTO(src, dst, lim) do *dst++ = *src++; while(src<lim)


/* Runs are identified by a pointer in the auxilliary list.
** The pointer is at the start of the list,
** and it points to the start of the next list.
** NEXT is used as an lvalue, too.
*/

#define	NEXT(P)		(*GPPP(P))


/* N is the minimum number of additional pairs with the same sense to justify
** checking for a run and extending it.  Note that N counts PAIRS, not just
** elements, so N == 3 means a run of 6.
*/

#define	N (3)


/*
** Overview of algorithm and variables.
** The array of elements at list1 will be organized into runs of length 2,
** or runs of length >= 2 * N.  We only try to form long runs when N
** adjacent pairs compare in the same way, suggesting overall order.
**
** Unless otherwise specified, pair pointers address the first of two elements.
**
** b and b+1 are a pair that compare with sense ``sense''.
** b is the ``bottom'' of adjacent pairs that might form a longer run.
**
** p2 parallels b in the list2 array, where runs are defined by
** a pointer chain.
**
** t represents the ``top'' of the adjacent pairs that might extend
** the run beginning at b.  Usually, t addresses a pair
** that compares with opposite sense from (b,b+1).
** However, it may also address a singleton element at the end of list1,
** or it may be equal to ``last'', the first element beyond list1.
**
** r addresses the Nth pair following b.  If this would be beyond t,
** we back it off to t.  Only when r is less than t do we consider the
** run long enough to consider checking.
**
** q addresses a pair such that the pairs at b through q already form a run.
** Often, q will equal b, indicating we only are sure of the pair itself.
** However, a search on the previous cycle may have revealed a longer run,
** so q may be greater than b.
**
** p is used to work back from a candidate r, trying to reach q,
** which would mean b through r would be a run.  If we discover such a run,
** we start q at r and try to push it further towards t.
** If b through r is NOT a run, we detect the wrong order at (p-1,p).
** In any event, after the check (if any), we have two main cases.
**
** 1) Short run.  b <= q < p <= r <= t.
**	b through q is a run (perhaps trivial)
**	q through p are uninteresting pairs
**	p through r is a run
**
** 2) Long run.  b < r <= q < t.
**	b through q is a run (of length >= 2 * N)
**
** Note that degenerate cases are not only possible, but likely.
** For example, if the pair following b compares with opposite sense,
** then b == q < p == r == t.
*/


static int
dynprep(glob *gp, int (*cmp)(gptr, gptr))
{
    gptr *list2;
    int sense;
    register gptr *b, *p, *q, *t, *p2;
    register gptr c, *last, *r;
    gptr *savep;
    long runs = 0;

    b = gp->m1ptr;
    list2 = gp->m2ptr;
    last = PINDEX(b, gp->brecs);
    sense = (cmp(*b, *(b+1)) > 0);
    for (p2 = list2; b < last; ) {
	/* We just started, or just reversed sense.
	** Set t at end of pairs with the prevailing sense.
	*/
	for (p = b+2, t = p; ++p < last; t = ++p) {
	    if ((cmp(*t, *p) > 0) != sense) break;
	}
	q = b;
	/* Having laid out the playing field, look for long runs */
	do {
	    p = r = b + (2 * N);
	    if (r >= t) p = r = t;	/* too short to care about */
	    else {
		while (((cmp(*(p-1), *p) > 0) == sense) && ((p -= 2) > q));
		if (p <= q) {
		    /* b through r is a (long) run.
		    ** Extend it as far as possible.
		    */
		    p = q = r;
		    while (((p += 2) < t) &&
			   ((cmp(*(p-1), *p) > 0) == sense)) q = p;
		    r = p = q + 2;	/* no simple pairs, no after-run */
		}
	    }
	    if (q > b) {		/* run of greater than 2 at b */
		savep = p;
		p = q += 2;
		/* pick up singleton, if possible */
		if ((p == t) &&
		    ((t + 1) == last) &&
		    ((cmp(*(p-1), *p) > 0) == sense)) savep = r = p = q = last;
		p2 = NEXT(p2) = p2 + (p - b);
		if (++runs > gp->mxrun) return(0);
		if (sense) while (b < --p) {
		    c = *b;
		    *b++ = *p;
		    *p = c;
		}
		p = savep;
	    }
	    while (q < p) {		/* simple pairs */
		p2 = NEXT(p2) = p2 + 2;
		if (++runs > gp->mxrun) return(0);
		if (sense) {
		    c = *q++;
		    *(q-1) = *q;
		    *q++ = c;
		} else q += 2;
	    }
	    if (((b = p) == t) && ((t+1) == last)) {
		NEXT(p2) = p2 + 1;
		if (++runs > gp->mxrun) return(0);
		b++;
	    }
	    q = r;
	} while (b < t);
	sense = !sense;
    }
/* fprintf(stderr, "%ld runs for %ld items, %g items/run\n", runs, gp->brecs, (double) gp->brecs/runs); */
    return(1);
}


/* f1 walks through successive lists.
** l1 is the end of the corresponding list.
** p2 is the base of the next list on the ``other side''.
** np2 is the base of the new pointer chain on the other side.
** Unique elements are copied to tp1, which will fall behind f1
** as duplicated items are found.
**
** The binary ramp-up and ramp-down are complex in software terms,
** but they are low-overhead.  For example, in the (common) case
** of a run of length 2 with no duplicates, the ramp-up stops at
** the first compare (to the adjacent element), and the ramp-down
** stops without a comparison when b is advanced and reaches that element.
** There are cases where comparisons are ``wasted'' relative to a linear
** search.  If there are exactly two dups in a run of at least four,
** it takes three compares, not 2, to find the non-dup at position 3.
** But when there are many dups, the binary ramping can save a lot
** of compares.
*/

static void
undupe(glob *gp, int (*cmp)(gptr, gptr))
{
    size_t i;
    gptr *f1, *l1, *t, *b, *p, *tp1;
    gptr *p2, *np2, *last;
    gptr *list1, *list2;

    list1 = gp->m1ptr;
    list2 = gp->m2ptr;
    last = PINDEX(list2, gp->brecs);
    f1 = tp1 = list1;
    p2 = np2 = list2;
    p2 = NEXT(p2);
    do {					/* loop over lists */
	l1 = POTHER(p2, list2, list1);		/* past end of list at f1 */
	do {					/* loop over dupes */
	    /* Ramp up.
	    ** f1 through b will be identical.
	    ** t will be greater than f1, or at the end of the list.
	    */
	    for (b = f1, t = l1, i = 1;; i = i + i) {
		if ((p = PINDEX(b, i)) >= t) {
		    if (((p = PINDEX(t, -1)) > b) && (cmp(*f1, *p) == 0)) {
			b = p;
		    } else t = p;
		    break;
		}
		if (cmp(*f1, *p) < 0) {
		    t = p;
		    break;
		}
		b = p;
	    }
	    b++;		/* b was known to be equal to f1 */
	    /* Ramp down.
	    ** shrink [b, t) to find least b greater than f1.
	    */
	    while (b < t) {
		p = PINDEX(b, (PNELEM(b, t) - 1) / 2);
		if (cmp(*f1, *p) < 0) t = p;
		else b = p + 1;
	    }
	    if (gp->revrs) f1 = b - 1;
	    *tp1++ = *f1;
	    f1 = b;
	} while (f1 < l1);
	/* Adjust list chains.
	** Until we hit a duplicate, the new chain and old chain
	** will coincide, so we have to be careful about the order
	** of fetching from the old relative to storing in the new.
	*/
	if ((i = (p2 != last))) p2 = NEXT(p2);
	np2 = NEXT(np2) = POTHER(tp1, list1, list2);
    } while (i);
    gp->brecs = PNELEM(list1, tp1);
    return;
}


/* The bmerge routine owes a great deal to Peter McIlroy and UCB.
** Peter's version sorted qsort-style arrays of fixed
** (but unpredictable) sized elements.
** This version sorts only arrays of pointers,
** allowing for some simplifications.
** This version also worries about strictly stable ordering.
*/


/* Overview of bmerge variables:
**
** list1 and list2 address the main and auxialliary arrays.
** They swap identities after each merge pass.
** In the original, base pointed to the original list1, so we could tell if
** the pointers ended up where they belonged (or must be copied).
** In this context, we don't care which array the sorted pointers
** end up in.  Just return it, no need to copy.
**
** When we are merging two lists, f1 and f2 are the next elements
** on the respective lists.  l1 and l2 mark the end of the lists.
** tp2 is the current location in the merged list.
**
** p1 records where f1 started.
** After the merge, a new descriptor is built there.
**
** p2 is a ``parallel'' pointer in (what starts as) descriptor space.
** It is used to identify and delimit the runs.
**
** In the heat of determining where q, the greater of the f1/f2 elements,
** belongs in the other list, b, t and p, represent bottom, top and probe
** locations, respectively, in the other list.
** They make convenient temporary pointers in other places.
*/


static gptr *
bmerge(glob *gp, int(*cmp)(gptr, gptr))
{
    long i, onmemb;
    int sense, lastsense;
    register gptr *f1, *f2, *t, *b, *p, *tp2, *l1, *l2, *q;
    gptr *p2, *last;
    /* gptr *base = list1; */
    gptr *p1;
    gptr *list1, *list2;

    list1 = gp->m1ptr;
    if (gp->brecs <= 1) return(list1);	/* sorted trivially */
    list2 = gp->m2ptr;
    onmemb = gp->brecs;
    gp->mxrun = onmemb / 32;		/* XXX demand 32 elements per run */
    if (dynprep(gp, cmp) == 0) return(NULL);
    if (gp->uniqu) undupe(gp, cmp);
    last = PINDEX(list2, gp->brecs);
    while (NEXT(list2) != last) {
	/* More than one run remains.  Do some merging to reduce runs. */
	l2 = p1 = list1;
	for (tp2 = p2 = list2; p2 != last;) {
	    /* The new first run begins where the old second list ended.
	    ** Use the p2 ``parallel'' pointer to identify the end of the run.
	    */
	    f1 = l2;
	    t = NEXT(p2);
	    f2 = l1 = POTHER(t, list2, list1);
	    if (t != last) t = NEXT(t);
	    l2 = POTHER(t, list2, list1);
	    p2 = t;
	    while (f1 < l1 && f2 < l2) {
		/* If head 1 is larger than head 2, find ALL the elements
		** in list 2 strictly less than head1, write them all,
		** then head1.  Then compare the new heads, and repeat,
		** until one or both lists are exhausted.
		**
		** In all comparisons (after establishing
		** which head to merge) the item to merge
		** (at pointer q) is the first operand of
		** the comparison.  When we want to know
		** if ``q is strictly less than the other'',
		** we can't just do
		**    cmp(q, other) < 0
		** because stability demands that we treat equality
		** as high when q comes from l2, and as low when
		** q was from l1, and we need to detect equality
		** when gp->uniqu is on.  So we ask the question by doing
		**    cmp(q, other) <= sense
		** and make sense == 0 when equality should look low,
		** or when we are trying to detect equality, and
		** -1 when equality should look high.
		**
		** To break ties effectively when gp->uniqu is in effect,
		** we make sure that lastsense captures the sense of the
		** latest comparison, and that t addresses the equal element.
		*/


		lastsense = cmp(*f1, *f2);
		if ((lastsense == 0) && gp->uniqu) {	/* tie in 1st place */
		    *tp2++ = (gp->revrs) ? *f2 : *f1;
		    f1++; f2++;
		    continue;
		}


		if (lastsense <= 0) {
		    q = f2; b = f1; t = l1;
		    sense = (gp->uniqu) ? 0 : -1;
		} else {
		    q = f1; b = f2; t = l2;
		    sense = 0;
		}


		/* ramp up
		**
		** If uniqu is in effect and a tie is detected,
		** leave t at the tie and lastsense == 0.
		** In any other case, leave t at something strictly
		** greater than q (or at the end of the list),
		** and b at something strictly less than q.
		*/
		for (i = 1; ; i <<= 1) {
		    if ((p = PINDEX(b, i)) >= t) {
			/* off the end */
			if (((p = PINDEX(t, -1)) > b) &&
			    ((lastsense = cmp(*q, *p)) <= sense))
			     t = p;
			else b = p;
			break;
		    } else if ((lastsense = cmp(*q, *p)) <= sense) {
			t = p;
			break;
		    } else b = p;
		}


		/* If gp->uniqu applies and lastsense is 0,
		** q and t address the equal pairs, and we need
		** look no further to determine q's position in
		** the other list.
		**
		** Otherwise, q is known to follow b
		** and must be inserted before t.
		** Increment b, so the range of possibilities is [b,t).
		** Round binary split down, to favor early appearance.
		** Adjust b and t until q belongs just before t.
		*/

		if (!((lastsense == 0) && (gp->uniqu))) {	/* not a tie */
		    b++;
		    while (b < t) {
			p = PINDEX(b, (PNELEM(b, t) - 1) / 2);
			if ((lastsense = cmp(*q, *p)) <= sense) {
			    t = p;
			    if ((lastsense == 0) && (gp->uniqu)) break;
			} else b = p + 1;
		    }
		}


		/* Copy all the strictly low elements */

		if ((lastsense == 0) && (gp->uniqu)) {	/* ended in a tie */
		    if (q == f1) {
			FROMTOUPTO(f2, tp2, t);
		    } else {
			FROMTOUPTO(f1, tp2, t);
		    }
		    *tp2++ = (gp->revrs) ? *f2 : *f1;
		    f1++; f2++;
		} else {
		    if (q == f1) {
			FROMTOUPTO(f2, tp2, t);
			*tp2++ = *f1++;
		    } else {
			FROMTOUPTO(f1, tp2, t);
			*tp2++ = *f2++;
		    }
		}
	    }


	    /* Run out remaining list */
	    if (f1 == l1) {
		   if (f2 < l2) FROMTOUPTO(f2, tp2, l2);
	    } else              FROMTOUPTO(f1, tp2, l1);
	    p1 = NEXT(p1) = POTHER(tp2, list2, list1);
	}
	gp->brecs = PNELEM(list2, tp2);
	t = list1;
	list1 = list2;
	list2 = t;
	last = PINDEX(list2, gp->brecs);
    }
    /* if (base == list2) memcpy(list2, list1, nmemb*sizeof(*list1)); */
    return (list1);
}


/* File name arguments to the original command are addressed,
** ``backwards'', through the gp->argnd pointer, via gp->nexti.
** If there were no file name arguments,
** a dummy `-' argument will have been supplied,
** and `-' will always direct us to standard in.
**
** Temporary index arguments will have non-negative indx values,
** the next of which will be kept in gp->nexto.
** Their names will have to be generated on the fly.
*/

/* O_EXCL and SGI NFS do not coexist well.  Files are created mode 000,
** which surely makes them exclusive, but after closing and reopening
** for input, they can't be read AT ALL.  So make it optional,
** and keep it off by default.
*/

#ifndef OPENNEWCOMMON
#define	OPENNEWCOMMON	(O_WRONLY|O_CREAT|O_TRUNC)
#endif

#ifdef SECURE
#define	OPENNEWSECURE	(OPENNEWCOMMON|O_EXCL)
#else
#define	OPENNEWSECURE	(OPENNEWCOMMON)
#endif

#define	STDINNAME	("-")

#ifdef DOSLIKE
#define	OPENREADMODE	("rb")
#define	OPENWRITEMODE	("wb")
#define	OPENNEWACCESS	(OPENNEWSECURE|O_BINARY)
#define	OPENFINALACCESS	(OPENNEWCOMMON|O_BINARY)
#define	OPENREADACCESS	(O_RDONLY|O_BINARY)
#define	OPENNEWMODE	(S_IREAD)
#else
#define	OPENREADMODE	("r")
#define	OPENWRITEMODE	("w")
#define	OPENNEWACCESS	(OPENNEWSECURE)
#define	OPENFINALACCESS	(OPENNEWCOMMON)
#define	OPENREADACCESS	(O_RDONLY)

#ifndef OPENNEWMODE
#define	OPENNEWMODE	(0440)
#endif

#ifndef OPENFINALMODE
#define	OPENFINALMODE	(0664)
#endif

#endif

static const char   *StdinName = STDINNAME;

/* Close (conditionally)
**
** We'll avoid closing stdin, so it can appear again later.
** (EOF on stdin doesn't mean it was closed.  For example,
** input could be coming from a tty, punctuated with CTRL-D's.)
*/

static void
closeio(batch *bp)
{
    glob *gp = bp->com;
    int i;
    size_t flush, got;

    /* Close this file, if there IS a file */
    i = 0;
    if ((bp->iofile.fd != -1) && strcmp(bp->iofile.name, STDINNAME)) {
	if ((bp->flag & BATCH_OUT) && (flush = gp->obufp - gp->obuff) > 0) {
	    got = write(bp->iofile.fd, (void *) (gp->obuff), flush);
	    if (flush != got) writefatal(gp, flush, got);
	    gp->obufp = gp->obuff;
	}
#ifdef	MMAP
	if (bp->flag & BATCH_MAP) {
	    long m = ROUNDUP(bp->total, bp->tunit);
	    munmap(bp->bufp, m);
	}
#endif
	i = closeit(bp);
    }
    if (i && !(gp->igerr)) writefatal(gp, i, bp->iofile.indx);
    return;
}


/* Encode as much as possible of number ``num'', in the ``wid'' bytes
** that precede ``endp''.  The user is responsible for ensuring that
** there really ARE ``num'' bytes preceding ``endp''.
**
** Return whatever part of num couldn't fit.
*/

static long
encode(glob *gp, long num, int wid, char *endp)
{
    while (--wid >= 0) {
	*(--endp) = ALPHABET[num % gp->abetz];
	num /= gp->abetz;
    }
    return (num);
}


static void
dotempdir(glob *gp)
{
    long pid;
    char *p, *q;
    int n;

    /* If the user supplied a temporary directory explicitly
    ** (which a checkpoint, in particular, would do),
    ** then accept it exactly as is, and allocate space for
    ** the gp->tbase (and slash and null).
    */

    if (gp->dname) {
	n = strlen(gp->dname) + 1 + BASETOTALLEN + 1;
	if ((gp->tname = (char *) malloc(n+n)) == NULL) {
	    errno = ENOMEM;
	    sprintf(gp->emesg,
		"Not enough space to allocate %d-byte temp area??", n+n);
	    fatal(gp);
	}
	gp->tnam2 = gp->tname + n;
        strcpy(gp->tname, gp->dname);
    } else {			/* Try to find a place to build one */


#ifndef	ENVTMPDIR
#define	ENVTMPDIR	"TMPDIR"
#endif

#ifndef	DIRSEP
#ifndef	DOSLIKE
#define	DIRSEP	(':')
#else
#define	DIRSEP	(';')
#endif
#endif

	/* Take gp->dlist from -T option by preference,
	** contents of environment variable if there was no -T option,
	** local directory if the environment variable is missing or empty.
	*/

	if (gp->dlist == NULL) gp->dlist = getenv(ENVTMPDIR);
	if ((gp->dlist == NULL) || !*gp->dlist) gp->dlist = ".";

	/* The complete list is at least as long as any component directory.
	** Adding enough space for 2 /'s, a null, our private subdirectory,
	** and our tempfile names will provide a workspace.
	*/

	n = strlen(gp->dlist) + 3 + DIRTOTALLEN + BASETOTALLEN;
	if ((gp->tname = (char *) malloc(n+n)) == NULL) {
	    errno = ENOMEM;
	    sprintf(gp->emesg,
		"Not enough space to allocate %d-byte temp area??", n+n);
	    fatal(gp);
	}
	gp->tnam2 = gp->tname + n;

	/* Run through the directories in the list, looking for one
	** in which we can create a temporary subdirectory encoding the pid.
	** A ``null directory'' is NOT treated as a reference to ``.''.
	** If you want a reference to ``.'', make it explicit.
	** ``Null directories'' will be silently ignored.
	*/

	pid = getpid();
	for (p=gp->dlist; *p; p = q) {
	    while (*p == DIRSEP) p++;		/* ignore leading separators */
	    if (*p == '\0') break;		/* ran off the end */
	    for (q = p+1; *q && (*q != DIRSEP); q++);	/* find end of name */
	    n = q - p;				/* strlen(name) */
	    strncpy(gp->tname, p, n);	/* copy name to work area */
	    p = gp->tname + n;		/* end of main directory name */
	    *p++ = SLASH;			/* add a slash for subdir */
	    strcpy(p, DIRPREFIX);		/* add prefix */
	    p += DIRPREFLEN + PIDENCODE;	/* leave room for pid */
	    encode(gp, pid, PIDENCODE, p);	/* fill it in (before p) */
	    strcpy(p, DIREXTENSION);		/* add extension */
	    if (MKDIR(gp->tname)) {		/* Failed, ok if it exists */
		if (errno != EEXIST) continue;	/* try another place */
	    }
	    p = gp->tname;			/* Make sure *p is non-null */
	    break;
	}
	if (*p == '\0') {
	    errno = ENOTDIR;
	    sprintf(gp->emesg,
		"No usable temporary directory in ``%.100s''", gp->dlist);
	    fatal(gp);
	}
    }
    p = gp->tname + strlen(gp->tname);	/* get to end of directory */
    *p++ = SLASH;				/* add in a slash */
    gp->tbase = p;				/* base of tempnames go here */
    strcpy(gp->bname, BASEPREFIX);		/* Initialize tempfile name */
    strcpy(gp->bname + BASEPREFLEN + SEQENCODE, BASEEXTENSION);
    return;
}


static void
settmpname(batch *bp)
{
    glob *gp = bp->com;
    ios *io = &(bp->iofile);
    long index;

    if ((index = io->indx) < 0) return;	/* Name is from argument list */
    io->name = gp->tname;
    index = encode(gp, index, SEQENCODE, gp->bname + BASEPREFLEN + SEQENCODE);
    if (index > 0) {
	errno = ENOMEM;
	sprintf(gp->emesg, "Too many temporaries (%ld), allocate more memory",
	    io->indx);
	fatal(gp);
    }
    strcpy(gp->tbase, gp->bname);
    return;
}


static void
setoffname(batch *bp)
{
    verylong off;
    ptrlen *ptrp;
    glob *gp = bp->com;

    settmpname(bp);			/* Make sure name is fresh */
    if ((off = bp->thiso) == -1) {	/* Calculate offset if not specified */
	switch (gp->fmtyp) {
	case FMTFIX:
	    off = bp->sumi * gp->grecz;
	    break;
	default:
	    ptrp = (ptrlen *) (bp->curp);
	    if (gp->ptohd > sizeof(ptrlen)) ptrp++;
	    off = ptrp->ptr - bp->batch.ptr;
	}
	off += bp->batcho;
    }
    sprintf(gp->recon, OffNameFmt,
#ifdef	HUGE
			    verylongtoa(off),
#else
			    off,
#endif
			    bp->iofile.name);
    return;
}


/* Summarize what happened */

#define STRFMT	"%15s %s\n"
#define LONGFMT	"%15s %ld\n"
#define REALFMT	"%15s %.2f\n"

static void
summarize(glob *gp)
{
    if (gp->justs) {
	gp->tbase[-1] = '\0';	/* clear slash and any basename */
	fprintf(stderr, STRFMT, "tempdir", gp->tname);
	gp->tbase[-1] = SLASH;	/* Restore / to tempdir */
    }
    #ifdef HUGE
    fprintf(stderr, STRFMT, "bytes", verylongtoa(gp->nbyte));
    #else
    fprintf(stderr, LONGFMT, "bytes", gp->nbyte);
    #endif
    #ifdef HUGE
    fprintf(stderr, STRFMT, "records", verylongtoa(gp->nrecs));
    #else
    fprintf(stderr, LONGFMT, "records", gp->nrecs);
    #endif
    if (gp->nrecs) {
	fprintf(stderr, LONGFMT, "maximum", gp->maxrz);
	fprintf(stderr, REALFMT, "average", ((double) gp->nbyte) / gp->nrecs);
    }
    fprintf(stderr, LONGFMT, "mergedepth", (long) (gp->mpass));
    fprintf(stderr, LONGFMT, "mergewidth", gp->fanin);
    fprintf(stderr, LONGFMT, "memwidth", gp->fanmx);
    fprintf(stderr, LONGFMT, "maxwidth", MAXBATCH);
    fprintf(stderr, LONGFMT, "temporaries", gp->nexto);
    fprintf(stderr, LONGFMT, "memory", gp->dsize);
    fprintf(stderr, LONGFMT, "alignment", DEFAULT_AUNIT);
    fprintf(stderr, LONGFMT, "overhead", (long) (gp->ptohd + 2*sizeof(uch *)));
    fprintf(stderr, LONGFMT, "maxio", gp->maxio);
    fprintf(stderr, LONGFMT, "blocksize", (long) gp->tunit);
    fprintf(stderr, STRFMT,  "version", RcsID);
    fprintf(stderr, STRFMT,  "command", gp->whomi);
}


/* Stuff common to opens for input and output */

static void
cleario(batch *bp)
{
    bp->iofile.posn = 0;
    bp->batcho = 0;
    bp->thiso = -1;
    bp->left = bp->parsed = bp->scanned = 0;
    bp->fsize = 0;
    bp->aunit = DEFAULT_AUNIT;
    bp->tunit = DEFAULT_TUNIT;
    bp->flag  = BATCH_ALL | BATCH_EOF | BATCH_CLS;

    bp->batch.len = 0;
    bp->sumc = bp->couldbe = bp->sumx = 0;
    bp->sumi = -1;
    return;
}


static int
closeit(batch *bp)
{
    glob *gp = bp->com;
    int rc;

    bp->flag |= BATCH_ALL | BATCH_CLS;
    if (rc = close(bp->iofile.fd)) {
	sprintf(gp->emesg, "Close error");
	diag(gp, 0);
    }
    bp->iofile.fd = -1;
    return (rc);
}


static int
openin(glob *gp, int sort)
{
    int want, got;
    verylong left;
    filestatus st;
    batch *bp;

    if (gp->btchi >= MAXBATCH) return(0);	/* out of batches in btcha */
    bp = gp->btcha + gp->btchi;
    bp->com = gp;
    cleario(bp);
    if (sort) batchreset(bp);
    bp->curp  = (gptr) (bp->plast);
    bp->bufp  = bp->batch.ptr = bp->auxbp;
    bp->grecz = gp->grecz;			/* initial -1 for hdr-delims */
    bp->maxio = gp->maxio;			/* start with system-wide */

    if ((bp->iofile.indx = gp->nexti) < 0) {
	bp->iofile.name = gp->argnd[bp->iofile.indx];
    } else {
	settmpname(bp);
    }
    if (strcmp(bp->iofile.name, STDINNAME) == 0) {
	bp->iofile.fd = 0;
    } else {
	if ((bp->iofile.fd = open(bp->iofile.name, OPENREADACCESS)) < 0) {
	    /* Tolerate failure from too many open files.
	    ** Other causes are regarded as fatal.
	    */
#ifdef	EMFILE
	    if (errno == EMFILE) return(0);
#endif
	    sprintf(gp->emesg, "Open for reading failed on file ``%.150s''",
				bp->iofile.name);
	    fatal(gp);
	}
    }
    gp->nexti++;				/* has been opened now */
    gp->btchi++;
    /* See what we can learn about the file */
    bp->flag &= ~(BATCH_ALL | BATCH_EOF | BATCH_CLS);
    if ((FSTAT(bp->iofile.fd, &st) == 0) && S_ISREG(st.st_mode)) {
	bp->flag |= BATCH_SIZ;
	bp->fsize = st.st_size;
	bp->tunit = gp->tunit;
#ifdef	MMAP
	/* mmap fails on empty files, on SGIs at any rate.
	** In any event, we have to be conservative about closing
	** memory-mapped file descriptors in the sort phase,
	** so a large number of empty files could chew up all
	** the file descriptors and cause the sort to fail.
	*/
	if (bp->fsize > 0) {
#ifdef	PAGESIZE
	    bp->tunit = PAGESIZE;
#else
#ifdef	_SC_PAGESIZE
	    bp->tunit = sysconf(_SC_PAGESIZE);
#else
	    bp->tunit = getpagesize();
#endif	/* _SC_PAGESIZE */
#endif	/* PAGESIZE */
	    bp->thiso = 0;
	    bp->bufp = MAP(0, ROUNDUP(bp->total, bp->tunit),
			PROT_READ, MAP_PRIVATE, bp->iofile.fd, 0);
	    if (bp->bufp == (uch *)(-1)) {
		setoffname(bp);
		sprintf(gp->emesg, "mmap failure at %s", gp->recon);
		diag(gp, 0);
		bp->tunit = gp->tunit;
	    } else {
		bp->batch.ptr = bp->bufp;
		bp->flag |= BATCH_MAP;
	    }
	}
#else	/* ! MMAP */
#ifdef	DIRECTIO
#if	defined(F_DIOINFO) & defined(F_SETFL) & defined(FDIRECT)
	{
	    struct dioattr da;

	    /* The first fcntl might fail if the file system is not
	    ** one of the acceptable types.  If it succeeds, though,
	    ** we have set a file flag, so there's no turning back.
	    */
	    if (fcntl(bp->iofile.fd, F_SETFL, FDIRECT) >= 0) {
		if (fcntl(bp->iofile.fd, F_DIOINFO, &da) < 0) {
		    sprintf(gp->emesg,
			"fcntl failure on ``%.100s'', compile without DIRECTIO",
			bp->iofile.name);
		    fatal(gp);
		}
		if ((bp->aunit % da.d_mem) || (bp->tunit % da.d_miniosz)) {
		    sprintf(gp->emesg,
			"DIRECTIO: aunit %ld vs %ld, tunit %ld vs %ld",
				bp->aunit, (long) da.d_mem,
				bp->tunit, (long) da.d_miniosz);
		    fatal(gp);
		}
		if (bp->maxio > da.d_maxiosz) bp->maxio = da.d_maxiosz;
		bp->flag |= BATCH_DIO;
	    }
	}
#endif	/* F_DIOINFO & F_SETFL & FDIRECT */
#endif	/* DIRECTIO */
#endif	/* MMAP */
    }
    if (gp->ckoff > 0) {	/* Skip some bytes at the start of this file */
	/* XXX tunit alignment will be hosed if we aren't lucky */
	if (bp->flag & BATCH_SIZ) {
	    /* XXX needs to be fancier for memory mapping */
	    bp->iofile.posn = LSEEK(bp->iofile.fd, gp->ckoff, SEEK_SET);
	} else {
	    for (left = gp->ckoff; left > 0; left -= got) {
		want = (left > bp->tunit) ? bp->tunit : left;
		got = read(bp->iofile.fd, bp->bufp, want);
		if (got <= 0) break;
		bp->iofile.posn += got;
	    }
	}
	if (bp->iofile.posn != gp->ckoff) {	/* Must have hit EOF or error */
	    bp->thiso = bp->iofile.posn;
	    setoffname(bp);
	    errno = EIO;
	    sprintf(gp->emesg, "Unable to seek to position %ld: %s",
				(long) (gp->ckoff), gp->recon); /* XXX */
	    fatal(gp);
	}
	bp->batcho = gp->ckoff;
	gp->ckoff = 0;
    }
    if ((bp->iofile.indx < 0) && (gp->safst == SAFEOUT_UNKNOWN)) {
	/* This is one of the command-line inputs,
	** and we don't know yet if the final output file
	** happens to be one of the input files, too.
	** Do what we can to detect a match.
	** In the UNIX world, two references are to the same file iff
	** the device and inodes are the same.  The DOS/PC world has
	** no useful counterpart to inode, so the best we can do is
	** check device, which corresponds to drive.
	** There's no reason why the fstat should have failed, but if it did,
	** just play it supersafe and pretend the input matches the output.
	*/
	if (bp->flag & BATCH_SIZ) {
	    if (st.st_dev == gp->finst.st_dev) {
#ifndef DOSLIKE
		if (st.st_ino == gp->finst.st_ino) /* ``falls through'' */
#endif
		    gp->safst = SAFEOUT_UNSAFE;
	    }
	} else gp->safst = SAFEOUT_UNSAFE;
    }
    if ((bp->flag & BATCH_SIZ) && (bp->fsize == 0)) {
	closeit(bp);
	bp->flag |= BATCH_ALL | BATCH_EOF | BATCH_CLS;
    }
    gp->btchp[gp->atbtp++] = bp;	/* caller ensures space is there */
    return (1);
}


static void
openfinalout(glob *gp)
{
    batch *bp = &(gp->obtch);
    ios *io = &(bp->iofile);

    cleario(bp);
    if (gp->final == NULL) {
	io->name = "(stdout)";
	io->fd = 1;
    } else {
	io->name = gp->final;
	if ((io->fd = open(io->name, OPENFINALACCESS, OPENFINALMODE)) < 0) {
	    sprintf(gp->emesg, "Open failed on final output ``%.100s''",
		io->name);
	    fatal(gp);
	}
    }
    io->indx = -1;		/* Make it look non-temporary */
    bp->flag = BATCH_OUT;
    return;
}


/* Begin ``opening'' the final output.
**
** If the final output file isn't among the input files,
** we can open it immediately.  Otherwise, divert to a temp file,
** and copy later, when everything is done.
**
** In the merge phase, an empty temp file has already been opened.
** In the sort phase, there is no pre-opened temp file.
*/

static void
beginfinalout(glob *gp, int tmpopen)
{
    if (gp->safst != SAFEOUT_UNSAFE) {	/* OK to blast final out */
	if (tmpopen) {			/* If we have a temp... */
	    closeio(&(gp->obtch));	/* ... we don't need it ... */
	    settmpname(&(gp->obtch));	/* close, refresh the name */
	    remove(gp->obtch.iofile.name);	/* and remove empty file */
	}
	openfinalout(gp);		/* Assign final out to gp->obtch */
    } else {				/* copy to temp for now */
	if (!tmpopen) opennewtemp(gp);	/* get temp if needed */
    }
    gp->writf = gp->strpf;
    setcopytype(gp, gp->strip);
    return;
}

static void
finishfinalout(glob *gp)
{
    if (gp->safst == SAFEOUT_UNSAFE) {		/* Must move/copy */
	/* Before we start, checkpoint to restart from this point */
	gp->focpy = 1;
	gp->nexto = gp->obtch.iofile.indx;	/* Identify input */
	ckptmerge(gp);
	settmpname(&(gp->obtch));		/* refresh the tempfile name */
	copyfile(gp, gp->obtch.iofile.name, gp->final);
    }
    return;
}


static void
copyfile(glob *gp, char *from, char *to)
{
    int c, rc;
    FILE *ip, *op;

    if (rename(from, to) == 0) {
	chmod(to, (gp->final == NULL) ? OPENFINALMODE : gp->finst.st_mode);
	return;
    }
    ip = fopen(from, OPENREADMODE);
    op = fopen(to, OPENWRITEMODE);
    if (ip && op) {
	while ((c = getc(ip)) != EOF) putc(c, op);
	rc = ferror(ip) + ferror(op);
	rc += fclose(ip);
	rc += fclose(op);
	if (rc == 0) return;
    }
    sprintf(gp->emesg, "Final copy failed from ``%.50s'' to ``%.50s''",
			from, to);
    fatal(gp);
}


static void
opennewtemp(glob *gp)
{
    batch *bp = &(gp->obtch);
    ios *io = &(bp->iofile);

    cleario(bp);
    io->indx = gp->nexto++;
    settmpname(bp);

    /* jpl is concerned about symlinks set up in the temp directory
    ** so that a sort job run as root could be used to clobber
    ** some file (like /etc/passwd).  Doing this securely needs
    ** some thought (and probably cannot be done using fopen).
    ** O_EXCL + O_CREATE in open() look ideal, if less portable,
    ** and the introduction of the subdirectory (which we can make
    ** less writable, in a UNIX environment) makes it more difficult
    ** for bad guys to create things that might look like temp files.
    **
    ** In any event, we'll remove the file before opening it,
    ** to at least make the process less risky for root.
    ** (There is still a window for mischief between remove and open).
    ** It is still dangerous for others, since the remove will
    ** fail on a symlink owned by someone else, even if it points
    ** at a file owned by you.  The secure, but less portable, approach
    ** will fail on open if the file still exists for any reason.
    */
    remove(io->name);
    if ((io->fd = open(io->name, OPENNEWACCESS, OPENNEWMODE)) < 0) {
	sprintf(gp->emesg,
	    "Open for output failed on temporary file ``%.50s''", io->name);
	fatal(gp);
    }
    bp->flag = BATCH_OUT;
    setcopytype(gp, 0);
    return;
}


/* Clean up just before exiting.
**
** No fatal() messages allowed, lest we get into a loop.
** Just tidy up, as directed, and return.
*/

static void
docleanup(glob *gp)
{
    batch *bp = &(gp->obtch);
    ios *io = &(bp->iofile);

    /* Do nothing if we didn't get far enough to set up names */
    if ((gp->tname == NULL) || (gp->tbase == NULL)) return;
    /* The temporary restart file is always junk,
    ** but it might be around if we crashed between creating it,
    ** and moving it into place as the genuine restart file.
    ** Remove it if it exists.
    */
    strcpy(gp->tbase, RESTARTEMP);
    remove(gp->tname);
    /* Stop cleaning if there is an explicit request to leave things around,
    ** or if we are going to exit non-zero and aren't just checking.
    */
    /* if ((gp->clean == NEVERCLEAN) || (gp->ecode && !gp->check)) return; */
    if (gp->clean == NEVERCLEAN) return;
    /* OK to (try to) remove restart file and temp directory */
    strcpy(gp->tbase, RESTART);
    remove(gp->tname);
    if (gp->justs && (gp->ecode == 0)) return;
    gp->tbase[-1] = '\0';		/* Clear / to reference tempdir */
    if (rmdir(gp->tname)) {		/* Might fail if non-empty */
	gp->tbase[-1] = SLASH;	/* Restore / to tempdir */
	/* Make sure EVERY temporary file has a chance to be removed.
	** This could take a while, so only do it when the first rmdir fails.
	*/
	for (io->indx = 0; io->indx <= gp->nexto; io->indx++) {
	    settmpname(bp);
	    remove(io->name);
	}
	gp->tbase[-1] = '\0';	/* Turn back into tempdir reference */
	rmdir(gp->tname);		/* Try again, give up if unsuccessful */
    }
    gp->tbase[-1] = SLASH;		/* Restore / (though done using) */
    return;
}

void
exitclean(int notused)
{
    docleanup(&G);
    return;
}

void
handle(int signum)
{
    struct sigaction sa;
    struct sigaction so;

    if ((sigaction(signum, 0, &so) == 0) && (so.sa_handler != SIG_IGN)) {
	memset((void *)(&sa), 0, sizeof(sa));
	sa.sa_flags = SA_RESETHAND;
	sa.sa_handler = &exitclean;
	sigaction(signum, &sa, 0);
    }
    return;
}


/* Reverse the pointers at p, and return a pointer to them.
**
** Leave open the option of reversing in place,
** or reversing into the ``other'' array.
*/

static gptr *
reversebatch(glob *gp, gptr *p)
{
    long n;
    gptr *to;

    to = (p == gp->m1ptr) ? gp->m2ptr : gp->m1ptr;
    to += n = gp->brecs;
    while (--n >= 0) *--to = *p++;
    return (to);
}


static void
sortbatch(glob *gp)
{
    long n;
    gptr *p;

    n = gp->brecs;

    if (n > 0) {
	prepsaptrs(gp);				/* Set up m[12]ptr arrays */
	if ((p = bmerge(gp, gp->cmprf)) == NULL) {
	    p = (gp->fmtyp == FMTFIX) ? frsort(gp) : vrsort(gp);
	}
	if (gp->revrs) p = reversebatch(gp, p);
	n = gp->brecs;
	do {
	    if (*p) WRITEG(gp, *p);		/* Write record if present */
	    p++;				/* Advance to next */
	} while (--n);
    }
    closeio(&(gp->obtch));			/* Close file just written */
    arenareset(gp);				/* Reset for next batch */
    return;
}


static batch *
newbatch(glob *gp)
{
    batch *bp, **bpp;
    long bi, left, n;

    bi = gp->atbtp;			/* how many batches in this load */
    if (bi <= 0) return(NULL);		/* if none, we're done */
    bp = gp->btchp[bi-1];		/* last entry in current array */
    left = !(bp->flag & BATCH_EOF);	/* left == 1 iff last still active */
    /* Close everything still open (except maybe last) */
    for (bpp = gp->btchp, n = bi - left; --n >= 0; bpp++) {
	if (!((*bpp)->flag && BATCH_CLS)) closeio(*bpp);
    }
    if ((gp->atbtp = left) > 0) {
	*(gp->btchp) = bp;
	bp->auxbp = gp->dynsp + gp->drecs;
	batchreset(bp);
    } else bp = 0;
    return (bp);
}


static void
batchreset(batch *bp)
{
    glob *gp = bp->com;

    if (!(bp->flag & BATCH_MAP)) bp->bufp = bp->auxbp;
    bp->auxbp = gp->dynsp + gp->drecs;
    bp->total = gp->dleft;
    bp->curp = (gptr) (bp->plast = gp->plcur);
    return;
}


static void
includebatch(batch *bp)
{
    glob *gp = bp->com;
    uch *recp, **mptr;
    long m, n, k;

    n = bp->parsed + (bp->batch.ptr - bp->bufp);
    m = ROUNDUP(n, bp->aunit);
    n = bp->sumc;
    gp->dleft -= m + n * gp->prohd;
    switch (gp->fmtyp) {
    case FMTFIX:
	mptr = (uch **) gp->plcur;
	k = gp->grecz;
	for (recp = bp->batch.ptr; --n >= 0; recp += k) *(--mptr) = recp;
	gp->plcur = (ptrlen *) mptr;
	break;
    default:
	gp->plcur = (ptrlen *) (bp->curp);
	break;
    }
    if ((bp->flag & BATCH_MAP) == 0) {
	gp->drecs += m;
	if (bp->flag & BATCH_EOF) {
	    gp->btchi--;
	    gp->atbtp--;
	}
    }
    bp->sumc = 0;
    bp->parsed = 0;
}


/* The central sort phase.
**
** There are several loops, which don't nest naturally.
** The outmost loop runs through the command line input files.
** The inner loop tries to fill memory with records,
** and sort them when no more will fit.
**
** If input files are large, it may take several passes through
** the inner loop to consume a single file.  If input files are
** relatively small, it may take several inputs to fill memory.
** If input files are very small, and we are memory mapping,
** so we have to keep files open until the sorting is completed,
** we may run out of file descriptors before we fill memory.
** It is common to exhaust memory part way through an input file.
** These complications make the flow a bit contorted.
*/

static void
sort(glob *gp)
{
    long nbatch;
    batch *bp;

    gp->prohd = gp->ptohd + 2 * sizeof(uch *);
    nbatch = gp->nexto;		/* usually 0, unless restarting */
    arenareset(gp);		/* start with an empty arena */
    gp->btchp = gp->btchb;
    gp->atbtp = 0;
    /* Initial checkpoint, before anything has happened */
    if (gp->ckopt == NULL) ckptsort(gp);
    while (gp->nexti < 0) {
        if (openin(gp, 1) == 0) {
	    if (gp->brecs > 0) goto dobatch;
	    sprintf(gp->emesg, "Out of file descriptors in sort phase");
	    fatal(gp);
	}
	bp = gp->btchp[gp->atbtp - 1];
	for (;;) {			/* load up another batch */
	    extend_batch(bp);		/* extend while possible */
	    includebatch(bp);		/* add what we got into batch */
	    /* If we hit EOF, try to start another batch.  */
	    if (bp->flag & BATCH_EOF) break;
	    /* So we ran out of space.
	    ** Sort what we have and continue reading
	    ** if there is a non-NULL batch pointer.
	    */
	    if (gp->brecs == 0) {
		sprintf(gp->emesg,
		    "Oversized record > %ld, rerun with increased -y\n",
		    bp->left);
		fatal(gp);
	    }
dobatch:
	    opennewtemp(gp);	/* Ran out of space, open temp for batch */
	    sortbatch(gp);	/* sort this batch, start another */
	    nbatch++;
	    ckptsort(gp);
	    if ((bp = newbatch(gp)) == NULL) break;
	}
    }
    if ((nbatch == 0) && (gp->justs == 0)) { /* Everything fit in one batch */
	beginfinalout(gp, 0);
	sortbatch(gp);
	finishfinalout(gp);
	gp->nomrg = 1;
    } else if (gp->brecs > 0) {
	opennewtemp(gp);
	sortbatch(gp);
    }
    return;
}


/* Report nonsensical gp->strip option */

static void
striperr(glob *gp)
{
    errno = 0;
    sprintf(gp->emesg, "Strip option would eliminate all data");
    fatal(gp);
}


/* When restarting from a checkpoint, assorted numerical values
** are needed to pass ``state'', like how many bytes have already
** been read from the first sort input file.  Some values are
** only needed for the sort phase, others only for the merge,
** but it's easiest to pass all of them all the time.
**
** We pass them as a colon-separated list of numbers.
** Position in the list corresponds to position in the State array.
** The code will, in some sense, self-correct if elements in the
** array are added or deleted or moved, but any checkpoint files
** already in existence may break if rerun with a modified array.
**
** The option is intended for checkpoint/restart support,
** not for interactive use.
*/

static long *State[] = {
    &G.nexto,
    &G.nexti,
    &G.lastg,
    &G.focpy
};

static verylong *VLState[] = {
    &G.ckoff
};


/* Process format option argument
**
** N is a (non-empty) digit string
**
** fN[:N[:N]]	fixed-length records whose length must be specified.
**		key length and key offset are optional.
**		For example:
**		    f10		for 10-byte records, or
**		    f10:5:2	for 10-byte records with 5-byte keys that
**				start after the first 2 bytes of each record.
**
** n[N[:N]]	newline-delimited records.
**		An optional upper limit on key length and
**		optional key offset may be specified.  For example
**		    n		records (treated as all key).
**		    n5		records with at most 5 bytes treated as key.
**
** h		header-delimited records.  No options for now.
*/

static int
doformat(glob *gp, char *p)
{
    int saverr;
    char *q;

    errno = 0;
    switch (*p++) {
    case 'f':
	gp->fmtyp = FMTFIX;
	gp->grecz = strtolk(p, &q, 0);
	saverr = errno;
	if (*q == SEP) {
	    gp->gklen = strtolk(q+1, &q, 0);
	    saverr |= errno;
	    if (*q == SEP) {
		gp->gkoff = strtolk(q+1, &q, 0);
		saverr |= errno;
	    }
	} else gp->gklen = gp->grecz;
	if (saverr || *q || (gp->grecz <= 0) || (gp->gklen < 0) || (gp->gkoff < 0) ||
	    (gp->grecz < (gp->gklen + gp->gkoff))) {
	    errno = 0;
	    sprintf(gp->emesg,
		"Invalid format spec ``%.100s'' for fixed length records", p);
	    diag(gp, 0);
	    return(1);
	}
	break;
    case 'n':
	gp->fmtyp = FMTNEW;
	gp->gklen = strtolk(p, &q, 0);
	saverr = errno;
	if (*q == SEP) {
	    gp->gkoff = strtolk(q+1, &q, 0);
	    saverr |= errno;
	}
	if (saverr || *q || (gp->gklen < 0) || (gp->gkoff < 0)) {
	    errno = 0;
	    sprintf(gp->emesg,
	    "Invalid format spec ``%.100s'' for newline-delimited records", p);
	    diag(gp, 0);
	    return(1);
	}
	break;
    case 'h':
	gp->fmtyp = FMTHDR;
	if (*p) {
	    errno = 0;
	    sprintf(gp->emesg,
	    "Invalid format spec ``%.100s'' for header-delimited records", p);
	    diag(gp, 0);
	    return(1);
	}
	break;
    default:
	errno = 0;
	sprintf(gp->emesg, "Invalid format spec ``%.100s''", p-1);
	diag(gp, 0);
	return(1);
    }
    return(0);
}


/* Process options and arguments.
**
** Use getopt() for portability.
*/

#ifndef	GETOPT
extern int	optind;
extern char	*optarg;
extern int getopt(int argc, char *const *argv, const char *optstring);
#endif

static void
doargs(glob *gp, int argc, char *const *argv)
{
    int c, sawerr = 0;
    char *p;

    optind = 1;  /* reset after use in Hancock program */
    while ((c = getopt(argc, argv, "cCiIjmrsSuvb:f:D:o:p:T:x:y:z:")) != EOF) {
	errno = 0;
	switch (c) {
	case 'b':
	    if (((gp->tunit = strtolk(optarg, &p, 0)) < 512) || *p) {
		sprintf(gp->emesg,
		    "Invalid blocksize specification ``%.100s'', using %ld",
					optarg, REGFILE_TUNIT);
		diag(gp, 0);
		gp->tunit = REGFILE_TUNIT;
	    }
	    break;
	case 'c':
	    gp->check++;
	    break;
	case 'C':
	    gp->check += 2;
	    break;
	case 'i':
	    gp->repti++;
	    break;
	case 'I':
	    gp->igerr++;
	    break;
	case 'j':
	    if (gp->merge) {
noop:
		strcpy(gp->emesg, "-j and -m options are incompatible");
		diag(gp, 0);
		sawerr++;
	    }
	    gp->justs = 1;
	    break;
	case 'm':
	    if (gp->justs) goto noop;
	    gp->merge = 1;
	    break;
	case 'r':
	    gp->revrs = 1;
	    break;
	case 's':
	    gp->strip++;
	    break;
	case 'S':
	    gp->strip += 2;
	    break;
	case 'u':
	    gp->uniqu = 1;
	    break;
	case 'v':
	    fprintf(stderr, "%s\n", RcsID);
	    exit(1);

	case 'D':
	    gp->dname = optarg;
	    break;
	case 'f':
	    gp->ckfmt = optarg;
	    if (doformat(gp, optarg)) sawerr++;
	    break;
	case 'o':
	    gp->final = (strcmp(optarg, STDINNAME) == 0) ? NULL : optarg;
	    break;
	case 'p':
	    gp->ckpol = optarg;
	    if (dopolicy(gp, optarg)) sawerr++;
	    break;
	case 'T':
	    gp->dlist = optarg;
	    break;
	case 'x':
	    gp->ckopt = optarg;
	    break;
	case 'y':
	    gp->ckmem = optarg;
	    gp->memax = strtolk(optarg, &p, 0);
	    if (errno || *p) {
		sprintf(gp->emesg, "Invalid memory specification ``%.100s''",
					optarg);
		diag(gp, 0);
		sawerr++;
	    }
	    if (gp->memax > MAXMEM) gp->memax = MAXMEM;	/* Complain?? */
	    else if (gp->memax < MINMEM(gp)) gp->memax = MINMEM(gp);
	    break;
	case 'z':
	    gp->maxrz = strtolk(optarg, &p, 0);
	    if (errno || *p) {
		sprintf(gp->emesg, "Invalid width specification ``%.100s''",
					optarg);
		diag(gp, 0);
		sawerr++;
	    }
	    break;
	default:
	    sawerr++;
	    break;
	}
    }
    if (sawerr) {
	errno = 0;
	sprintf(gp->emesg, "One or more errors processing arguments");
	fatal(gp);
    }



    /* If a final output file name was supplied,
    ** stat() it, to see if it already exists.
    ** If it does, we have to check to see if it is also an input,
    ** lest we might clobber the input when opening the output.
    ** If the stat() fails for any reason, we'll regard the file
    ** as not existing, and leave the status as ``safe to write''.
    ** The stat() might fail for other reasons (like a missing
    ** directory in the path), but it's hard to imagine how an
    ** existing file will be put in peril if stat() fails,
    ** and checking error codes is pretty ugly.
    ** The mode of the file is preserved unless it has already been set,
    ** presumably by restart options.
    */
    if (gp->final) {
	gp->finst.st_mode = OPENFINALMODE;
	if (STAT(gp->final, &(gp->finst)) == 0) {
	    gp->safst = SAFEOUT_UNKNOWN;
	}
    }


    /* Arrange to access arguments backwards through gp->argnd.
    ** When restarting from a checkpoint, accept gp->nexti from getckstate().
    ** Otherwise, if there are no arguments, fake a reference to stdin.
    */
    gp->argnd = argv + argc;
    if (gp->ckopt) getckstate(gp);
    else if ((gp->nexti = optind - argc) >= 0) {	/* No arguments */
	gp->nexti = -1;
	gp->argnd = (char *const *)(&StdinName) + 1;
    }
    switch (gp->fmtyp) {
    case FMTFIX:
	if (gp->strip && (gp->grecz <= (gp->gklen + gp->gkoff))) striperr(gp);
	gp->maxrz =	gp->minrz = gp->avgrz = gp->grecz;
	gp->parsf =	fixparse;
	gp->writf =	writefix;
	gp->strpf =	(gp->strip) ? stripfix : writefix;
	gp->cmprf =	(gp->gkoff > 0) ? fwdfixoff : fwdfix;
	gp->ptohd =	0;
	gp->gstep =	gp->grecz;
	setcopytype(gp, 0);
	break;
    case FMTNEW:
	if (gp->strip && (gp->gklen == 0)) striperr(gp);
	gp->avgrz =	2;		/* lowball guess */
	gp->minrz =	1;		/* empty line */
	gp->parsf =	newparse;
	gp->writf =	writenew;
	gp->strpf =	(gp->strip) ? stripnew : writenew;
	gp->cmprf = 	fwdhdrnew;
	gp->ptohd = 	(gp->gklen || gp->gkoff) ?
			    (2 * sizeof(ptrlen)) :
			    sizeof(ptrlen);
	gp->gstep =	-gp->ptohd;
	break;
    case FMTHDR:
	gp->avgrz =	2;		/* lowball guess */
	gp->minrz =	2;		/* 0; -- dumb but possible */
	gp->parsf =	newparse;
	gp->grecz =	-1;		/* looking for TERM on 1st parse */
	gp->parsf =	hdrparse;
	gp->writf =	writehdr;
	gp->strpf =	(gp->strip) ? ((gp->strip == 1) ? trimhdr : striphdr)
				: writehdr;
	gp->cmprf = 	fwdhdrnew;
	gp->ptohd =	2 * sizeof(ptrlen);
	gp->gstep =	-gp->ptohd;
	break;
    }
    return;
}


/* Reconstruct command for checkpoint/restart */

static void
undoargs(glob *gp)
{
    char *onedash =  " -";

    fprintf(gp->ckiop, "%s", gp->whomi); /* Begin with the command name */

    /* Start with keyletter options.
    ** We want at most one dash, even if there are multiple options.
    ** Simplify the appearance with a macro.
    */

#define dashchar(C) fprintf(gp->ckiop, "%s%c", onedash, (C)), onedash = ""

    if (gp->check > 0) {
			dashchar('c');
	if (gp->check > 1)	dashchar('c');
    }
    if (gp->merge)		dashchar('m');
    if (gp->revrs)	dashchar('r');
    if (gp->strip > 0) {
			dashchar('s');
	if (gp->strip > 1)	dashchar('s');
    }
    if (gp->uniqu)	dashchar('u');

    /* Now do options with arguments */

    gp->tbase[-1] = '\0';	/* Clear / to turn gp->tname into dirname */
    fprintf(gp->ckiop, " -D %s", gp->tname);
    gp->tbase[-1] = SLASH;	/* Restore slash */

    /* If there was an explicit format option, copy it */
    if (gp->ckfmt) fprintf(gp->ckiop, " -f %s", gp->ckfmt);

    /* If there was an explicit non-default output option, copy it */
    if (gp->final && strcmp(gp->final, STDINNAME))
	fprintf(gp->ckiop, " -o %s", gp->final);

    /* If there was a policy argument, copy it */
    if (gp->ckpol) fprintf(gp->ckiop, " -p %s", gp->ckpol);

    /* If there was an explicit memory limit option, copy it */
    if (gp->ckmem) fprintf(gp->ckiop, " -y %s", gp->ckmem);

    /* Always supply a best-guess record size as of checkpoint time */
    fprintf(gp->ckiop, " -z %ld", gp->maxrz);
}


/* Add file arguments to checkpoint
**
** We add only files from the argument list, those indexed with a
** negative index, not temporary files.  Access to temporary files
** in the command line will always be by index, not by name.
*/

static void
addfiles(glob *gp)
{
    long n = gp->nexti;

    while (n < 0) {
	fprintf(gp->ckiop, " %s", gp->argnd[n]);
	n++;
    }
    fprintf(gp->ckiop, "\n");
    return;
}


/* Process policy option, return non-zero if there are any problems */

static int
dopolicy(glob *gp, char *p)
{
    char *q;
    long n;

    while (*p) switch(*p) {
    case 'x':				/* Checkpoint policy */
	n = strtolk(p+1, &q, 0);	/* get numerical chaser, if any */
	gp->cknot = !n;
	p = q;
	break;
    case 'c':				/* Cleanup policy */
	n = strtolk(p+1, &q, 0);	/* get numerical chaser, if any */
	switch ((int) n) {
	case ALWAYSCLEAN:
	case NEVERCLEAN:
	case DEFAULTCLEAN:
	    gp->clean = n;
	    break;
	default:
	    errno = 0;
	    sprintf(gp->emesg, "Invalid cleanup policy value %ld", n);
	    diag(gp, 0);
	    return(1);
	}
	p = q;
	break;
    default:
	errno = 0;
	sprintf(gp->emesg, "Invalid policy option starting ``%.50s''", p);
	diag(gp, 0);
	return(1);
    }
    return(0);
}


/* Set checkpoint argument from state array */

static void
setckstate(glob *gp)
{
    long **next, **last;

    next = State;
    last = State + sizeof(State)/sizeof(State[0]);
    while ((--last >= next) && (**last == 0L));	/* skip trailing 0's */
    if (next <= last) {				/* at least 1 non-zero */
	fprintf(gp->ckiop, " -x ");		/* here comes the state */
	for (;;) {
	    fprintf(gp->ckiop, "%ld", **next);	/* print value */
	    if (++next > last) break;		/* bail out if no more */
	    fprintf(gp->ckiop, "%c", SEP);	/* separate from next value */
	}
    }
    return;
}


/* Get state array values from checkpoint argument */

static void
getckstate(glob *gp)
{
    long **next, **last, val;
    char *stvec, *stop;

    next = State;
    last = State + sizeof(State)/sizeof(State[0]);
    errno = 0;
    for (stvec = gp->ckopt; *stvec; stvec = stop, next++) {
	errno = 0;
	val = strtol(stvec, &stop, 0);
	if ((next >= last) || errno || (*stop && (*stop != SEP))) {
	    errno = 0;
	    sprintf(gp->emesg, "Invalid checkpoint argument ``%.100s''", stvec);
	    fatal(gp);
	}
	**next = val;
	if (*stop == SEP) stop++;
    }
    return;
}


/* Open checkpoint/restart file, or die trying.
**
** Beware, gp->tname is reset as a side-effect.
** (to the temporary checkpoint file name)
*/

static void
openckpt(glob *gp)
{
    strcpy(gp->tbase, RESTARTEMP);		/* working copy */
    remove(gp->tname);			/* ditch any old version */
    gp->ckiop = fopen(gp->tname, OPENWRITEMODE);
    if (gp->ckiop == NULL) {
	sprintf(gp->emesg, "Open failed on checkpoint/restart file ``%.50s''",
		gp->tname);
	fatal(gp);
    }
    return;
}

/* Close the checkpoint file
**
** In the event of errors, bail out.
** If the close succeeds, rename as atomically as possible.
*/

static void
closeckpt(glob *gp)
{
    strcpy(gp->tbase, RESTART);		/* ``real'' checkpoint file */
    strcpy(gp->tnam2, gp->tname);	/* copy for rename() */
    strcpy(gp->tbase, RESTARTEMP);	/* working copy */
    if (ferror(gp->ckiop) || fclose(gp->ckiop)) {
	sprintf(gp->emesg, "Error on checkpoint/restart file ``%.50s''",
		gp->tname);
	fatal(gp);
    }
    rename(gp->tname, gp->tnam2);	/* replace old with new */
}


/* Do a checkpoint during the sort phase
**
** Except at the very start of the sort,
** we have just finished writing a sort temporary file.
** We were reading from the final batch at btchp when it became
** necessary to sort the current memory load and write it.
** We can restart by repositioning ourselves to the same place,
** and picking up where we left off.
*/

static void
ckptsort(glob *gp)
{
    batch *bp;
    long bi, ndx;

    if (gp->cknot) return;
    ndx = gp->nexti;			/* save, in case we tamper */
    bi = gp->atbtp - 1;
    if ((bi >= 0) && (((bp = gp->btchp[bi])->flag & BATCH_EOF) == 0)) {
	gp->nexti = bp->iofile.indx;
	gp->ckoff = bp->iofile.posn - bp->left;
    } else {
	if (ndx >= 0) return;		/* will catch as merge checkpoint */
	gp->ckoff = 0;
    }

    openckpt(gp);
    undoargs(gp);
    setckstate(gp);
    addfiles(gp);
    closeckpt(gp);
    gp->nexti = ndx;			/* restore, in case we tampered */
    gp->ckoff = 0;
    return;
}


/* Do a checkpoint during the merge phase
**
** We'll do a checkpoint when we first enter the merge phase,
** and another each time we close a merge output.
** Unlike the sort phase, we don't have partially read inputs.
** However, in the process of determining how many files we can merge
** on each pass, we may have opened files that haven't yet been merged.
** So we have to back off gp->nexti temporarily, to account for them,
** and then reset it after the state has been saved in the restart file.
*/

static void
ckptmerge(glob *gp)
{
    if (gp->cknot) return;
    gp->nexti -= gp->atbta;
    openckpt(gp);
    undoargs(gp);
    setckstate(gp);
    addfiles(gp);
    closeckpt(gp);
    gp->nexti += gp->atbta;
    return;
}


/* Do a checkpoint while we are checking order
**
** By default, we don't want to leave junk around when just checking order,
** so do nothing unless NEVERCLEAN is specified.
**
** We'll checkpoint near the start of each input file,
** and whenever we're about to exit abnormally.
** The io structure is for the last record that *was* in order,
** which could be the last record in the previous file.
** So we save gp->nexti around reporting the index.
*/

static void
ckptcheck(glob *gp, ios *io)
{
    long saveindex;

    if (gp->cknot || (gp->clean != NEVERCLEAN)) return;
    saveindex = gp->nexti;
    gp->nexti = io->indx;
    openckpt(gp);
    gp->ckoff = io->posn;
    undoargs(gp);
    setckstate(gp);
    addfiles(gp);
    closeckpt(gp);
    gp->ckoff = 0;
    gp->nexti = saveindex;
    return;
}


/* Merge phase
**
** Simply merging files as they occur is adequate for /bin/sort,
** but our wish for stability complicates matters.  For example,
** suppose we are in the merge pass, with inputs looking something
** like the following:
**              _       _
**             | |     | |
**             | |     | |
**  _ _       _| |     | |
** | | |     | | |     | |
** |_|_| ... |_|_| ... |_|
**  y           b         n
**
** The tall stacks at right represent the results of previous merge passes.
** Suppose we have enough file descriptors available to merge all the
** short stacks AND one or more of the tall stacks.  Ordinarily,
** we break ties on equal elements by (as pictured) treating the
** ``leftmost'' as low, thereby preserving the order of the stacks.
** But the tall stacks were created by merging elements that were
** previously ``to the left of'' the short stacks.  So, when we compare
** across stacks ``from different generations'', we must reverse the
** tiebreaking order.  For example, suppose we have three records
** with equal keys, Ry from the stack at y, Rb from the stack at b,
** and Rn from the final stack, near n.  Suppose further that we
** don't have enough descriptors to include the final stack on this pass.
** As noted, Ry must compare high to both Rb and Rn.  And Rb must compare
** low to Rn.  But, after we have merged Rb and Ry into a new stack,
** we have lost track of their origin.  One key (Rb) must compare low
** to Rn, the other (Ry) high, but the keys are (at this point)
** indistinguishable.  The best I've been able to come up with
** is to prohibit merging ``across generations''.  Since stability
** is a property some users might not care about, it could also be
** made optional.
**
** y is the ``youngest'' unprocessed (or incompletely processed) input
**   file index.  In the code, this will be gp->nexti.
** n is the index of the ``next'' temporary file that will be created.
**   It will be recorded in gp->nexto.
** b is the index of the ``boundary'' between generations.
**   We don't need to record it in the code.
*/


/* When eliminating duplicates (-u option), we must be able to
** detect equality between different records.  Otherwise,
** compares between different merge inputs should never be regarded
** as equal, since stability requires that we treat the key from
** the earlier input as lesser.
*/

#define	MERGECOMPARE(GP,A,B) REVERSENSE(GP,CMPRG(GP,(A)->curp,(B)->curp))

#define	MERGECMP(GP,A,B,SENSE) \
	{ \
	    if ((SENSE = MERGECOMPARE(GP,A,B)) == 0) { \
		SENSE = (A)->tbkr - (B)->tbkr; \
	    } \
	}

#define	UNIQCMP(GP,A,B,TOTAL,UNIQ) \
	{ \
	    if ((TOTAL = UNIQ = MERGECOMPARE(GP,A,B)) == 0) { \
		TOTAL = (A)->tbkr - (B)->tbkr; \
	    } \
	}


/* Insert the (non-empty) batch at btchp+atbtp-1 into its proper place
** in the atbtq (non-empty) batches at btchq.
**
** btchp and btchq may move in the process, and atbtp will always
** decrease by 1 as atbtq increases by 1.  However, we will always
** maintain these relationships:
**
** btchb <= btchp <= (btchp + atbtp) <= btchq <= (btchq + atbtq) <= btcht
**
** When data have a (short) history of being orderly, that is,
** when the new pointer has been inserted at the very start of btchq,
** ``risk'' a single compare to see if that is true again.
** This will save a binary search when the data are orderly,
** and will cost at most one extra compare when they aren't.
**
** After determining where the new element belongs in the btchq array,
** we either shift btchq left (there's always room, since the new element
** is making space) if it belongs in the first half, or shifting right,
** if there IS room and the new item belongs in the last half,
** or we pack the array up against btchb, if we wanted to shift right,
** but there wasn't room.  This will make room for subsequent right shifts.
*/

static void
insert(glob *gp, int knownhi)
{
    register batch **b, **t;
    register long n, k;
    batch **m;
    batch *new, **q;
    long wasatq;

    new = gp->btchp[--(gp->atbtp)];	/* save pointer to new element */
    b = gp->btchq;			/* bottom of (non-empty) array */
    if ((n = gp->atbtq++) == 0) {	/* no other item at btchq */
	gp->order = 1;			/* single element is orderly */
	*(gp->btchq = --b) = new;	/* install at new start */
	return;
    }
    q = b;				/* save original q ... */
    wasatq = n;				/* ... and size thereof */
    if (knownhi) {			/* new known to be > least */
	b++;				/* so we need not check 1st */
	--n;
    } else if (gp->order) {
	MERGECMP(gp, new, *b, k);
	if (k <= 0) {			/* still orderly */
	    *(gp->btchq = --b) = new;	/* install at new start */
	    return;
	}

	/* We guessed wrong, the new element is no longer minimal.
	** We don't have to ``waste'' the compare, though,
	** because we can at least shrink the size of the array
	** that remains be searched.
	*/

	b++;
	--n;
    }

    /* We'll operate under the following loop invariants.
    **
    ** n >= 0 is the number of unchecked, sorted, array elements.
    ** b + n == t
    ** When we exit, b addresses the spot BEFORE WHICH new should go.
    ** (If new exceeds all others, the exit value of b
    ** will be just past the end of the original array.)
    **
    ** When there are an odd number of pointers left to be checked,
    ** we clearly want to check the middle pointer, to split what's
    ** left into equal parts.  But if there are an even number left,
    ** we have a bit of choice.  We ``round down'' in these cases,
    ** which is a tiny win in the case of pre-existing order,
    ** and no worse than the other in the case of random order.
    ** For example, if only 2 pointers remain, we compare against
    ** the first, not the second.  This wins if new is lower than the first,
    ** loses if it is greater than the second.
    */

    t = b + n;
    while (n > 0) {
	m = b + ((n-1)/2);		/* b <= m < t */
	MERGECMP(gp, new, *m, k);
	if (k <= 0) t = m;		/* new < m (cannot be equal) */
	else        b = m+1;		/* new > m */
	n = t - b;
    };


    /* Set about making room for the new element, where it belongs */

    n = b - q;				/* how many elements left of new */
    if (n == 0) {			/* none, looks orderly */
	gp->order = 1;
	*(gp->btchq = --q) = new;
	return;
    } else if (!knownhi) gp->order = 0;	/* no longer orderly */

    k = wasatq - n;			/* how many items right of new */
    if (n <= k) {			/* n in 1st half of original btchq */
	b = q;
	t = b--;
	gp->btchq = b;
	SLAMLEFT(b, t, n);
	*b = new;
	return;
    }

    /* Would LIKE to shift right.  Is there room? */
    if ((t = q + wasatq) < gp->btcht) {		/* yep, there's room */
	/* Shift right, if there are any items to shift.
	** Drop new into place, in any event.
	*/
	b = t++;
	if (k > 0) {
	    SLAMRIGHT(t, b, k);
	}
	*b = new;
	return;
    }

    /* Compress both arrays left, inserting new as we go */
    b = gp->btchb;
    if ((k = gp->atbtp) > 0) {
	t = gp->btchp;
	SLAMLEFT(b, t, k);
    }
    k = wasatq - n;			/* items right of new */
    gp->btchp = gp->btchb;
    gp->btchq = b;
    t = q;
    SLAMLEFT(b, t, n);
    *b++ = new;
    if (k > 0) SLAMLEFT(b, t, k);
    return;
}


/* Open the next n merge input files
**
** There will be at least n files left to open,
** at least n batch structures at the start of btcha to hold them,
** and room for n batch pointers at the start of btchb to address them.
** But it may not be possible to open n at once.
** In particular, the first time this routine is called,
** we don't know how many file descriptors are available,
** so we may run out of descriptors before we open everything.
** Return the number of files successfully opened.
**
** Do all the hard work of setting tiebreakers for equal keys now,
** so we can simply subtract them to break ties when the merge heats up.
*/

static long
mergeopens(glob *gp, long n)
{
    batch **b;
    long i;

    b = gp->btchp = gp->btchb;
    gp->btchi = 0;	/* Start at the beginning of btcha */
    gp->atbtp = 0;	/* no batches at b yet */
    for (i = 0; i < n; i++, b++) {
	if (openin(gp, 0) != 1) break;
	extend_batch(*b);
	SETCURP(*b, 0);
	(*b)->tbkr = REVERSENSE(gp, (*b)->iofile.indx);
    }
    return (i);
}


/* Open n more input files, or die.
**
** We expect to be able to open n more inputs, and aren't
** prepared to handle failure gracefully.
*/

static void
mustopens(glob *gp, long n)
{
    long got;

    got = mergeopens(gp, n);
    if (n != got) {
	sprintf(gp->emesg,
	    "Expected to open %ld input files, got %ld", n, got);
	fatal(gp);
    }
    return;
}


static void
makeopen(glob *gp, long n)
{
    long i;
    batch **b, *ap;

    if (gp->atbta > 0)	{		/* some are already open */
	gp->btchp = b = gp->btchb;	/* things will start here */
	gp->atbtp = 0;			/* nothing there yet */
	i = (n - gp->atbta);		/* shortfall, if we need n */
	if (i > 0) {			/* need to open i more */
	    mustopens(gp, i);		/* just what mustopens does best */
	    b += i;			/* pass those just added */
	} else i = 0;			/* get them all from btcha */
	i = n - i;			/* how many to take from open ones */
	ap = gp->btcha + gp->inbta;	/* base of open batches */
	gp->inbta += i;			/* advance index past what we'll use */
	gp->atbtp += i;			/* account for those we're adding */
	gp->atbta -= i;			/* reduce outstanding unused ones */
	while (--i >= 0) *b++ = ap++;
    } else {				/* none currently open */
	mustopens(gp, n);
    }
    return;
}


/* Remove temp files among the last n inputs */

static void
mergeclean(glob *gp, long n)
{
    batch *bp = gp->btcha;			/* 1st element will be free */
    long i, nxti;

    /* The next file to be read is the next input index,
    ** less the number of files opened, but not yet read.
    ** Earlier files can be junked if they are temp files (index >= 0).
    */
    if ((nxti = gp->nexti - gp->atbta) > 0) {	/* some temps among inputs */
	if ((i = nxti - n) < 0) i = 0;		/* skip non-temps */
	for (;i < nxti; i++) {
	    bp->iofile.indx = i;
	    settmpname(bp);			/* refresh the name */
	    remove(bp->iofile.name);		/* remove the file */
	}
    }
    return;
}


/* Read next record from this batch.  Return 0 for EOF, +1 for ok. */

static int
readmerge(batch *bp)
{
    while (++(bp->sumi) >= bp->sumc) {		/* used all that were parsed */
	if (bp->flag & BATCH_EOF) {		/* no more forthcoming */
#ifdef	MMAP
	    if (bp->flag & BATCH_MAP) closeio(bp);
#endif
	    return (0);
	}
	bp->sumi = -1;				/* will be incremented above */
	bp->sumc = 0;				/* done with summaries */
	if (bp->couldbe && (PARSEB(bp) > 0)) continue;
	bp->parsed = 0;				/* all have been consumed */
	extend_batch(bp);			/* go after some more */
    }
    SETCURP(bp, bp->sumi);
    return (1);
}


/* Merge the inputs that are open (but unread) at gp->btchp
**
** The overall flow is this:
** We have an array of atbtp pointers at btchp whose next elements
** are in no predictable sorted order.  And we have an array of
** atbtq pointers at btchq whose current elements are in sorted order.
** Initially, everything is in the first array.
**
** At each step, we eliminate the pointers at btchp by either
** dropping them, if there is no next element, or incorporating
** them into the sequenced batch at btchq.  If two or more
** elements remain, the least is at btchq, and second place is
** at btchq+1.
**
** Dealing with the unique option and orderly data is complicated.
** See the commentary below.
*/

static void
merge(glob *gp)
{
    batch *bp, *b2;
    long i, j, lo, hi, left;
    long sense, uniq;
    int knownhi;

    i = gp->atbtp;			/* number of inputs being merged */
    gp->btchq = gp->btchp + i;		/* base of sorted inputs */
    gp->atbtq = 0;			/* none sorted, yet */
    knownhi = 0;

    if ((left = i) > 1) for (;;) {
	/* Add any batches at btchp to the sequence at btchq
	** by reading the next record from the last batch.
	** If we hit EOF, drop the batch and move on.
	** Otherwise, insert it into the btchq array.
	*/
	for (i = gp->atbtp; --i >= 0; ) {	/* another btchp item */
	    if (readmerge(gp->btchp[i]) == 0) {	/* EOF */
		gp->atbtp--;		/* one less input to merge */
		if (--left <= 1) break;	/* special-case single input */
	    } else insert(gp, knownhi);
	}
	if (left <= 1) break;


	/* We have totally ordered sequence at btchq.
	** It is non-trivial in the sense that there are at least 2 items,
	** the minimal element (at btchq) and the second place finisher
	** (2PF) at btchq+1.
	**
	** If we don't have to worry about eliminating duplicates,
	** the easiest thing to do is to write the item at btchq,
	** then ``transfer it'' to btchp, so it will be read and
	** re-inserted on the next cycle.  This is precisely what we
	** do when neither gp->uniqu and gp->order are in effect.
	**
	** When gp->uniqu is on, we have to detect and eliminate dupes.
	** Since the batches are sorted, all the duplicates must be
	** adjacent to the least item.  We need only write the least item,
	** find all the dupes, and put them ALL in btchp, so they will
	** be read and re-inserted on the next cycle.
	**
	** When data have a history of being orderly, we can exploit the
	** order by risking a few compares.  In particular, we can
	** look for more than one item in the batch at btchq that is
	** less than or equal to the 2PF.  More binary ramping.
	** We have a chance for double savings over the ``easy'' approach.
	** Each item at btchq lower than the 2PF saves a binary insertion
	** search through the other items in the batches at btchq+1 on,
	** and those we skip over in the ramping saves even the single
	** compare it would otherwise take to ensure it compares low to
	** the 2PF.
	*/


	knownhi = 0;			/* we know nothing about next insert */
	bp = *(gp->btchp = gp->btchq++); /* least batch from btchq to btchp */
	--(gp->atbtq);			/* one less left at btchq */
	WRITEG(gp, bp->curp);		/* write the minimal item */


	if (!(gp->uniqu)) {		/* don't have to deal with dupes */
	    gp->atbtp = 1;		/* only 1st will be read next cycle */
	    if (gp->order) {		/* look for others < 2PF */
		/* lo < 2PF, hi either out of range or > 2PF */
		lo = bp->sumi;		/* known to be < 2PF */
		hi = bp->sumc;		/* just out of range */
		b2 = *(gp->btchq);	/* 2PF */
		for (j = 1 ;; j = j+j) {	/* ramp up */
		    if ((i = lo + j) >= hi) {
			if ((i = hi - 1) > lo) {
			    SETCURP(bp, i);
			    MERGECMP(gp, bp, b2, sense);
			    if (sense < 0) lo = i;
			    else           hi = i;
			}
			break;
		    }
		    SETCURP(bp, i);
		    MERGECMP(gp, bp, b2, sense);
		    if (sense > 0) {
			hi = i;
			break;
		    }
		    lo = i;
		}
		lo++;				/* known to be less than 2PF */
		while ((i = (hi - lo)) > 0) {	/* ramp down */
		    i = lo + (i-1)/2;
		    SETCURP(bp, i);
		    MERGECMP(gp, bp, b2, sense);
		    if (sense < 0) lo = i+1;
		    else           hi = i;
		}
		/* The record at sumi has already been written.
		** Write any others that were found low.
		*/
		for (i = bp->sumi; ++i < lo; ) {
		    SETCURP(bp, i);
		    WRITEG(gp, bp->curp);
		}
		if (hi < bp->sumc) knownhi = 1;
		bp->sumi = i - 1;
	    }
	    continue;
	} else {			/* must detect and eliminate dupes */


	    /* Similar to what was done above,
	    ** but we must detect equality across batches,
	    ** and there cannot be duplicates within batches.
	    ** We check for equality of the initial elements,
	    ** and, if they aren't equal, and things look orderly,
	    ** we go looking for other low elements.
	    ** We arrange things so that if equality is EVER detected,
	    ** ``lo'' indexes the equal element and ``sense'' records
	    ** the tiebroken sense.
	    */
	    lo = bp->sumi;
	    b2 = *(gp->btchq);
	    UNIQCMP(gp, bp, b2, sense, uniq);
	    if (uniq && gp->order) {	/* get fancy, look for more */
		/* lo must now be strictly low, since uniq != 0.
		** Exit the for loop with lo strictly low (if uniq != 0)
		** or equal (if uniq == 0) to 2PF, hi > 2PF (or out of range).
		*/
		hi = bp->sumc;			/* just out of range */
		for (j = 1 ;; j = j+j) {	/* ramp up */
		    if ((i = lo + j) >= hi) {
			if ((i = hi - 1) > lo) {
			    SETCURP(bp, i);
			    UNIQCMP(gp, bp, b2, sense, uniq);
			    if (uniq <= 0) lo = i;
			    else           hi = i;
			}
			break;
		    }
		    SETCURP(bp, i);
		    UNIQCMP(gp, bp, b2, sense, uniq);
		    if (uniq >= 0) {
			if (uniq) hi = i;
			else      lo = i;
			break;
		    }
		    lo = i;
		}


		/* leave lo just past those that are strictly low */
		if (uniq) {			/* didn't detect equality */
		    lo++;			/* so lo was strictly low */
		    while ((i = (hi - lo)) > 0) {	/* ramp down */
			i = lo + (i-1)/2;
			SETCURP(bp, i);
			UNIQCMP(gp, bp, b2, sense, uniq);
			if (uniq > 0)  hi = i;		/* strictly high */
			else if (uniq) lo = i+1;	/* strictly low */
			else {				/* equal */
			    lo = i;
			    break;
			}
		    }
		}


		/* The record at sumi has already been written.
		** Write any others that were found strictly low.
		*/
		i = bp->sumi + 1;
		if (i < lo) {		/* found some */
		    do {
			SETCURP(bp, i);
			WRITEG(gp, bp->curp);
		    } while (++i < lo);
		    bp->sumi = i - 1;
		} else gp->order = 0;	/* not much order evident */

		if (uniq) {		/* didn't end in a tie */
		    /* If we didn't run off the end, we must have been high */
		    if (lo < bp->sumc) knownhi = 1;
		} else {		/* ended in a tie */
		    bp->sumi = lo;	/* be sure to index ... */
		    SETCURP(bp, lo);	/* ... and address it, for compares */
		    if (sense < 0) {	/* bp was the lesser of two equals */
			WRITEG(gp, bp->curp);
		    } else {		/* 2PF was the lesser */
			WRITEG(gp, b2->curp);
		    }
		}
	    }


	    if (uniq == 0) {		/* Ended in a tie */
		/* Put ALL the batches with equal 1st records into the
		** btchp array, for reading and insertion.
		** We COULD do binary ramping here, too, but the
		** number of batches will not usually be large.
		*/
		i = gp->atbtq - 1;	/* number left following 2PF */
		gp->btchq++;		/* skip 2PF */
		while (i > 0) {		/* still some left at btchq */
		    if (MERGECOMPARE(gp, bp, *(gp->btchq))) break; /* != */
		    --i;
		    gp->btchq++;
		}
		gp->atbtq = i;
		gp->atbtp = gp->btchq - gp->btchp;
	    } else gp->atbtp = 1;
	}
    }


    /* Only one batch left, run it out */
    if (gp->atbtq > 0) {
	bp = *(gp->btchq);
	WRITEG(gp, bp->curp);
    } else bp = *(gp->btchp);
    while (readmerge(bp)) WRITEG(gp, bp->curp);
    return;
}


static long
fiddlimit(long limit, long inputs)
{
    long n, guess, lo, i, k;

    if (limit > MAXBATCH) limit = MAXBATCH;	/* need a batch for each */
    if (limit >= inputs)  limit = inputs;	/* Need at most one per input */
    else {
	/* find least n for which   limit ** n >= inputs */
	n = 1;
	guess = limit;
	do {
	    guess *= limit;
	    n++;
	} while (guess < inputs);

	/* We need n merge passes over the data.
	** find the least limit for which   limit ** n >= inputs
	** We could do this with a single call to pow(),
	** but that may not be universally available,
	** we only do this once per sort, so efficiency is inconsequential,
	** and we hate to pass up an opportunity for a binary search.
	**
	** lo ** n is too low, limit ** n is good enough
	*/

	for (lo = 1; (guess = limit - lo) > 1; ) {
	    guess = lo + guess / 2;
	    for (i = n, k = guess; --i > 0; ) k *= guess;
	    if (k >= inputs) limit = guess;
	    else	     lo = guess;
	}
    }
    return (limit);
}


/* We need to merge files with indexes from gp->nexti to gp->nexto.
** Several things may limit the number of files we can merge in one pass:
** The total number of input files, the number of file descriptors,
** the buffer space that we can carve out of the arena.
**
** Open a temporary output file to ``reserve'' a file descriptor.
** Then start opening input files until something runs out.
*/

static void
mergephase(glob *gp)
{
    long inputs;
    long i, n, limit;
    batch *bp;
    uch *p;

    /* For each merge input, we need a buffer adequate to hold gp->maxrz,
    ** plus generous allocations for record and summary space.
    */

    gp->merge = 1;			/* We're merging now */
    gp->prohd = gp->ptohd;		/* no need for gptrs in merge phase */
    ckptmerge(gp);			/* be ready to restart from here */
    n = ROUNDUP(gp->maxrz + 4 * gp->tunit, DEFAULT_AUNIT);
    gp->fanmx = limit = gp->dsize / n;	/* Can only allocate this many */
    if (limit < 2) {
	errno = ENOMEM;
	sprintf(gp->emesg,
	    "Not enough space in arena, size %ld, for 2 records of size %ld",
	    gp->dsize, gp->maxrz);
	fatal(gp);
    }
    inputs = gp->nexto - gp->nexti;	/* How many input files now */
    limit = fiddlimit(limit, inputs);	/* Tune limits a bit */
    opennewtemp(gp);			/* Reserve output file descriptor */
    n = gp->dsize / limit;		/* rough guess at space for each */
    n = ROUNDDOWN(n, DEFAULT_AUNIT);	/* keep them aunit aligned */

    /* Initialize buffer space for the first ``limit'' batches at btcha
    ** (which are all that will ever be used).
    */
    gp->btchi = 0;
    bp = gp->btcha;
    for (i = 0, p = gp->dynsp; i < limit; i++, bp++, p += n) {
	bp->com = gp;
	bp->auxbp = p;
	bp->total = ROUNDDOWN(n, sizeof(ptrlen));
	bp->plast = (ptrlen *)(p + bp->total);
    }


    gp->fanin = limit = mergeopens(gp, limit);	/* Open as many as possible */
    if ((limit < 2) && (limit < inputs)) {
	sprintf(gp->emesg, "Unable to open 2 merge descriptors");
	fatal(gp);
    }
    gp->atbtp = 0;			/* Lie, makeopen will get them */
    gp->atbta = limit;			/* ... courtesy of these values */
    gp->inbta = 0;

    for (gp->mpass = 1;;gp->mpass++) {
	/* If there are still inputs from a previous generation,
	** whittle away at them.  Otherwise, regard all remaining inputs
	** as a new generation.  If the new generation can be done
	** in a single pass, break out.
	*/
	if (gp->lastg == 0) {
	    gp->lastg = inputs;
	    if (gp->lastg <= limit) break;
	}
	/* We need to reduce the number of inputs by merging into
	** temporary files.  We will assume that we can always open
	** ``limit'' input files, and merge them onto a temp file.
	** The goal is reduce the number to or below limit,
	** then finish the phase off by merging onto the final output.
	**
	** If we didn't have to worry about stability, we could just
	** merge ``limit'' inputs onto a new temporary, and repeat.
	** But, as noted above, we must be careful about mixing any
	** of the new temporaries with existing ones, so, instead,
	** we merge all existing temporaries onto a new set of temporaries
	** on each iteration through this loop.
	**
	** We try to merge roughly the same number of inputs on each pass,
	** mostly on aesthetic grounds.
	*/
	i = CEILING(gp->lastg, limit);	/* Need i (>0) merge passes */
	do {
	    n = CEILING(gp->lastg, i);	/* split inputs evenly over passes */
	    makeopen(gp, n);		/* arrange for n open inputs */
	    merge(gp);			/* merge them */
	    closeio(&(gp->obtch));	/* close this output */
	    ckptmerge(gp);		/* be ready to restart from here */
	    mergeclean(gp, n);		/* tidy up temporary inuts */
	    gp->lastg -= n;		/* n fewer at this generation */
	    inputs  -= n - 1;		/* but one new temp overall */
	    opennewtemp(gp);		/* open temp for next pass */
	} while (--i > 0);
    }


    /* We can now merge all the remaining inputs onto the final output */
    beginfinalout(gp, 1);
    makeopen(gp, gp->lastg);		/* open everything */
    merge(gp);				/* merge them */
    closeio(&(gp->obtch));		/* close final output */
    finishfinalout(gp);
    if (gp->clean != NEVERCLEAN) mergeclean(gp, gp->lastg);
    return;
}


/* Copy a record from one batch to another, (so we can clobber the from
** batch without losing the record that was copied).
*/

static void
copyrec(batch *top, batch *fromp)
{
    glob *gp = top->com;
    ptrlen *tpp, *fpp;
    uch *trp, *frp;
    long n;

    top->bufp  = top->auxbp;
    switch (gp->fmtyp) {
    case FMTFIX:
	/* Fixed-length records are straightforward.  There will always be
	** adequate space in the to buffer.  Just aim the current pointer
	** at the start of the buffer, and copy the record into place.
	*/
	top->curp = top->bufp;
	MEMCPY(top->curp, fromp->curp, gp->grecz);
	break;

    default:
	/* The other formats are trickier.  We have one or two ptrlens
	** to copy, the last of which tells us where the record is and
	** how long it is.  We dare not copy without checking lengths,
	** and we must relocate the ptrlens to reference the copy.
	** Set both ptrlens beyond the current ptrlen array,
	** deal with the record ptrlen, then the (optional) key ptrlen.
	*/
	tpp = top->plast;
	fpp = (ptrlen *) ((uch *)(fromp->curp) + gp->ptohd);
	(--tpp)->len = n = (--fpp)->len;
	if (gp->fmtyp == FMTNEW) n++;
	if (top->total < (n + gp->ptohd)) {
	    sprintf(gp->emesg,
		"Oversized record (%ld bytes), rerun with increased -z\n", n);
	    fatal(gp);
	}
	frp = fpp->ptr;
	trp = tpp->ptr = top->bufp;
	MEMCPY(trp, frp, n);
	if (gp->ptohd > sizeof(ptrlen)) {
	    (--tpp)->len = (--fpp)->len;
	    tpp->ptr = fpp->ptr - frp + trp;
	}
	top->curp = (gptr)tpp;
	break;
    }
    return;
}


/* Copy a reference to the current record from one batch to another.
** In this case, the original batch is not going to change,
** so we can reference the record in place in the source batch.
*/

static void
copyref(batch *top, batch *fromp)
{
    glob *gp = top->com;
    ptrlen *tpp, *fpp;
    long n;

    switch (gp->fmtyp) {
    case FMTFIX:
	top->curp = fromp->curp;
	break;
    default:
	fpp = (ptrlen *)(fromp->curp);
	n = gp->ptohd;
	tpp = (ptrlen *)(((uch *) (gp->plast)) - n);
	top->curp = (gptr) tpp;
	do *tpp++ = *fpp++; while ((n -= sizeof(ptrlen)) > 0);
	break;
    }
    return;
}


static void
check(glob *gp)
{
    batch *bp, *last;
    uch *p;
    long n, k, sense;

    gp->prohd = gp->ptohd;
    p = gp->dynsp;
    k = gp->maxrz;
    if (k <= 0) k = LARGERECLEN;
    k += gp->ptohd;
    k = ROUNDUP(k, sizeof(ptrlen));
    n = gp->dsize - k;
    n = ROUNDDOWN(n, DEFAULT_AUNIT);
    if (n < ((2 * gp->tunit) + k)) {
	errno = ENOMEM;
	sprintf(gp->emesg,
	    "Unable to allocate two buffers of size %ld to check sequence",
	    gp->maxrz);
	fatal(gp);
    }

    /* Use btcha[0] to hold records from the inputs,
    ** btcha[1] to hold references to the previous record.
    */
    bp = gp->btcha;
    bp->com = gp;
    bp->auxbp = p; p += n;
    n = ROUNDDOWN(n, sizeof(ptrlen));
    bp->total = n;
    bp->plast = (ptrlen *)(bp->auxbp + n);

    last = bp + 1;
    last->com = gp;
    last->auxbp = p;
    last->total = k;
    last->plast = (ptrlen *)(p + k);
    last->curp = NULL;

    while (gp->nexti < 0) {
	mustopens(gp, 1);
	n = 0;
	while (readmerge(bp) > 0) {
	    for (;;) {	/* break out when sumi reaches sumc */
		n++;
		if (last->curp != NULL) {
		    sense = MERGECOMPARE(gp, bp, last);
		    if ((sense < 0) || ((sense == 0) && gp->uniqu)) {
			errno = 0;
			setoffname(bp);
			sprintf(gp->emesg, "%s at record %ld, %s",
				    (sense) ? "Disorder" : "Duplication",
				    n, gp->recon);
			gp->ecode = 1;
			if (gp->check == 1) {
			    ckptcheck(gp, &(bp->iofile));
			    fatal(gp);
			}
			diag(gp, 0);
		    }
		    if (n == 2) ckptcheck(gp, &(bp->iofile));
		}
		if (++(bp->sumi) >= bp->sumc) break;
		copyref(last, bp);
		SETCURP(bp, bp->sumi);
	    }
	    copyrec(last, bp);
	}
	if (gp->merge) last->curp = NULL;	/* disorder ok at boundaries */
    }
    return;
}


/* One-time initialization of non-zero global values */

static void
globinit(glob *gp)
{
    batch *bp, **bpp;
    long n;

    gp->tunit = REGFILE_TUNIT;		/* transfer unit for regular files */
    gp->memax = DEFMEM;			/* Establish a good default */
    gp->abetz = sizeof(ALPHABET) - 1;	/* # characters in alphabet */
    gp->fmtyp = FMTNEW;			/* Default, if -f is missing */
    gp->safst = SAFEOUT_SAFE;		/* Safe to clobber final out */
    gp->btchp = bpp = gp->btchb;	/* base at bottom of array */
    gp->btcht = bpp + BIGBATCH;		/* top of array of batch pointers */
    gp->obufp = gp->obuff;		/* start of output buffer */
    gp->obtch.com = gp;
    for (n = MAXBATCH, bp = gp->btcha; --n >= 0; ) *bpp++ = bp++;
    handle(SIGINT); handle(SIGHUP); handle(SIGTERM); handle(SIGPIPE);
    return;
}


int
amrRSORT(int argc, char *const *argv)
{
    glob *gp = &G;

    gp->whomi = argv[0];
    globinit(gp);
    doargs(gp, argc, argv);
    dotempdir(gp);

    if (gp->focpy) {			/* Restarting at final move/copy */
	gp->safst = SAFEOUT_UNSAFE;	/* Must have been true originally */
	gp->obtch.iofile.indx = gp->nexto;	/* Saved in finishfinalout */
	finishfinalout(gp);		/* Try, try again */
    } else {
	arenalloc(gp);
	if (gp->merge || gp->check) {	/* Skip sort phase, merge or check */
	    if (gp->maxrz == 0) gp->maxrz = LARGERECLEN; /* take a stab */
	    if (gp->check) {
		gp->nomrg = 1;		/* Neither sort nor merge */
		check(gp);
	    }
	} else {
	    gp->srtng = 1;
	    sort(gp);
	    gp->srtng = 0;
	}
	if ((gp->nomrg == 0) && (gp->justs == 0)) mergephase(gp);
    }

    docleanup(gp);
    if (gp->repti) summarize(gp);
    return (gp->ecode);
}


/* Radix sort for fixed-length things.
**
** The array of gptrs at m1ptr addresses fixed-length records.
** Sort them and leave them in m1ptr, with m2ptr available for scratch use.
**
** There may be a non-zero global gkoff within the records,
** but that is accommodated in the initial stack item.
** The length of all keys will be in grecz.
** We don't have to worry about empty strings.
**
** If gp->uniqu is selected, pointers to redundant records will be set to 0.
*/

typedef struct { long off, len, ndx; } item;
#define	STACKINCR (4096)
#define	PUSH(OFF, LEN, INDEX) { \
    if (++itemi >= items) { \
	stack = (item *) realloc((void *) stack, \
	sizeof(item) * (items += STACKINCR)); \
	if (stack == NULL) { \
	    sprintf(gp->emesg, \
		"Unable to grow radix sort stack to %ld items", items); \
	    fatal(gp); \
	} \
	sp = stack + itemi - 1; \
    } \
    sp->off = (OFF); \
    sp->len = (LEN); \
    (sp++)->ndx = (INDEX); \
}

#define POP(OFF, LEN, INDEX) { \
    if (--itemi < 0) break; \
    OFF = (--sp)->off; \
    LEN = sp->len; \
    INDEX = sp->ndx; \
}


static gptr *
frsort(glob *gp)
{

/* The 1st push will populate the stack with available items */
static item *stack;	/* Base of stack (may be reallocated) */
long itemi = 0;		/* How many items are in use in the stack */
static long items;	/* How many items can the stack hold */
item *sp = stack;	/* Next free item (stack[itemi]) */

    long n, nc, j;
    uch **mn, **tm, **ak, **aj, **pile[UCHAR_MAX + 1];
    int c, cmin;
    long *cp;
    static long count[UCHAR_MAX + 1];
    long at, ln, ix, lastix;
/* long p1 = 0, p3 = 0; */

    ln = gp->brecs;
    lastix = gp->gkoff + gp->gklen - 1;
    mn = (uch **) gp->m1ptr;
    tm = (uch **) gp->m2ptr;
    PUSH(0, ln, gp->gkoff)
    for (;;) {				/* POP breaks out on empty */
	POP(at, ln, ix)
	nc = 0;
#ifdef notdef
	if ((ln < THRESHOLD) &&		/* not many items left to sort ... */
	    (ix < lastix)) {		/* and more than one radix pass left */
	    simplesort(mn, tm, at, ln, ix);
	    continue;
	}
#endif
	cmin = UCHAR_MAX;		/* Least char seen */
	/* Tally characters, keep track of least char, and # different chars */
	for (ak = mn + at, n = ln; --n >= 0; ak++) {
	    c = (*ak)[ix];
	    if (count[c]++ == 0) {
		if (cmin > c) cmin = c;
		nc++;
	    }
	}


	/* A single pile needs no reorganization.
	** Things with leading blanks or 0's (binary or ascii)
	** may make the single pile check worthwhile.  Or maybe not.
	** It's easy to take out.
	**
	** ak was left pointing just past the array of pointers
	** sharing the identical byte.
	*/
	if (nc == 1) {
	    if (ix == lastix) {
		if (gp->uniqu) {
		    if (gp->revrs) --ak;	/* Spare last rather than 1st */
		    while (--ln > 0) *--ak = 0;
		}
	    } else {
		PUSH(at, ln, ix+1);
	    }
	    count[cmin] = 0;
/* p1 += ln; */
	    continue;
	}
/* p3 += ln; */


	/* We have at least two non-empty piles.
	** Figure out where each pile belongs.
	** Stack piles for which work remains.
	*/
	for (j = nc, n = at, cp = count + (c = cmin);; c = cp - count) {
	    if ((*cp > 1) && (ix < lastix)) {
		PUSH(n, *cp, ix+1)
	    }
	    pile[c] = tm + n;
	    n += *cp;
	    *cp = 0;
	    if (--j == 0) break;
	    while (*++cp == 0);
	}
	for (ak = mn + at, n = ln; --n >= 0; ak++) {	/* deal into temp */
	    *(pile[(*ak)[ix]])++ = *ak;
	}


	ak = mn + at;
	aj = tm + at;
	if (gp->uniqu && (ix >= lastix)) {
	    /* When we finished dealing the piles,
	    ** the pile pointers ended up immediately after their
	    ** corresponding pile, at the start of the next pile.
	    ** Pick the first or last representative from each pile,
	    ** as determined by -r, and clear pointers, if any,
	    ** corresponding to duplicated elements.
	    */
	    ln -= nc;	/* First nc items are representative choices */
	    for (c = cmin;; c = (*aj)[ix]) {
		/* aj addresses the start of this pile,
		** pile[c] the start of the next;
		*/
		if (gp->revrs) {
		    aj = pile[c];
		    *ak++ = *(aj - 1);	/* last item on previous pile */
		} else {
		    *ak++ = *aj;	/* first item on this pile */
		    aj = pile[c];
		}
		if (--nc <= 0) break;
	    }
	    while (--ln >= 0) *ak++ = 0;
	} else while (--ln >= 0) *ak++ = *aj++;
    }
/* fprintf(stderr, "%ld records, %ld 1-pass-items, %ld 3-pass-items, %ld item-passes\n", gp->brecs, p1, p3, p1+ 3*p3); */
    return (gp->m1ptr);
}


/* Radix sort for variable-length things.
**
** The array of gptrs at m1ptr addresses ptrlens defining the keys.
** Sort them and leave them in m1ptr, with m2ptr available for scratch use.
**
** This is in many ways similar to the fixed-length radix sort,
** but we have an extra pile for the keys that have terminated,
** and we have to check termination on a key-by-key basis.
**
** If -u is selected, pointers to redundant records will be set to 0.
*/


/* Programmer's Notes:
**
** There is an extra level of indirection involved in the variable-length
** code, relative to the fixed-length code.  The array of pointers point
** to ptrlens, rather than to the records themselves.  Getting to the key
** requires an access to the ptrlen array, and another to the record data.
** Since both the ptrlen array and record data are scattered about,
** neither is likely to be present in a small cache.  If key data were
** copied in with the ptrlen info, it wouldn't be necessary to go to
** record data as often, which might result in a performance improvement.
** However, this doesn't help with merge sorting, or with the merge phase,
** and might even hurt some by bloating the summary structures.
** So it's worth thinking about, but probably not worth implementing (yet).
*/


static gptr *
vrsort(glob *gp)
{

static item *stack;	/* Base of stack (may be reallocated) */
long itemi = 0;		/* How many items are in use in the stack */
static long items;	/* How many items can the stack hold */
item *sp = stack;	/* Next free item (stack[itemi]) */
#define	EINDEX	(UCHAR_MAX + 1)
#define	KEYOF(P) ((*(P))->ptr)
#define	LENOF(P) ((*(P))->len)

    long n, nc, j;
    ptrlen **mn, **tm, **ak, **aj, **pile[UCHAR_MAX + 2];
    int c, cmin;
    long *cp;
    static long count[UCHAR_MAX + 2];
    long at, ln, ix;
/* long p1 = 0, p3 = 0; */

    ln = gp->brecs;
    mn = (ptrlen **) (gp->m1ptr);
    tm = (ptrlen **) (gp->m2ptr);
    PUSH(0, ln, 0)
    for (;;) {				/* POP breaks out on empty */
	POP(at, ln, ix)
	nc = 0;
#ifdef notdef
	if (ln < THRESHOLD) {		/* not many items left to sort */
	    simplesort(mn, tm, at, ln, ix);
	    continue;
	}
#endif
	cmin = UCHAR_MAX + 1;		/* Least char seen */
	/* Tally characters, keep track of least char, and # different chars */
	for (ak = mn + at, n = ln; --n >= 0; ak++) {
	    c = (LENOF(ak) > ix) ? KEYOF(ak)[ix] : EINDEX;
	    if (count[c]++ == 0) {
		if (cmin > c) cmin = c;
		nc++;
	    }
	}
	/* A single pile needs no reorganization.
	** Things with leading blanks or 0's (binary or ascii)
	** may make the single pile check worthwhile.  Or maybe not.
	** It's easy to take out.
	**
	** ak was left pointing just past the array of pointers
	** sharing the identical byte.
	*/
	if (nc == 1) {
/* p1 += ln; */
	    if (cmin == EINDEX) {	/* end of the line for this pile */
		if (gp->uniqu) {
		    if (gp->revrs) --ak;	/* Spare last rather than 1st */
		    while (--ln > 0) *--ak = 0;
		}
	    } else {
		PUSH(at, ln, ix+1);
	    }
	    count[cmin] = 0;
	    continue;
	}
/* p3 += ln; */


	/* We have at least two non-empty piles.
	** Figure out where each pile belongs.
	** Special-case the pile of exhausted keys, if any.
	** Stack other non-singleton piles, for which work remains.
	*/
	n = at;
	if ((j = count[EINDEX]) > 0) {
	    pile[EINDEX] = tm + n;
	    n += j;
	    --nc;
	}
	for (cp = count + (c = cmin);; c = cp - count) {
	    if (*cp > 1) {
		PUSH(n, *cp, ix+1)
	    }
	    pile[c] = tm + n;
	    n += *cp;
	    *cp = 0;
	    if (--nc == 0) break;
	    while (*++cp == 0);
	}

	/* Deal into temp.
	** We only have to check key lengths while there are
	** still exhausted keys outstanding.
	*/

	ak = mn + at;
	n = ln;
	if ((j = count[EINDEX]) > 0) {
	    while (--n >= 0) {
		if (LENOF(ak) <= ix) {
		    *(pile[EINDEX]++) = *ak++;
		    if (--j <= 0) break;
		} else *(pile[KEYOF(ak)[ix]])++ = *ak, ak++;
	    }
	}
	while (--n >= 0) {
	    *(pile[KEYOF(ak)[ix]])++ = *ak, ak++;
	}

	ak = mn + at;
	aj = tm + at;
	if ((j = count[EINDEX]) > 0) {
	    count[EINDEX] = 0;
	    if (gp->uniqu) {
		*ak++ = (gp->revrs) ? *(aj + j - 1) : *aj;
		aj += j;
		ln -= j;
		while (--j > 0) *ak++ = 0;
	    }
	}
	do { *ak++ = *aj++;} while (--ln > 0);
    }
/* fprintf(stderr, "%ld records, %ld 1-pass-items, %ld 3-pass-items, %ld item-passes\n", gp->brecs, p1, p3, p1+ 3*p3); */
    return (gp->m1ptr);
}


/* Assign all remaining available bytes to summary structures beyond
** the sumc already allocated.  If there is no per-record overhead,
** use a very large number.
*/

static void
setsumx(batch *bp, long avail)
{
    long prohd = bp->com->prohd;

    bp->sumx = (prohd > 0) ? (bp->sumc + (avail / prohd)) : bp->total;
    return;
}


/* Add another parsed record to the batch.
**
** The ptrlen at bp->batch defines an area, either of mapped address space
** or of main memory, available for holding records.  A batch of records
** may already occupy part of that space.  If so, bp->left will be positive,
** the number of bytes that must be preserved from the previous batch.
**
** Leave the batch ptrlen addressing the old data (if any)
** and new additions (if any).
*/

static void
extend_batch(batch *bp)
{
    glob *gp = bp->com;
    long m, n, k;
    offset o;
    unsigned char *p, *q;

    /* If we hit EOF, persist in saying so by an immediate return */

    if (bp->flag & BATCH_EOF) return;

    /* We aren't (knowingly) at EOF, so we'll have to go after more data.
    ** HOW we go after the data varies significantly between memory-mapped
    ** files (where we ``move windows over data'') and normal files
    ** (where we move and read data in a buffer).
    */


    q = bp->batch.ptr;		/* start of data, parsed and unparsed */
    q += n = bp->batch.len;	/* skip past what is there */
    k = bp->left;		/* how much of THAT is leftovers */
    bp->batcho += n - k;	/* non-leftovers are part of batch */


    /* Set q to the place where reading would commence.
    **
    ** Often, when we are not in the sort phase, and once per batch,
    ** when we ARE sorting, we will have freed up a bunch of space
    ** in the buffer before the start of the next record to be processed.
    ** We may be able to increase the space available
    ** for new data and pointers by sliding leftover data (if any)
    ** towards the start of the buffer, preserving alignment.
    ** In the case of memory mapped files, we move the window
    ** towards the leftover data instead of vice versa,
    ** but the logic is similar.
    */

#ifdef	MMAP
    if (bp->flag & BATCH_MAP) {
	if (((m = q - bp->bufp - k) >= bp->tunit) && !(bp->flag & BATCH_ALL)) {
	    /* We CAN move the map (at least one tunit of gap exists),
	    ** and we SHOULD move it (there is still something left to map).
	    */
	    q -= ROUNDDOWN(m, bp->tunit);
	    m = ROUNDUP(bp->total, bp->tunit);
	    o = ROUNDDOWN(bp->batcho, bp->tunit);
	    if (bp->bufp != MAP(bp->bufp, m,
			    PROT_READ, MAP_FIXED|MAP_PRIVATE,
			    bp->iofile.fd, o))
	    {
		setoffname(bp);
		sprintf(gp->emesg, "mmap failure at %s", gp->recon);
		fatal(gp);
	    }
	}
	bp->batch.ptr = q - k;
    } else {
#endif

	/* ``Ordinary'' (non-mapped) I/O */
	/* When there are no summary structures, whatever (if anything)
	** is left in the batch is unparsed data with no pointers into it.
	** We are free to move it around to create space.
	** If there are no leftover bytes, we are properly aligned,
	** and can reuse the entire buffer.  Otherwise,
	** we must preserve the alignment of the leftovers.
	*/
	if (k == 0) {		/* no leftovers, reuse entire buffer */
	    q = bp->batch.ptr = bp->bufp;
	} else {
	    p = q - k;		/* leftovers start here */
	    if ((n = p - bp->bufp) >= bp->aunit) {	/* space at start */
		n /= bp->aunit;	/* complete aunits in gap */
		bp->batch.ptr = q = p - n * bp->aunit;	/* start here instead */
		MEMCPY(q, p, k);	/* move batch into new place */
		q += k;			/* skip past leftovers to new end */
	    }
	}
   
#ifdef	MMAP
    }
#endif
    bp->batch.len = q - bp->batch.ptr;


    /* When we have just shifted the data or mmap windows to skip over
    ** records that were already parsed and read, the space made
    ** available for summary pointers may be all we need to make
    ** it possible to parse additional records.  In particular,
    ** when we have read all the data (BATCH_ALL), but were unable
    ** to parse it all because we didn't have enough summary pointers,
    ** it is essential that we parse WITHOUT going after more data,
    ** since the file descriptor may have been closed.
    **
    ** Calls via readmerge shouldn't get here with bp->couldbe on,
    ** since there's ALWAYS a free summary pointer in the merge phase,
    ** and records are parsed directly out of readmerge.  Still, the
    ** check and return are mostly harmless.
    */

    if (bp->couldbe) {
	setsumx(bp, bp->total - (q - bp->bufp) - (bp->sumc * gp->prohd));
	if ((PARSEB(bp) > 0) && (!gp->srtng)) return;
	if (bp->flag & BATCH_ALL) return;	/* no more forthcoming */
    }


    /* We have now parsed what we can of whatever was there.
    ** If we can carve out enough space to read some more,
    ** do so, and parse as much as possible of what we read.
    ** Stop when get at least one record in the merge phase,
    ** keep going until we exhaust the data or the space in
    ** the sort phase.
    */
    while (k = carve(bp, bp->total - (q - bp->bufp) - (bp->sumc * gp->prohd))) {
#ifdef	MMAP
	if (bp->flag & BATCH_MAP) {
	    if ((bp->iofile.posn += k) > bp->fsize) {
		k = bp->fsize - (bp->iofile.posn - k);
		bp->iofile.posn = bp->fsize;
		bp->flag |= BATCH_ALL;
	    }
	    q += k;
	} else {
#endif

	    for (; k > 0; k -= m) {
		m = read(bp->iofile.fd, q, k);
		if (m < 0) {
		    sprintf(gp->emesg, "Read error on %s fd %d wanting %d",
				bp->iofile.name, bp->iofile.fd, k);
		    fatal(gp);
		}
		if (m == 0) {
		    bp->flag |= BATCH_ALL;
		    break;
		}
		q += m;
		bp->iofile.posn += m;
#ifdef DIRECTIO
		if ((bp->flag & BATCH_DIO) && (bp->iofile.posn >= bp->fsize)) {
		    bp->flag |= BATCH_ALL;
		    break;
		}
#endif
	    }
#ifdef	MMAP
	}
#endif

	if ((bp->flag & (BATCH_ALL|BATCH_CLS|BATCH_MAP)) == BATCH_ALL) {
	    closeit(bp);
	}
	bp->batch.len = q - bp->batch.ptr;
	if ((PARSEB(bp) > 0) && (!gp->srtng)) break;
	if (bp->flag & BATCH_EOF) break;
    }
    return;
}


/* The job of the ``carve'' routine is to divide available
** bytes of space between records and summary structures.
** The number of bytes allocated to records is returned.
** The number of summary structures reserved is saved in bp->sumx.
** We will operate under the following assumptions.
**
** 1) Carve won't be called if there is useful work that
**	could be done without adding more record data.
**	A return of 0 should only happen when avail is too
**	small for even a single additional record transfer unit.
**	It can also happen when called with a completely empty file.
**
** 2) Subject to constraint 1, carve is free to allocate record space
**	and summary space to implement assorted strategies.
**	In particular, in the merge/check phase, it is possible
**	to make progress if even a single record is present.
**	Going after more record data will reduce system calls
**	and increase the opportunity to take advantage of orderliness
**	in the merge phase, but at the cost of possibly blocking
**	when progress would have been possible.  Eventually,
**	it would be best to have a separate thread doing read-ahead
**	for each input file, so we could take advantage of all
**	record data immediately available, without blocking.
**	For now, we can at least avoid the blocking by being
**	minimalist about allocation to record space when merging
**	or checking.  That strategy is not as good in the sort phase,
**	since there's only one source of input (hence blocking)
**	at a time.  When we get a partial batch, we almost always
**	turn around and go back for more.
*/


static long
carve(batch *bp, long avail)
{
    glob *gp = bp->com;
    long m, n, k;

    /* k will be our record allocation amount, n the number of records
    ** we expect to fit, m a scratch variable.
    **
    ** Under normal circumstances, reading another bp->tunit of record
    ** will give us a chance to complete (at least) one more record.
    ** Records larger than bp->tunit may need even more.
    ** It would be wasteful to go after less, since we'd just have to
    ** come back and ask for more, costing additional read() calls.
    */
    k = bp->tunit;
    if (gp->avgrz > k) k = ROUNDUP(gp->avgrz, k);
    if ((k + gp->prohd) > avail) {
	/* Not enough room for a full block and a record summary.
	** This is ok in the sort phase (stuff may fit after sorting
	** the batch), but not when merging or checking, where all
	** complete records should already have been parsed and processed,
	** and record data left-shifted over any empty blocks at the start.
	*/
	if (!gp->srtng) {
	    sprintf(bp->com->emesg, "Oversized record > %ld, max %ld",
					bp->left, gp->maxrz);
	    fatal(bp->com);
	}
	return (0);
    }

    /* We have enough space to paste another tunit onto the end of the batch,
    ** and point to at least one record at a time.  In the sort phase,
    ** we may want to be more aggressive about going after records,
    ** since we'll just have to come back for more if we didn't fill memory.
    ** See how the available space can be split between records and summaries.
    */
#ifdef	AGGRESSIVE
    if (1) {
#else
    if (gp->srtng) {
#endif	/* AGGRESSIVE */
	/* Estimate number of summaries we'll need.
	** Fixed format records are easy.  For non-fixed,
	** boost the expected number, because giving summaries back
	** is much less trouble than running out of them.
	*/
	n = avail / (gp->avgrz + gp->prohd);
	if (gp->fmtyp != FMTFIX) n += (n >> 3);
	m = ROUNDDOWN((avail - (n * gp->prohd)), bp->tunit);
	if (m > bp->maxio) m = bp->maxio;
	if (k < m) k = m;
    } else {	/* merging or checking */
	/* If we had an ``oracle'' (or thread) that knew how many blocks
	** were available without blocking, we'd do something like the
	** logic for sorting, throttled by the available blocks.
	** For now, just accept the minimal blocks.
	*/
    }
    setsumx(bp, avail - k);
    return (k);
}


/* All parse routines will be called with a batch of data, at least some
** part of which hasn't been parsed into records, and summary structure
** counts which allow at least one more record to be summarized (sumc < sumx).
** (If there's no unparsed data, the parse routines can't make any progress,
** and if there are no free summaries, they can't record any progress that
** they DO make).
**
** Subject to the above, parsing will stop when either unparsed data or
** available summaries are exhausted (or both).  If we exhaust summaries,
** then leftover record data (if any) will be essentially unprocessed,
** since we commence parsing only when a summary structure remains free
** (as it does at entry).  couldbe will set to 1 to indicate that there is
** some doubt about the existence of one or more complete unparsed
** records in the current batch.
**
** If we DON'T exhaust summaries, then we must have started a record we
** couldn't finish.  Presumably, the rest of the record will be added
** and parsed later.  couldbe is set zero to indicate that all
** complete records have been parsed.
**
** We must be vigilent about truncated or superhuge inputs,
** which aren't SUPPOSED to happen, but are beyond our ability to control.
** These would be heralded by the inability to add even a single record
** (thanks to the entry assertions).  If we are in a context where
** more record data can be tacked onto what is already there,
** this isn't fatal.  We leave to the caller the possibility of
** adding more data, perhaps after arranging a more capacious environment
** for the leftover data.  But if BATCH_ALL is set, we know no more
** data is forthcoming.  If a partial record occurs at the end of
** such a batch, we have a truncated input.
**
** We turn BATCH_EOF on when BATCH_ALL is set (meaning no more data
** will be forthcoming), and all leftover data have been parsed and
** summarized.
*/


/* Parse the batch of data into fixed-length records */

static long
fixparse(batch *bp)
{
    glob *gp = bp->com;
    long n, nr;

    bp->basep = (gptr)(bp->batch.ptr);
    n = bp->batch.len / gp->grecz;		/* total (whole) records */
    bp->couldbe = 0;				/* assume all will fit */
    if (n > bp->sumx) {				/* check that assumption */
	n = bp->sumx;
	bp->couldbe = 1;
    }
    nr = (n - bp->sumc);			/* new records */
    bp->sumc = n;
    bp->parsed = n *= gp->grecz;
    bp->left = bp->batch.len - n;
    if (bp->flag & BATCH_ALL) {			/* That's all, folks */
	if ((n = (bp->left % gp->grecz))) {
	    bp->thiso = bp->batcho + bp->batch.len - n;
	    setoffname(bp);
	    errno = EIO;
	    sprintf(gp->emesg, "Short record (%ld < %ld) at %s",
				n, gp->grecz, bp->com->recon);
	    fatal(bp->com);
	}
	if (bp->left == 0) bp->flag |= BATCH_EOF;
    }
    if (bp->iofile.indx < 0) {
	gp->nrecs += nr;
	gp->brecs += nr;
	gp->nbyte += nr * gp->grecz;
    }
    return (nr);
}


/* Parse the batch of data into newline-delimited records
**
** bp->batch.ptr is the base of an area of size bp->batch.len.
** If there are completely parsed records in the area,
** then bp->sumc is their number, and they occupy the first
** bp->parsed bytes of the area.  Any complete records may be
** followed by bp->left bytes of data that couldn't be parsed,
** either because they represent a single partial record whose
** terminating newline hasn't appeared yet (couldbe == 0),
** or because we ran out of summary structures (couldbe != 0),
** and haven't looked at the leftover data.  The rest of the area
** (if any) is new data, added since the previous parse.
*/

static long
newparse(batch *bp)
{
    glob *gp = bp->com;
    uch *p, *last, *q;
    long i, n, maxlen, skip;
    ptrlen *ptrp;


    /* Scan backwards for a newline to use as sentinal,
    ** so we don't have to keep a count on the forward scan
    ** that follows.  We used to store a newline at the last byte
    ** as sentinal, but this has undesirable implications for mapped data.
    ** If we parsed all complete records last time (couldbe == 0), we have
    ** already scanned the bp->left bytes for a newline, and found none.
    ** We don't have to scan them again.
    */
    bp->basep = (gptr)(((uch *)(bp->plast)) + gp->gstep);
    p = bp->batch.ptr;		/* 1st byte of 1st (possibly parsed) record */
    q = p + bp->batch.len;	/* 1st byte following all data */
    n = bp->parsed;		/* bytes of complete records from last parse */
    last = p += n;		/* 1st byte of 1st incompletely parsed record */
    skip = (bp->couldbe) ? 0 : bp->left;	/* bytes known to be \n-free */
    last += skip;		/* already scanned for \n */
    bp->couldbe = 0;		/* guess that we'll parse everything */
    while ((--q >= last) && (*q != NEWLINE));


    if (q < last) i = 0;	/* no \n => no records, parsed (n) unchanged */
    else {			/* guaranteed to find at least 1 rec */
	last = q;		/* last newline in batch */
	maxlen = gp->maxrz;
	if ((i = bp->sumc) == 0) bp->curp = (gptr)(bp->plast);
	ptrp = (ptrlen *) (bp->curp);
	/* p is either on the first byte of a file, or just past the
	** newline that terminated the previously parsed record.
	** We don't advance p unless there's room for another summary
	** AND a newline is known to be forthcoming in the current batch.
	** Therefore, we exit with p at the start of a record, too.
	*/
	for (; (i < bp->sumx) && (p <= last); i++) {
	    --ptrp;
	    ptrp->ptr = q = p;
	    p += skip; skip = 0;	/* may have scanned part of 1st */
	    while (*p != NEWLINE) p++;	/* must stop at or before last */
	    ptrp->len = p++ - q;	/* don't include newline in len */
	    if (maxlen <= ptrp->len) maxlen = ptrp->len + 1;
	    if (gp->ptohd > sizeof(ptrlen)) {
		n = ptrp->len - gp->gkoff;
		if (n <= 0) {
		    n = 0;
		} else {
		    q += gp->gkoff;
		    if ((gp->gklen > 0) && (n > gp->gklen)) n = gp->gklen;
		}
		--ptrp;
		ptrp->ptr = q;
		ptrp->len = n;
	    }
	}
	if (p <= last) bp->couldbe = 1;	/* left at least one unparsed */
	bp->curp = (gptr) ptrp;
	n = p - bp->batch.ptr;	/* How much has now been parsed */
	i -= bp->sumc;		/* How many records were just added (>0) */
	if (bp->iofile.indx < 0) {
	    gp->avgrz =		/* new average record size is ... */
		(gp->nbyte += n - bp->parsed)	/* ... new total bytes ... */
		/ (gp->nrecs += i);		/* ... over new total recs */
	    gp->brecs += i;	/* additional recs in just this batch */
	    if (gp->maxrz < maxlen) gp->maxrz = maxlen;
	}
    }


    bp->sumc += i;
    bp->left = bp->batch.len - (bp->parsed = n);


    /* If the file has been entirely read into memory, BATCH_ALL will be on.
    ** If we parsed everything, then we can turn on BATCH_EOF.
    ** If stuff remains unparsed, that's ok if we ran out of summaries.
    ** Otherwise, we have an unterminated record at the end of the file.
    */
    if (bp->flag & BATCH_ALL) {
	if (bp->left == 0) bp->flag |= BATCH_EOF;
	else if (bp->couldbe == 0) {
	    bp->thiso = bp->batcho + bp->parsed;
	    setoffname(bp);
	    errno = EIO;
	    sprintf(gp->emesg,
		"File has no terminating newline, last record at %s",
		bp->com->recon);
	    fatal(bp->com);
	}
    }
    return (i);
}


/* Parse the batch of data into header-delimited records
**
** Each record is attacked in two phases.
** During phase 1, we haven't read in the entire header yet.
** During that phase, bp->grecz will be -1, a value that cannot
** be associated with a legitimate record.
** Once we have read in the entire header, we can determine
** the actual record length.  This will be stored in gp->grecz,
** thereby identifying phase 2.  When the entire record is also
** available, we can set the key and record ptrlens, and go after
** the next record.
**
** bp->scanned will always contain the number of bytes belonging
** to the header, complete or otherwise.  bp->left is the number
** of bytes from the start of the last incomplete record to the
** last byte available when the attempt to parse took place.
*/

static long
hdrparse(batch *bp)
{
    glob *gp = bp->com;
    uch *p, *last, *q;
    long i, n, maxlen;
    ptrlen *ptrp;

    bp->basep = (gptr)(((uch *)(bp->plast)) + gp->gstep);
    if (bp->batch.len == 0) {
	if ((bp->left == 0) && (bp->flag & BATCH_ALL)) bp->flag |= BATCH_EOF;
	bp->sumc = 0;
	return (0);
    }
    maxlen = gp->maxrz;
    p = bp->batch.ptr;
    last = p + bp->batch.len;
    q = p + bp->parsed;			/* next header starts here */
    p = q + bp->scanned;		/* header (complete or not) end */
    if ((i = bp->sumc) == 0) bp->curp = (gptr)(bp->plast);
    ptrp = (ptrlen *) (bp->curp);


    /* When entering and leaving the following loop, the following hold:
    ** q is the 1st byte of a (possibly incomplete) header.
    ** p is the last byte processed in (or immediately following) that header.
    ** i is what will become sumc, the (total) number of completely
    ** parsed records in the current batch.
    */
    bp->couldbe = 1;			/* assume we WON'T get them all */
    for (; ((i < bp->sumx) && (p < last)); i++, q = p) {
	if (bp->grecz < 0) {		/* header not complete yet */
	    while ((p < last) && (*p != TERM)) p++;	/* scan for TERM */
	    if (p >= last) {			/* didn't find one */
		if (bp->flag & BATCH_ALL) {	/* no term is forthcoming */
		    bp->sumi = i;		/* XXX ??? XXX */
		    bp->curp = (gptr) ptrp;
		    parserror(bp, q);
		}
		bp->couldbe = 0;		/* got all complete ones */
		break;
	    }
	    bp->scanned = ++p - q;
	    parseheader(bp, q);
	}
	if ((last - p) < bp->grecz) {		/* whole record not present */
	    bp->couldbe = 0;			/* got all complete ones */
	    break;
	}
	--ptrp;
	ptrp->ptr = q;
	ptrp->len = bp->scanned + bp->grecz;
	if (maxlen < ptrp->len) maxlen = ptrp->len;
	--ptrp;
	ptrp->ptr = p + bp->gkoff;
	ptrp->len = bp->gklen;
	p += bp->grecz;
	bp->grecz = -1;
	bp->curp = (gptr) ptrp;
    }


    n = q - bp->batch.ptr;		/* total bytes now parsed */
    bp->scanned = p - q;		/* bytes of header (so far) */
    i -= bp->sumc;			/* new, completely parsed records */
    /* What follows parallels the activity in newparse */
    if ((i > 0) && (bp->iofile.indx < 0)) {
	gp->avgrz =			/* new average record size is ... */
	    (gp->nbyte += n - bp->parsed)	/* ... new total bytes ... */
	    / (gp->nrecs += i);		/* ... over new total recs */
	gp->brecs += i;	/* additional recs in just this batch */
	if (gp->maxrz < maxlen) gp->maxrz = maxlen;
    }


    bp->sumc += i;
    bp->left = bp->batch.len - (bp->parsed = n);


    if (bp->flag & BATCH_ALL) {
	if (bp->left == 0) bp->flag |= BATCH_EOF;
	else if (bp->couldbe == 0) {
	    if (bp->grecz < 0) {	/* header itself was lopped off */
		bp->curp = (gptr) ptrp;
		parserror(bp, q);
	    } else {			/* header ok, record truncated */
		bp->thiso = bp->batcho + bp->parsed;
		setoffname(bp);
		errno = EIO;
		n = bp->scanned;
		if (n > 50) n = 50;
		sprintf(gp->emesg, "Truncated record with header `%.*s' at %s ",
				    (int) n, q, bp->com->recon);
		fatal(bp->com);
	    }
	}
    }
    return (i);
}
