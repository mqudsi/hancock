#define DIRECTORY 0
#define MAP 1
#define PICKLE 2

typedef struct HRSpt_s {
  char type;       /* DIRECTORY, MAP, PICKLE */
  void *handle;
  struct HRSpt_s *next;
} *HRSpt_t;

void HRSaddPDS(char, void *);

typedef struct {
  Sfio_t *file;
  char *name;
  char buffered;  /* has the file been set to use buffered I/O */
  int32 mode;    /* READONLY or READWRITE */


} fileInfo_s;

void HRSwriteBytes(fileInfo_s fi, uint8 *data, Sfoff_t start, Sfoff_t numBytes, char syncRequired);
void HRSreadBytes(fileInfo_s fi, uint8 *data, Sfoff_t start, Sfoff_t numBytes);
