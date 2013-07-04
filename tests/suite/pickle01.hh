#include <unistd.h>

typedef struct{
  Sfio_t *fp;          /* cache file pointer. */
  int val;             /* persistent integer. */
} rep_t;

int init_foo(char paramsSupplied, int init, Sfio_t *fp, rep_t *data, char readonly){
  int initVal = (paramsSupplied) ? init : 100;
  char *s1 = "Init_foo: Initialized from parameter with value %d\n";
  char *s2 = "Init_foo: No parameter supplied; initalized with %d\n";
  if (sfsize(fp) == 0) {
    sfprintf(sfstdout, "Empty file\n");
    if (paramsSupplied) sfprintf(sfstdout, s1, initVal);
    else sfprintf(sfstdout, s2, initVal);
    data->val = initVal;
  }
  else {
    sfscanf(fp, "%d\n", &data->val);
    sfprintf(sfstdout, "Init_foo: Initialized from file with value %d\n", data->val);
  }
  data->fp = fp;  /* cache file pointer. */
  return 0; /* Report no error. Non-zero values denote errors. */
}

int write_foo(Sfio_t *fp, rep_t *data, char close){
  Sfoff_t currLoc;
  if (fp != data->fp) {
    sfprintf(sfstdout, "Cached file pointer doesn't match.\n");
    return 1; /* Report error. */
  } else {
    sfseek(fp, (Sfoff_t) 0, SEEK_SET); /* Seek to beginning of the file. */
    sfprintf(fp, "%d\n", data->val);   /* Write current value to file. */
    currLoc = sfseek(fp, (Sfoff_t) 0, SEEK_CUR);  /* Find current seek address. */
    ftruncate(sffileno(fp), currLoc);   /* Truncate anything after the most recent write. */
    return 0; /* Report no error. */
  }
}

pickle foo_p (int init) {init_foo(:init+3:) => rep_t => write_foo};

