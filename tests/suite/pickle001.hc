#include <unistd.h>

typedef struct{
  Sfio_t *fp;
  char val;
} rep_t;


int init_foo(Sfio_t *fp, rep_t *data, char readonly){
  if (sfsize(fp) == 0) {
    sfprintf(sfstdout, "Empty file\n");
    data->val = 'a';
  }
  else {
    sfscanf(fp, "%c\n", &data->val);
    sfprintf(sfstdout, "In init_foo: %c\n", data->val);
  }
  data->fp = fp;
  return 0; /* denotes no error. Non-zero values denote errors. */
}

int write_foo(Sfio_t *fp, rep_t *data, char close){

  Sfoff_t currLoc;

  if (fp != data->fp) {
    sfprintf(sfstdout, "Cached file pointer doesn't match.\n");
    return 1; /* some kind of error */
  } else {
    sfseek(fp, (Sfoff_t) 0, SEEK_SET); /* seek to beginning of the file */
    sfprintf(fp, "%c\n", data->val);
    currLoc = sfseek(fp, (Sfoff_t) 0, SEEK_CUR);  /* find current seek address */
    ftruncate(sffileno(fp), currLoc);   /* truncate anything after the most recent write */
    return 0; /* denotes no error. */
  }
  return 0;
}

pickle foo_p {init_foo => rep_t => write_foo};

void chkPickle(foo_p myPickle){
  sfprintf(sfstdout, "Initial pickle value: %c\n", myPickle->val);
  myPickle->val = myPickle->val + 2;
  sfprintf(sfstdout, "New pickle value: %c\n", myPickle->val);
}

int sig_main (foo_p myPickle1 <p:>, foo_p myPickle2 <q:>){
  chkPickle(myPickle1);
  myPickle2 :=: myPickle1;
  chkPickle(myPickle2);
  return 0;
}

// Other things to test:
// pickles in directories
// pickle copy in directories
