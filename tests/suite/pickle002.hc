#include <unistd.h>
#include "../scampRec.hh"

map bar_m {
  key ppn_t;
  value int;
  default 0;
};

typedef struct{
  Sfio_t *fp;
  char val;
} rep_t;

int init_foo(Sfio_t *fp, rep_t *data, char readonly){
  int returnCode;
  data->fp = fp;
  if (sfscanf(fp, "%c\n", &data->val) == EOF){
    data->val = 'a';  /* initial value for empty pickle. */
  }
  return 0; /* denotes no error. Non-zero values denote errors. */
}

int write_foo(Sfio_t *fp, rep_t *data, char close){
  Sfoff_t currLoc;
  if (fp != data->fp) {
    sfprintf(sfstdout, "Cached file pointer doesn't match.\n");
  } else {
    sfseek(fp, (Sfoff_t) 0, 0);
    sfprintf(fp, "%c\n", data->val);
    currLoc = sfseek(fp, (Sfoff_t) 0, SEEK_CUR);  /* find current seek address */
    ftruncate(sffileno(fp), currLoc);   /* truncate anything after the most recent write */
  }
  return 0; /* denotes no error. */
}

pickle foo_p {init_foo => rep_t => write_foo};

directory dir_d {
 foo_p f;
 bar_m m;
}

void chkPickle(dir_d myDir){
  lpn_t pn = {973, 984, 3167};
  sfprintf(sfstdout, "Initial map value: %d\n", myDir->m<:pn:>);
  myDir->m<:pn:> = myDir->m<:pn:> + 2; 
  sfprintf(sfstdout, "New map value: %d\n", myDir->m<:pn:>);
  sfprintf(sfstdout, "Initial pickle value: %c\n", myDir->f->val);
  myDir->f->val = myDir->f->val + 2;
  sfprintf(sfstdout, "New pickle value: %c\n", myDir->f->val);
}

int sig_main (dir_d myDir1 <d:>, dir_d myDir2 <D:>){
  chkPickle(myDir1);
  myDir2 :=: myDir1;
  chkPickle(myDir2);
  return 0;
}

