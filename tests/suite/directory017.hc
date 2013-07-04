#include <unistd.h>

int foo_read(Sfio_t *f, int *x, char readonly) {
  if (sfsize(f)== 0) {
    sfprintf(sfstdout, "Empty file.\n");
    *x = 25;
    return 0;
  } else {
    int numRead = sfscanf(f , "%d", x);
    return (numRead != 1);
  }
}


int foo_write(Sfio_t *f, int *x, char write) {
  Sfoff_t currLoc;

  sfseek(f, (Sfoff_t) 0, SEEK_SET); /* seek to beginning of the file */
  sfprintf(f,"%d\n",*x);
  currLoc = sfseek(f, (Sfoff_t) 0, SEEK_CUR);  /* find current seek address */
  ftruncate(sffileno(f), currLoc);   /* truncate anything after the most recent write */
  return 0;
}

pickle foo_p { foo_read => int => foo_write }

directory dir_d {
  foo_p f;
}

int sig_main(dir_d myDir <d:, "existing directory">, 
             dir_d myNewDir <D:, "new directory">){
  
  *(myDir->f) -= 10;

  myNewDir :=: myDir;

  return 0;
}
