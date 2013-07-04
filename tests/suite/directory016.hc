
int foo_read(Sfio_t *f, int *x, char readonly) {
  if(f==NULL) {
    sfprintf(sfstdout, "Initialized it.!\n");
    *x = 2;
    return 0;
  } else return (sfscanf(f,"%d",x) == 1);
}

int foo_write(Sfio_t *f, int *x, char write) {
  sfprintf(f,"%d\n",*x);
  return 0;
}

pickle foo_p { foo_read => int => foo_write }

directory dir_d {
  foo_p f;
}

int sig_main(new dir_d myNewDir <D:, "new directory">){
  
  *(myNewDir->f) = 10;

  return 0;
}
