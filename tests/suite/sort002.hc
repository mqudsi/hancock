int getValidInt(Sfio_t *f, int* i){
  int found;
  found = sfscanf(f, "%d", i);
  if (found == EOF | found != 1) 
    return 0;
  else 
    return 1;
}

stream int_s {
  getValidInt: Sfio_t => int;
}

munion int_e {: int theInt :};

int_e getInt(int *w[1:0]){
  return {: theInt = *w[0] :};
}

void sig_main(int_s data <c:, "Ascii integer stream">){
  iterate 
    (over data
     sorted
     withevents getInt){
   
    event theInt(int i){
      sfprintf(sfstdout, "%d\n", i);
    }
  }

}
