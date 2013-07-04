int nextElem(char param, int *i, int *next){
  static int j = 0;
  if (param) {
    *next = *i;
    *i += 1;
  } else { *next = j;    j++;   }
  if (10 == *next)
    return HRS_STREAM_STOP;
  else
    return HRS_STREAM_KEEP_REC;
}

stream gs_s(int *i) { nextElem(:i:) : void => int; };

int main(){
  int count = 0;
  gs_s s(:&count:) = NULL;
  iterate (over s
	   filteredby (i) (0 == *i % 2)) {
    event(int *i){
      sfprintf(sfstdout, "The next number was:%d.\n", *i);
    }
  }
  sfprintf(sfstdout, "Done.\n");
  return 0;
}
