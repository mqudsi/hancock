typedef struct{
  int i;
  int j;
} intPair;

int getValidIntPair(intPair *p, intPair* l){
    l->i = p->i;
    l->j = p->j;
    return 1;
}

stream intPair_s {
  getValidIntPair: intPair => intPair;
}

munion intPair_e {: intPair theInts :};

intPair_e getIntPair(intPair *w[1:0]){
  return {: theInts = *w[0] :};
}

void sig_main(intPair_s data <c:, "Binary integer pair stream">){
  iterate 
    (over data
     sortedby i
     withevents getIntPair){
   
    event theInts(intPair p){
      sfprintf(sfstdout, "i=%d, j=%d\n", p.i, p.j);
    }
  }

}
