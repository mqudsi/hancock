int getValidChar(Sfio_t *f, char* c){
  int found;
  found = sfscanf(f, "%c", c);
  if (found == EOF | found != 1) 
    return 0;
  else 
    return 1;
}

stream char_s {
  getValidChar: Sfio_t => char;
}

munion char_e {: char theChar :};

char_e getChar(char *w[1:0]){
  return {: theChar = *w[0] :};
}

int main(){
  char_s data = "../genData/data/chars.ascii";
  iterate 
    (over data
     filteredby (cp) *cp !='e'
     sorted
     withevents getChar){
   
    event theChar(char c){
      sfprintf(sfstdout, "%c", c);
    }
  }
  sfprintf(sfstdout, "\n");
  return 0;
}
