#define TRUE	1
#define FALSE   0

int sig_main(char first <t:> default 't',
             char second <u>,
             char third <v> default TRUE,
             char fourth <w> default FALSE,
             char fifth <x:>) {

  sfprintf(sfstdout, "Parameters: ");
  sfprintf(sfstdout, "first = %c, ", first);
  sfprintf(sfstdout, "second = %x, ", second);
  sfprintf(sfstdout, "third = %x, ", third);
  sfprintf(sfstdout, "fourth = %x, ", fourth);
  sfprintf(sfstdout, "fifth = %c. ", fifth);
  sfprintf(sfstdout, "\n", first);
  if (second) 
     sfprintf(sfstdout, "Second parameter was true.\n");	
  else
     sfprintf(sfstdout, "Second parameter was false.\n");	
  if (third) 
     sfprintf(sfstdout, "Third parameter was true.\n");	
  else
     sfprintf(sfstdout, "Third parameter was false.\n");	
  if (fourth)
     sfprintf(sfstdout, "Fourth parameter was true.\n");	
  else
     sfprintf(sfstdout, "Fourth parameter was false.\n");	
  return 0;
}











