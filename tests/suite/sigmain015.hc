
sig_main(unsigned char c <c:>,
           signed char s <s:>){
  sfprintf(sfstdout, "Unsigned char c is: \"%hu\"\n", c);
  sfprintf(sfstdout, "Unsigned char c is: \"%c\"\n", c);
  sfprintf(sfstdout, "Signed char s is: \"%d\"\n", s);
  sfprintf(sfstdout, "Signed char s is: \"%c\"\n", s);
}
