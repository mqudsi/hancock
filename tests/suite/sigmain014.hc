
void sig_main(unsigned short u <u:>,
              signed short s <s:>,
	      unsigned int j <j:>,
	      signed int i <i:>){

  sfprintf(sfstdout, "The value of u is: %hu.\n", u);
  sfprintf(sfstdout, "The value of s is: %hd.\n", s);
  sfprintf(sfstdout, "The value of j is: %u.\n", j);
  sfprintf(sfstdout, "The value of i is: %d.\n", i);
}
