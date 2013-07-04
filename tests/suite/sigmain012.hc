void sig_main(long l <l:>,
              long long k <k:>,
              unsigned long j <j:>,
	      unsigned long long i <i:>){

  sfprintf(sfstdout, "The value of l is: %ld.\n", l);
  sfprintf(sfstdout, "The value of k is: %lld.\n", k);
  sfprintf(sfstdout, "The value of j is: %lu.\n", j);
  sfprintf(sfstdout, "The value of i is: %llu.\n", i);
}
