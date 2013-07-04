map map_m(long long min, long long max, int def){
  key (min .. max);
  split (1000,10);
  value int;
  default def;
}

void sig_main(char flag <c, "flag">,
	      long long min default 0 <n:, "minimum value.">,
	      long long max default 9999999999LL <x:, "maximum value.">,
              int def default 12 <d:, "default value.">,
              map_m m(:min,max,def:) default "testMap" <m:, "map value.">){

  if (flag)
    sfprintf(sfstdout, "Flag c was present on the command line.\n");
  else
    sfprintf(sfstdout, "Flag c was not present on the command line.\n");
  sfprintf(sfstdout, "The value of min is: %lld.\n", min);
  sfprintf(sfstdout, "The value of max is: %lld.\n", max);
  sfprintf(sfstdout, "The value of def is: %d.\n", def);
  sfprintf(sfstdout, "The value of m<:%d:> is: %d.\n", 10, m<:10:>);

}
