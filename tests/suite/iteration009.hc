#define MAXKEY 99999999
map test_m{
  key (0 .. MAXKEY);
  split(1000, 100);
  value int;
  default 0;
}

munion m_e{: long long re  :};

int main(){
  test_m m = "../genData/data/testMap";
  m<:10:> = 100;
  iterate(over m
	  withevents (w[1:0]){: re =  *w[0] :})
  {
    event re(long long l) {
      sfprintf(sfstdout, "key value: %lld.\n", l);
    }  
  }
  return 0;
}
