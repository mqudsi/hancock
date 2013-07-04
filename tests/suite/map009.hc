#define MAXKEY 99999999
map test_m{
  key (0 .. MAXKEY);
  split(1000, 100);
  value int;
  default 0;
}

int main(){
  new test_m m = "../genData/data/testMap";
    m<:10:> = 100;
    
  if (m?<:MAXKEY + 1 :>) 
    sfprintf(sfstdout, "%d is not out of range of map.\n", MAXKEY + 1 );
  else
    sfprintf(sfstdout, "%d is out of range of map.\n", MAXKEY+1);


  if (m@<:10:>) 
    sfprintf(sfstdout, "%d is in the map.\n", 10);
  else
    sfprintf(sfstdout, "%d is not in the map.\n", 10);


  if (m@<:100:>) 
    sfprintf(sfstdout, "%d is in the map.\n", 100);
  else
    sfprintf(sfstdout, "%d is not in the map.\n", 100);
  return 0;
}
