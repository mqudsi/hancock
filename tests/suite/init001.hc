#include "../scampRec.hh"
map map_m {
  key ppn_t;
  value int;
  default 0;
}

directory dir_d{
  int x;
}

void insertTimes(cdStream calls, map_m myMap){

  iterate (
    over calls
    sortedby connecttime
    withevents originDetect){
 
    event call(HscampRec r){
        myMap<:r.origin:> = r.connecttime;
    }   
  }
}

void printTimes(map_m myMap){
  lpn_t start = {000,000,0000};
  lpn_t stop  = {999,999,9999};
  iterate(
    over myMap[start .. stop]
    withevents mapDetect){

    event line_begin(lpn_t pn){
      sfprintf(sfstdout,"(%03d)%03d-%04d: ",pn.npa,pn.nxx,pn.line);
      sfprintf(sfstdout, "%d.\n", myMap<:pn:>);
    }
  }
}

int sig_main(char * myMapName <m:>, 
             char * myDirName <d:>,
	     char * myStreamName <s:>){
  static int x = 0;
  const int y = 9, w = 20;
  lpn_t pn = {222, 233, 2099};
  new map_m myMap = myMapName;
  dir_d myDir = myDirName;
  cdStream myStream = myStreamName;
  insertTimes(myStream, myMap);
  printTimes(myMap);
  myDir->x = myMap<:pn:> + x + y + w;
}

