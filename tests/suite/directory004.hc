#include "../scampRec.hh"

struct data_t {
  int myInt;
  float myFloat;
};

map map_t {
  key ppn_t;
  value struct data_t;
};


directory dir_t {
  map_t myMap;
  int myDIData;
  float myDFData;
};


int sig_main(exists dir_t myOldDir <d:>,
              new dir_t myNewDir <D:>) {
  lpn_t pn = {973,360,8675};
  struct data_t myNewData;
  struct data_t myOldData;

  myOldData = myOldDir->myMap<:pn:>;

  myNewDir :=: myOldDir;
  myNewData = myNewDir->myMap<:pn:>;
  myNewData.myInt += 3;
  myNewData.myFloat += 3.0;

  myNewDir->myDIData = myOldData.myInt + myNewData.myInt + 4;
  myNewDir->myDFData = myOldData.myFloat + myNewData.myFloat + 4.4;
  myNewDir->myMap<:pn:> = myNewData;
  return 1;
}


