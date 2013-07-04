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

int sig_main(new dir_t myNewDir <D:>) {
  lpn_t pn = {973,360,8675};
  struct data_t myData;
  myNewDir->myDIData = 1;
  myNewDir->myDFData = 1.1;
  myData.myInt = myNewDir->myDIData + 2;
  myData.myFloat = myNewDir->myDFData + 2.2;
  myNewDir->myMap<:pn:> = myData;
  return 1;
}


