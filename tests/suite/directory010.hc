#include <HRS.h>

struct foo_t{
  int x;
  float y;
  int myArray[2];
  int mySecondArray[3];
};

typedef struct{
  int z;
  char * myString;
} foo2_t;

directory dir_t {
  int   myDate;
  float myFloat;
  char  myChar;
  struct foo_t myStruct;
  foo2_t mySArray[2];
};


int sig_main(exists dir_t myDir <d:>, new dir_t myNewDir <D:>) {
 myNewDir :=: myDir;
 myNewDir->myDate += 1;
 return 1;
}



