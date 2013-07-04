
struct foo_t{
  int x;
  float y;
};

directory dir_t {
  int   myDate;
  float myFloat;
  struct foo_t myStruct;
  int   myArray[2];
};


int sig_main(new dir_t myDir <D:>) {
 myDir->myDate = 2000;
 myDir->myFloat = 3.14159;
 myDir->myStruct.x = 11;
 myDir->myStruct.y = 12.08;
 myDir->myArray[0] = 3;
 myDir->myArray[1] = 4;
 return 1;
}



