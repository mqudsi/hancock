
struct foo_t{
  int x;
  float y;
};

directory dir_t {
  int   myDate;
  float myFloat;
  struct foo_t myFoo;
};


int sig_main(new dir_t myDir <D:>) {
 myDir->myDate = 2000;
 myDir->myFloat = 3.14159;
 myDir->myFoo.x = 3;
 myDir->myFoo.y = 10.09;
 return 1;
}



