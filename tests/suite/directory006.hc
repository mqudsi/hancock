
struct foo_t{
  int x;
  float y;
};

directory dir_t {
  int   myDate;
  float myFloat;
  struct foo_t myFoo;
};


int sig_main(exists dir_t myDir <d:>,
              new dir_t newDir <D:>) {
 newDir :=: myDir; 
 return 1;
}



