
directory dir_t {
  int   myDate;
  float myFloat;
};


int sig_main(new dir_t myDir <d:>) {
 myDir->myDate = 2000;
 myDir->myFloat = 3.14159;
 return 0;
}



