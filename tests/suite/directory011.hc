
directory dir_t {
  char * myString;
};


int sig_main( new dir_t myDir <D:>,
               char * cString <s:>) {

 myDir->myString = cString;
 return 1;
}



