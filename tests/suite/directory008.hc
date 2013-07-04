struct foo_t{
  int x;
  float y;
  int myArray[2];
  int mySecondArray[3];
};

typedef struct{
  int z;
  char *myString;
} foo2_t;

directory dir_t {
  int   myDate;
  float myFloat;
  char  myChar;
  struct foo_t myStruct;
  foo2_t mySArray[2];
};


int sig_main(new dir_t myDir <D:>) {
 myDir->myDate = 2000;
 myDir->myFloat = 3.14159;
 myDir->myChar = 'a';
 myDir->myStruct.x = 11;
 myDir->myStruct.y = 12.08;
 myDir->myStruct.myArray[0] = 3;
 myDir->myStruct.myArray[1] = 4;
 myDir->myStruct.mySecondArray[0] = 5;
 myDir->myStruct.mySecondArray[1] = 6;
 myDir->myStruct.mySecondArray[2] = 7;
 myDir->mySArray[0].z = 8;
 myDir->mySArray[0].myString = "first string.";
 myDir->mySArray[1].z = 9;
 myDir->mySArray[1].myString = "second string.";
 return 1;
}



