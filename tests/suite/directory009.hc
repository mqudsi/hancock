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


int sig_main(exists dir_t myDir <d:>, new dir_t myNewDir <D:>) {
 myNewDir->myDate = myDir->myDate + 1;
 myNewDir->myFloat = myDir->myFloat + 1.0;
 myNewDir->myChar = myDir->myChar + 1;
 myNewDir->myStruct.x = myDir->myStruct.x + 1;
 myNewDir->myStruct.y = myDir->myStruct.y + 1.0;
 myNewDir->myStruct.myArray[0] = myDir->myStruct.myArray[0] +1;
 myNewDir->myStruct.myArray[1] = myDir->myStruct.myArray[1] +1;
 myNewDir->myStruct.mySecondArray[0] = myDir->myStruct.mySecondArray[0] + 1;
 myNewDir->myStruct.mySecondArray[1] = myDir->myStruct.mySecondArray[1] + 1;
 myNewDir->myStruct.mySecondArray[2] = myDir->myStruct.mySecondArray[2] + 1;
 myNewDir->mySArray[0].z = myDir->mySArray[0].z + 1;
 myNewDir->mySArray[0].myString = "new first string.";
 myNewDir->mySArray[1].z = myDir->mySArray[1].z + 1;
 myNewDir->mySArray[1].myString = "new second string.";
 return 1;
}



