#include <stdio.h>
#include "autoconfig.h"
extern void exit(int i);

int main ()
{
  int8 space[10];
  int8 *buffer=space;
  int32 data=10;
  int32 result;
  store_int32 (&buffer,data);
  buffer = space;
  read_int32 (&buffer,&result);
  fprintf (stdout,"data: %d, result: %d.\n",data,result);

  if (data != result) {
        printf("Output is wrong!\n");
        exit(-1);
  }
  return 0;
}


