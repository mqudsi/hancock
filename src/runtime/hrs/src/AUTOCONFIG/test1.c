#include <stdio.h>
#include "autoconfig.h"
extern void exit(int i);

int test_storeu8(){
  int8 space[10];
  int8 *buffer = space;
  uint8 data = 10;
  uint8 result;

  store_uint8(&buffer, data);

  buffer = space;
  read_uint8(&buffer, &result);

  printf("data: %d, result: %d.\n", data, result);
  if (data != result) {
	printf("Output is wrong!\n");
  	exit(-1);
  }
  return 0;
}

int test_stores8(){
  int8 space[10];
  int8 *buffer = space;
  int8 data = -120;
  int8 result;

  store_int8(&buffer, data);

  buffer = space;
  read_int8(&buffer, &result);

  printf("data: %d, result: %d.\n", data, result);
  if (data != result) {
	printf("Output is wrong!\n");
  	exit(-1);
  }
  return 0;
}

int test_stores16(){
  int8 space[10];
  int8 *buffer = space;
  int16 data = I16_C(0x8f39);
  int16 result;

  store_int16(&buffer, data);

  buffer = space;
  read_int16(&buffer, &result);

  printf("data: %d, result: %d.\n", data, result);
  if (data != result) {
	printf("Output is wrong!\n");
  	exit(-1);
  }
  if (space[0] != I8_C(0x8f) || space[1] != I8_C(0x39)) {
    fprintf(stderr, "Byte order is not right!\n Try a make clean!\n");
    exit(-2);
  }
  return 0;
}


int test_storeu16(){
  int8 space[10];
  int8 *buffer = space;
  uint16 data = U16_C(0x8f39);
  uint16 result;

  store_uint16(&buffer, data);

  buffer = space;
  read_uint16(&buffer, &result);

  printf("data: %d, result: %d.\n", data, result);
  if (data != result) {
	printf("Output is wrong!\n");
  	exit(-1);
  }
  return 0;
}

int main() {
	test_stores8();
	test_storeu8();
	test_stores16();
	test_storeu16();
	return 0;
}
