#include <stdio.h>
#include <stdlib.h>

#include "autoconfig.h"

/* ---- example of MIN_ARRAY_LENGTH --- */

/* Allocate a variable sized struct */

struct msg {
    int32   mtype;
    int32   length;
    int8    body[MIN_ARRAY_LENGTH];
};

typedef struct msg msg_t;

msg_t* alloc_msg(int32 payloadsize) {
    msg_t* p = (msg_t*) malloc (sizeof(msg_t)+payloadsize-MIN_ARRAY_LENGTH);

    p->length = payloadsize;
    return p;
}
    
    	

void dump(unsigned char* v, int size) {
    int i = 0;

    printf("size = %d: ", size);
    for (i = 0 ; i < size ; ++ i) {
        printf("%0x ", v[i] & 0x0ff);
    }
    printf("\n");
}


int main() {

/* define some variables initialized by some "portable" constants */
	int8  i8    =  I8_C(0x01);
	int16 i16   = I16_C(0x0123);
	int32 i32   = I32_C(0x01234567);
	int64 i64   = I64_C(0x0123456789abcdef), j64 = I64_C(0);
	uint8 *p;
    	int   i;
	float32 f32 = F32_C(0.3232), tmp_f32;
	float64 f64 = F64_C(0.646464646464), tmp_f64;
    	unsigned char nf32[] = { 0x3e, 0xa5, 0x7a, 0x78 };
    	unsigned char nf64[] = { 0x3f, 0xe4, 0xaf, 0xd6, 0xa0, 0x52, 0xa8, 0x9c };
	
	
/* Portable printf (fprintf, ...) calls */

    	printf("i8=" X8_L ", i16=" X16_L ", i32=" X32_L ", i64 = " X64_L "\n", i8, i16, i32, i64);

/* Convert a number to network format */

    	j64 = hton_int64(i64);

/* perform some simple sanity checks */

	p = (uint8*) &j64;
	for (i = 0 ; i < 64/8 ; i += 2) {
	    if (p[i/2] != ((i<<4)|(i+1))) {
	    	printf("Error: conversion to network format contains an error (byte %d: %x != %x)!\n", i/2, (int) p[i/2], ((i<<4)|(i+1)));
		return -1;
	    }
	}

/* convert a number in network format to host format */
	
    	j64 = hton_int64(j64);
    	if (j64 != i64) {
	    	printf("Error: conversion from network format to host format contains an error!\n");	
		return -1;
	}

    	printf("f = " G32_L ", f64 = " G64_L "\n", f32, f64);
    	printf("f = " F32_L ", f64 = " F64_L "\n", f32, f64);
    	printf("f = " E32_L ", f64 = " E64_L "\n", f32, f64);
	
    	tmp_f32 = hton_float32(f32);
	
	if (tmp_f32 != *(float32*) nf32) {
	    printf("Error: float32 conversation to network format failed\n!");
	    return -1;
	}
    	tmp_f32 = ntoh_float32(tmp_f32);

    	if (tmp_f32 != f32) {
	    	printf("Error: float32 conversion failed (" G32_L "!=" G32_L ")\n", tmp_f32, f32);
		dump((unsigned char*) &tmp_f32, sizeof(tmp_f32));	
		dump((unsigned char*) &f32, sizeof(f32));	
		return -1;	
	}
	
    	tmp_f64 = hton_float64(f64);

	if (tmp_f64 != *(float64*) nf64) {
	    printf("Error: float64 conversation to network format failed\n!");
	    dump((unsigned char*) &tmp_f64, sizeof(tmp_f64));	
	    return -1;
	}
	
    	tmp_f64 = ntoh_float64(tmp_f64);

    	if (tmp_f64 != f64) {
	    	printf("Error: float64 conversion failed!\n");	
		dump((unsigned char*) &tmp_f64, sizeof(tmp_f64));	
		dump((unsigned char*) &f64, sizeof(f64));	
		return -1;	
	}
	
	printf("autoconfig.h passed simple consistency tests\n");  		
    	return 0;

}
