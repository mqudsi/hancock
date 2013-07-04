#ifndef __CKIT_REPLACE_H__
#define __CKIT_REPLACE_H__

#ifdef __OSX_PREPROCESSOR_FIXES
#define _AST_STD_H
typedef void* __builtin_va_list;

/* Fr OSByteOrder.h */
#define OS_INLINE
#include <stdint.h>
extern uint64_t OSSwapConstInt64(uint64_t data);
extern uint32_t OSSwapConstInt32(uint32_t data);
extern uint16_t OSSwapConstInt16(uint16_t data);
#include "/usr/include/libkern/machine/OSByteOrder.h"
#define __DARWIN_ALIAS(x) 
#define __asm(x)
#endif

#ifdef __LINUX_PREPROCESSOR_FIXES
typedef void* __builtin_va_list;
#define __THROW
extern int ftruncate (int __fd, long int __length) ;

#ifndef GETOPT
extern int     optind;
extern char    *optarg;
extern int     opterr;
extern int     optopt;
extern int     getopt(int argc, char *const *argv, const char *optstring);
#endif
#endif

#endif
