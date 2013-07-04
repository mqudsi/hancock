
#ifdef SH_COMMANDS
    CC=$1 
    if test -r tmp_config.c
        then echo "File tmp_config.c exists. Please remove." >& 2; exit -1;
    fi
    echo "int main(int argc, char** argv) { return 0; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    then echo "/* C compiler = " $CC " */"
    else echo "Cannot compile files! Using compiler '"$CC"'" >& 2; exit -1
    fi
    echo "struct S { int i; int a[0];}; int main(int argc, char** argv) { struct S s; return s.i; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    then echo "#define MIN_ARRAY_LENGTH 0"
    else echo "#define MIN_ARRAY_LENGTH 1"
    fi
    echo "int main(int argc) { int a[argc];  a[0] = 1; return a[0]; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    then echo "#define VAR_ARRAY_LENGTH 1"
    else echo "#define VAR_ARRAY_LENGTH 0"
    fi
    echo "int t; int main(int argc) { typeof(t); return 1; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    then echo "#define TYPE_OF          1"
    else echo "#define TYPE_OF          0"
    fi
    echo "int main() { char c[11]; int i = 0; *(int *)&c[1] = i; return 0; } " >tmp_config.c
    $CC -o tmp_config tmp_config.c 2>/dev/null
    if ./tmp_config 2>/dev/null
    then echo "#define ALIGN_DATA       0"  
    else echo "#define ALIGN_DATA       1"
    fi
    echo 'int main() { char c; c = (char) -1;  if (c >= 0) { printf("#define CHAR_SIGNED      0\n"); } else {  printf("#define CHAR_SIGNED      1\n");}}' >tmp_config.c
    $CC -o tmp_config tmp_config.c 2>/dev/null
    ./tmp_config
    printf '#include<sys/types.h>\n#include<netinet/tcp.h>\n#include<netdb.h>\n#include<stdio.h>\nint main() {\n#ifndef SOL_TCP\nstruct protoent *p = getprotobyname("TCP");\n' >tmp_config.c
    printf 'if (!p) {\nfprintf(stderr, "Cannot find protocol TCP!\\n");\nexit(-1);\n}\n printf("#define SOL_TCP %%d \\n", p->p_proto);\n#endif}\nreturn 0;}\n' >>tmp_config.c
    if $CC -o tmp_config tmp_config.c 2>/dev/null
    then ./tmp_config
    else
    	if $CC -o tmp_config tmp_config.c -lsocket -lnsl 2>/dev/null
	then ./tmp_config
	else echo "Don't know how to make this program!:" ; exit -1;
	fi
    fi
#
# HAS __FUNCTION__
#
    echo "int main() { char* p = __FUNCTION__ ; return (int) p; }" >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    then echo "#define HAS_MACRO_FUNCTION 1"
    else echo "#define HAS_MACRO_FUNCTION 0"
    	 echo '#define __FUNCTION__ "<function name unknown>"'
    fi
#
# HAS variable number of args
#
    printf "#define M(a...) f(##a)\n f(int a, int b) { return a+b; } int main() { return M(0,0); }" >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    then echo "#define HAS_VARARG_MACROS 1"
    else echo "#define HAS_VARARG_MACROS 0"
    fi
#
# HAS_ATTRIBUTE
#
    printf "#include <stdio.h>\nvoid d_prt(int level, const char* file, int line, const char* function, const char* format,...) __attribute__ ((__format__ (__printf__, 5, 6))); main() { return 0; }" >tmp_config.c
     if $CC tmp_config.c 2>/dev/null
    then echo "#define HAS_ATTRIBUTE 1"
    else echo "#define HAS_ATTRIBUTE 0"
    	 echo "#define __attribute__(a)"
    fi

     
# MSG_NOSIGNAL .. maybe more elaborate approach would be better
    printf "#ifndef MSG_NOSIGNAL\n#define MSG_NOSIGNAL 0\n#endif\n" 
#
# test for function "snprintf"
#
    printf '#include <stdio.h>\nint main() { char buf[10]; snprintf(buf, sizeof(buf), "Hello"); }' >tmp_config.c
    if $CC -o tmp_config tmp_config.c 2>/dev/null
    then echo "#define HAS_SNPRINTF 1"
    else echo "#define HAS_SNPRINTF 0"
    fi
#
# test for function "vsnprintf"
#
    printf '#include <stdarg.h>\n#include <stdio.h>\nint main() { va_list ap; char buf[10]; vsnprintf(buf, sizeof(buf), "Hello", ap); }' >tmp_config.c
    if $CC -o tmp_config tmp_config.c 2>/dev/null
    then echo "#define HAS_VSNPRINTF 1"
    else echo "#define HAS_VSNPRINTF 0"
    fi
#
# test for type socklen_t
#
    printf '#include <sys/types.h>\n#include <sys/socket.h>\n socklen_t s; int main() {}' >tmp_config.c
    if $CC -o tmp_config tmp_config.c 2>/dev/null
    then echo "#define HAS_SOCKLEN_T 1"
    else echo "#define HAS_SOCKLEN_T 0"
	 echo "typedef int socklen_t;"
    fi
#
# getrlimit constants
#
    printf "#ifndef RLIMIT_VMEM\n#ifdef RLIMIT_AS\n#define RLIMIT_VMEM RLIMIT_AS\n#endif\n#endif\n"
    
#
# test for OS
#
    OS=`uname`
    case $OS in
	Linux)  printf "#define IS_LINUX  1\n#define IS_IRIX 0\n#define IS_SOLARIS 0\n";;
	IRIX)   printf "#define IS_LINUX  0\n#define IS_IRIX 1\n#define IS_SOLARIS 0\n";;
	IRIX64)   printf "#define IS_LINUX  0\n#define IS_IRIX 1\n#define IS_SOLARIS 0\n";;
	SunOs)  printf "#define IS_LINUX  0\n#define IS_IRIX 0\n#define IS_SOLARIS 1\n";;
    esac
    printf "#ifndef RTLD_NEXT\n#define RTLD_NEXT ((char*) 0)\n#endif\n";
    rm -f tmp_config.c
    rm -f core
    echo "config terminated successfully! Please ignore any core dump messages!" >& 2; exit;


#endif
