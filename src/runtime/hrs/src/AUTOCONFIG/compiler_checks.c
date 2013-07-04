#ifdef SH_COMMANDS
    CC=$1 
    echo "int main(int argc, char** argv) { __int64 x=0;return (int) x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo "#define HAS___int64 1"
	     echo "#undef  LONGLONG_t"
	     echo "#undef  LONGLONG_str"
	     echo "#define LONGLONG_t __int64"
	     echo '#define LONGLONG_str "__int64"'
    	else echo "#define HAS___int64 0"
    fi
    echo "int main(int argc, char** argv) { __uint64 x=0;return (int) x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo "#define HAS___uint64 1"
	     echo "#undef  ULONGLONG_t"
	     echo "#undef  ULONGLONG_str"
	     echo "#define ULONGLONG_t __uint64"
	     echo '#define ULONGLONG_str "___uint64"'
    	else echo "#define HAS___uint64 0"
    fi
    echo "int main(int argc, char** argv) { unsigned __int64 x=0;return (int) x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo "#define HAS___unsignedint64 1"
	     echo "#undef  ULONGLONG_t"
	     echo "#undef  ULONGLONG_str"
	     echo "#define ULONGLONG_t unsigned __int64"
	     echo '#define ULONGLONG_str "unsigned __int64"'
    	else echo "#define HAS___unsignedint64 0"
    fi
    echo "int main(int argc, char** argv) { long long x=0;return (int) x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo "#define HAS_LONGLONG 1"
	     echo "#undef  LONGLONG_t"
	     echo "#undef  LONGLONG_str"
	     echo "#define LONGLONG_t long long"
	     echo '#define LONGLONG_str "long long"'
    	else echo "#define HAS_LONGLONG 0"
    fi
    echo "int main(int argc, char** argv) { unsigned long long x=0;return (int) x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo "#define HAS_ULONGLONG 1"
	     echo "#undef  ULONGLONG_t"
	     echo "#undef  ULONGLONG_str"
	     echo "#define ULONGLONG_t unsigned long long"
	     echo '#define ULONGLONG_str "unsigned long long"'
    	else echo "#define HAS_LONGLONG 0"
    fi
    echo "int main(int argc, char** argv) { int x=0L;return x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo '#define ExtensionL_str "L"'
    	else echo '#define ExtensionL_str ""'
    fi
    echo "int main(int argc, char** argv) { int x=0LL;return x; } " >tmp_config.c
    if $CC tmp_config.c 2>/dev/null
    	then echo '#define ExtensionLL_str "LL"'
    	else echo '#define ExtensionLL_str ExtensionL_str'
    fi
    rm -f tmp_config.c
#endif
