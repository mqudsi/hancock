typedef char foo;
typedef foo * string;

int sig_main(const string paramName default "hello"  <f:>,
             char * second <s:>){

  sfprintf(sfstdout, "Parameters: ");
  sfprintf(sfstdout, "paramName = %s, ", paramName);
  sfprintf(sfstdout, "second = %s\n", second);
}











