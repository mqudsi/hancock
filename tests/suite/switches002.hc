void sig_main(int data <i:, "command-line integer ">,
              char c <c:, "command-line character">,
	      double d <d:, "command-line float">,
              char f <b, "command-line boolean">){
      sfprintf(sfstdout, "i=%d\n", data);
      sfprintf(sfstdout, "c=%c\n", c);
      sfprintf(sfstdout, "d=%3.2f\n", d);
      if (f) 
	sfprintf(sfstdout, "b flag was present.\n");
      else	       
	sfprintf(sfstdout, "b flag was not present.\n");
}

