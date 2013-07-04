
directory dir_d {
  unsigned char c;
  signed char sc;
  unsigned short s;
  signed short ss;
  unsigned int ui;
  int i;
  unsigned long ul;
  long l;
  unsigned long long ull;
  long long ll;
  float f;
  double d;
};

int sig_main(exists dir_d myDir <d:, "existing directory">, 
                new dir_d myNewDir <D:, "new directory">){
  myNewDir :=: myDir;
  return 0;
}

