
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

int sig_main(new dir_d myDir <D:>){
  myDir->c = 'A';
  myDir->sc = 'M';
  myDir->s = 3240;
  myDir->ss = 4560;
  myDir->ui = 10;
  myDir->i = -11;
  myDir->ul = 231234;
  myDir->l = -1234;
  myDir->ull = 8675;
  myDir->ll =  -675;
  myDir->f = 1.234;
  myDir->d = 23.456;
  return 1;
}

