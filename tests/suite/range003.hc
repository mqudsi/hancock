range npanxx = [200000 .. 999999];
range line   = [0000 .. 9999];

typedef struct{
  npanxx primary;
  line   second;
} p_line;

map aMap{
  key p_line;
  value int;
  default 0;
};


int sig_main(){
 return 1;
}


