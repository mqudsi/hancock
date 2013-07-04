#include "../scampRec.hh"


typedef struct{
  npanxx_r primary;
  line_r   second;
} p_line;

map aMap{
  key p_line;
  value int;
  default 9;
};

int sig_main(new aMap test <v:>){
 return 1;
}











