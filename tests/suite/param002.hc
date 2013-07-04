#include "pickle01.hh"

directory dir_d (int dinit) {
  foo_p myPickle(:dinit + 4 :);
};

typedef struct foo_s {
  float f;
  int i; 
} foo_t;

directory dir2_d(int d2init, struct foo_s s2init){
  dir_d nestedDir (:d2init + 9 :);
  int g;
  int i default 3;
  char*  str  default "hello";
  foo_t s default {4.14, 1969};
  struct foo_s s2 default s2init;
  int arr[10] default {0,1,2,3,4,5,6,7,8,9};
};

int main() {
  struct foo_s s = {7.21, 1992};
  dir2_d myDir1(:13, s:)  = "myDir1";
  dir2_d myDir2 = "myDir2";
  myDir2 :=: myDir1;
  myDir1->i += 1;
  myDir1->nestedDir->myPickle->val += 1;
  myDir1->arr[5] += 1;
  strcpy(myDir1->str, "bye!");
  return 0;
}




