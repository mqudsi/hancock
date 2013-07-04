#define NUMDUR 5
#define NUMTOD 3
#define NUMLABELS 6

struct bits {
  char in_dur_bv[NUMDUR];
  char out_dur_bv[NUMDUR];

  /* bits 0-2 represent durations 0-2 */
  char in_tod_bv[NUMTOD];
  char out_tod_bv[NUMTOD];

  /* Biz == 0, Res == 1,  Unknown == 2  !!!! Check this !!! */
  /* service == 3, tollfree == 4, international ==5 */

  char in_labels_bv[NUMLABELS];
  char out_labels_bv[NUMLABELS];
};


/* Features2 directory */
directory brDirF2 {
  int week;
  struct bits satBits;
  struct bits sunBits;
  struct bits weekdayBits;
};

void initBits(struct bits * theBits, char init){
  char i;
  for(i=0; i<NUMDUR; i++){
    theBits->in_dur_bv[i] = init + i;
    theBits->out_dur_bv[i] = 2*init + i;
  }
}

int sig_main(new brDirF2 myDir <D:>){
 myDir->week = 1;
 
 return 1;
}
