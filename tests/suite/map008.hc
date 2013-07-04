#define MAXNPANXX 1000000
#define MAXSTRIPE 100
#define MAXENTRY  100
#define MAXNXX    1000

typedef struct {
  short int npa;
} areacode_t;

typedef struct {
  short int npa;
  short int nxx;
} exchange_t;

typedef struct {
  short int npa;
  short int nxx;
  int line;
} line_t;

range npanxx_r = [0 .. (MAXNPANXX-1)];
range stripe_r = [0 .. (MAXSTRIPE-1)];
range entry_r   = [0 .. (MAXENTRY-1)];

view phoneR(lpn_t, ppn3_t){
  line_t <=> struct{
              npanxx_r primary;
	      stripe_r secondary;
              entry_r tertiary;
             };
  lpn_t(ppn){lpn_t lpn;
             lpn.npa = ppn.primary/MAXNXX;
             lpn.nxx = ppn.primary%MAXNXX;
             lpn.line = ppn.secondary * ppn.tertiary;
             return lpn;}
  ppn3_t(lpn){ppn3_t ppn;
             ppn.primary = lpn.npa * MAXNXX + lpn.nxx;
             ppn.secondary = lpn.line / MAXENTRY;
	     ppn.tertiary = lpn.line % MAXENTRY;
             return ppn;}                
};

map foo_m {
  key ppn3_t;
  value int;
  default 12;
};

int sig_main(foo_m foo <m:>) {
    ppn3_t p3 = {973984, 31, 67};
    int x;
    x = foo<:p3:>;
    sfprintf(sfstdout, "value : %d.\n", x);
    foo<:p3:> =  foo<:p3:> + 1;
    x = foo<:p3:>;
    sfprintf(sfstdout, "value : %d.\n", x);
    return 1;
}


