#ifndef __SCAMPREC_H
#define __SCAMPREC_H

#define MAXNPANXX 1000000
#define MAXNPA 1000
#define MAXNXX 1000
#define MAXLINE 10000
#define MAXBASE (MAXNXX*MAXLINE)
#define TELNUMSIZE 10

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
range line_r   = [0 .. (MAXLINE-1)];

view phoneR(lpn_t, ppn_t){
  line_t <=> struct{
              npanxx_r primary;
              line_r secondary;
             };
  lpn_t(ppn){lpn_t lpn;
             lpn.npa = ppn.primary/1000;
             lpn.nxx = ppn.primary%1000;
             lpn.line = ppn.secondary;
             return lpn;}
  ppn_t(lpn){ppn_t ppn;
             ppn.primary = lpn.npa * 1000 + lpn.nxx;
             ppn.secondary = lpn.line;
             return ppn;}                
};



typedef int date_t;
typedef int Htime_t;

typedef struct {
   lpn_t origin;     
   lpn_t dialed;    
   lpn_t terminated;
   date_t connecttime;
   Htime_t duration;
   char isTollFree;
   char isIntl;
   char isIncomplete;
   char isCIC;
  } HscampRec;

typedef struct
{
  int onpa, obase;
  int tnpa, tbase;
  int dnpa, dbase;
  int con;
  int dur;  
} CscampRec;


int getvalidcall(CscampRec *c, HscampRec *h)
{ 
  /* origin */
  if (c->onpa < 200 || c->onpa > 999 ) 
    return 0; 

  if ((c->obase < 0) || c->obase > 9999999) 
    return 0;

  h->origin.npa      = c->onpa;
  h->origin.nxx      = c->obase/MAXLINE;
  h->origin.line     = c->obase%MAXLINE;

  if (c->dnpa == -15) {
    /* intl */
    h->isIntl = 1;
    h->isTollFree = 0;
    if (c->tnpa == -10)
      h->isIncomplete = 1;
    else 
      h->isIncomplete = 0;
    if (c->tnpa == -15)
      h->isCIC = 1;
    else
      h->isCIC = 0;

    h->dialed.npa = 0;
    h->dialed.nxx = 0;
    h->dialed.line = 0;
    h->terminated = h->dialed;
  }
  else {
    /* not intl */
    h->isIntl = 0;

    if (((c->tnpa < 0) && (c->tnpa != -10) && (c->tnpa != -15)) ||
	(c->tnpa > 999))
      return 0;

    if ((c->dnpa < 200) || (c->dnpa > 999))
      return 0; 

    if ((c->dbase < 0) || (c->dbase > 9999999))
      return 0;

    if ((c->tbase < 0 ) || (c->tbase > 9999999))
      return 0;

    if ((c->dnpa == 800) || (c->dnpa == 888) || 
	(c->dnpa == 877) || (c->dnpa == 866))
      h->isTollFree = 1;
    else
      h->isTollFree = 0;

    h->dialed.npa      = c->dnpa;
    h->dialed.nxx      = c->dbase/MAXLINE;
    h->dialed.line     = c->dbase%MAXLINE;
    
    /* INCOMPLETE */
    if (c->tnpa == -10) {
      h->isIncomplete = 1;
      h->isCIC = 0;
      h->terminated = h->dialed;
    }
    /* CIC */
    else if (c->tnpa == -15) {
      h->isCIC = 1;
      h->isIncomplete = 0;
      h->terminated = h->dialed;
    }
    else {
      h->isCIC = 0;
      h->isIncomplete = 0;
      h->terminated.npa  = c->tnpa;
      h->terminated.nxx  = c->tbase/MAXLINE;
      h->terminated.line = c->tbase%MAXLINE;
    }
  }

  h->connecttime     = c->con;
  h->duration        = c->dur;

  return 1;
}

stream cdStream { getvalidcall : CscampRec => HscampRec; };



munion line_e {: areacode_t npa_begin, exchange_t nxx_begin, 
		  lpn_t line_begin, areacode_t npa_end, exchange_t nxx_end,
		  lpn_t line_end, HscampRec call :};


munion scamp_e {: line_e o, line_e d, line_e t :};

line_e beginLineDetect(lpn_t *prev, lpn_t *current)
{
  areacode_t a;
  exchange_t n;

  a.npa = current->npa;
  n.npa = current->npa;
  n.nxx = current->nxx;

  /* XXX - can we count on short-circuit evaluation ? */
  if ((prev==NULL) || (prev->npa != current->npa))
    return {: npa_begin = a, nxx_begin = n, line_begin = *current :};

  if (prev->nxx != current->nxx)
    return {: nxx_begin = n, line_begin = *current :};

  if (prev->line != current->line) 
    return {: line_begin = *current :};
  
  return (line_e) {: :};
}


line_e endLineDetect(lpn_t *current, lpn_t *next)
{
  areacode_t a;
  exchange_t n;

  a.npa = current->npa;
  n.nxx = current->nxx;
  n.npa = current->npa;

  if ((next == NULL) || (current->npa != next->npa))
    return {: line_end = *current, npa_end = a, nxx_end = n :};

  if (current->nxx != next->nxx)
    return {: line_end = *current, nxx_end = n :};

  if (current->line != next->line)
    return {: line_end = *current :};

  return (line_e) {: :};
}




static line_e internalLineDetect(lpn_t *prev, lpn_t *curr, lpn_t *next) {

  line_e b,e;

  b = beginLineDetect(prev,curr);
  e = endLineDetect(curr,next);

  return b :+: e;
}

line_e lineDetect(lpn_t *w[3:1]) {
  return internalLineDetect(w[0],w[1],w[2]);
}

#define CHOOSE(w,f) (((w)==NULL) ? NULL : &((w)->f))

line_e callDetect(HscampRec *w[3:1]) {
  return {: call = *w[1] :};
}

line_e originDetect(HscampRec *w[3:1]) {
  line_e res;

  res = internalLineDetect(CHOOSE(w[0],origin),
			   &w[1]->origin,
			   CHOOSE(w[2],origin));
  return res :+: {: call = *w[1] :};
}

line_e dialedDetect(HscampRec *w[3:1]) {
  line_e res;

  res = internalLineDetect(CHOOSE(w[0],dialed),
			   &w[1]->dialed,
			   CHOOSE(w[2],dialed));
  return res :+: {: call = *w[1] :};
}

line_e termDetect(HscampRec *w[3:1])
{
  line_e res;

  res = internalLineDetect(CHOOSE(w[0],terminated),
			   &w[1]->terminated,
			   CHOOSE(w[2],terminated));
  return res :+: {: call = *w[1] :};
}

#undef CHOOSE


scamp_e scampDetect(HscampRec *w[3:1]) {

  return {:   o = originDetect(w),
	      d = dialedDetect(w),
	      t = termDetect(w) :};
}

line_e mapDetect(lpn_t *w[1:0]) {

  return {: line_begin = *w[0] :};
}

#endif
