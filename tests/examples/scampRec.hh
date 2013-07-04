/* Header file for describing binary stream of call detail records. */
#ifndef	__SCAMPREC_H
#define	__SCAMPREC_H

#define	MAXNPANXX 1000000
#define	MAXNPA 1000
#define	MAXNXX 1000
#define	MAXLINE	10000
#define	MAXBASE	(MAXNXX*MAXLINE)
#define	TELNUMSIZE 10

#define	MINVALIDPN 2000000000LL
#define	MAXVALIDPN 10000000000LL

#define	extractNPA(pn) ((pn)/MAXBASE)
#define	extractNXX(pn) (((pn)%MAXBASE)/MAXLINE)
#define	extractNPANXX(pn) ((pn)/MAXLINE)
#define	extractLINE(pn)	((pn)%MAXLINE)

/* Physical representation */
typedef	struct {
  int onpa, obase;
  int dnpa, dbase;
  int con;
  int dur;
} pScampRec_t;

typedef	long long pn_t;
typedef	int areacode_t;
typedef	int exchange_t;
typedef	int Hdate_t;
typedef	int Htime_t;

/* Logical representation */
typedef	struct {
   pn_t	origin;
   pn_t	dialed;
   Hdate_t connecttime;
   Htime_t duration;
   char	isTollFree;
   char	isIncomplete;
   char	isIntl;
} scampRec_t;

int getValidCall(pScampRec_t *p, scampRec_t *h);

/* Translation function */
int getValidCall(pScampRec_t *p, scampRec_t *h)
{
  if (p->onpa <	200 || p->onpa > 999 )	return HRS_STREAM_DROP_REC;
  if ((p->obase	< 0) ||	p->obase > 9999999) return HRS_STREAM_DROP_REC;

  h->origin = ((long long) p->onpa)*MAXBASE + p->obase;

  if (p->dnpa == -15) {		 /* international */
    h->isIntl =	1;
    h->isTollFree = 0;
    h->dialed =	0;
  }
  else {			 /* not international */
    h->isIntl =	0;

    if ((p->dnpa  < 200) || (p->dnpa > 999))	  return HRS_STREAM_DROP_REC;
    if ((p->dbase < 0)	 || (p->dbase >	9999999)) return HRS_STREAM_DROP_REC;

    if ((p->dnpa == 800) || (p->dnpa ==	888) ||
	(p->dnpa == 877) || (p->dnpa ==	866) ||	(p->dnpa == 855))
      h->isTollFree = 1;
    else
      h->isTollFree = 0;

    h->dialed =	((long long) p->dnpa)*MAXBASE +	p->dbase;
  }
  h->isIncomplete = (0 == p->dur);
  h->connecttime     = p->con;
  h->duration	     = p->dur;
  return HRS_STREAM_KEEP_REC;
}

/* Stream declaration */
stream callDetail_s {
  getValidCall : pScampRec_t =>	scampRec_t;
};

/* Events */
munion line_e {: areacode_t npa_begin,
		 exchange_t nxx_begin,
		 pn_t line_begin,
		 scampRec_t call,
		 pn_t line_end,
		 exchange_t nxx_end,
		 areacode_t npa_end :};

/* Event detection code. */
line_e beginLineDetect(pn_t *prev, pn_t	*current) {
  areacode_t a = extractNPA(*current);
  exchange_t n = extractNPANXX(*current);

  if ((prev==NULL) || (extractNPA(*prev) != a))
    return {: npa_begin	= a, nxx_begin = n, line_begin = *current :};
  if (extractNPANXX(*prev) != n)
    return {: nxx_begin	= n, line_begin	= *current :};
  if (*prev != *current)
    return {: line_begin = *current :};

  return (line_e) {: :};
}

line_e endLineDetect(pn_t *current, pn_t *next){
 areacode_t a =	extractNPA(*current);
 exchange_t n =	extractNPANXX(*current);

  if ((next == NULL) ||	(a != extractNPA(*next)))
    return {: line_end = *current, nxx_end = n , npa_end = a :};
  if (n	!= extractNPANXX(*next))
    return {: line_end = *current, nxx_end = n :};
  if (*current != *next)
    return {: line_end = *current :};

  return (line_e) {: :};
}

line_e originDetect(scampRec_t *w[3:1])
{ line_e b,e;
  pn_t *prev, *curr, *next;

  prev = ((w[0]	== NULL) ? NULL	: &(w[0]->origin));
  curr = &(w[1]->origin);
  next = ((w[2]	== NULL) ? NULL	: &(w[2]->origin));
  b = beginLineDetect(prev,curr);
  e = endLineDetect(curr,next);

  return b :+: {: call = *w[1] :} :+: e;
}

#endif
