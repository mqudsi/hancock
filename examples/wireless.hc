#include "ctDir.hh"
#define	ORIGIN 0
#define	DIALED 1
#define	LAMBDA 0.15

int completeOriginCellCall(wc_t	*w){
  return (w->poct != CTD);
}

int completeDialedCellCall(wc_t	*w){
  return (w->pdct != CTD);
}

void printProfile(profile_t *p,	pht_p pht){
  int i;
  char *unknownTower = "Unknown";
  char *towerName;
  for (i=0;i<TOPN;i++){
    towerName =	lookup_pht(pht,	p->tower[i]);
    if (towerName == PHT_UNKNOWN)
      towerName	= unknownTower;

    sfprintf(sfstdout, "Tower: %s, weight = %f\n",
	     towerName,	p->count[i]);
  };
  sfprintf(sfstdout, "Other weight = %f\n", p->other);
}

int check(long long *k){
  sfprintf(sfstdout, "map key = %llu\n", *k);
  return 1;
}

void printMapRange(pht_p pht, cellTower_m m, long long lo, long	long hi){
  iterate(
      over m[lo	.. hi]
      filteredby check
      withevents (w[1:0]) {: phone = *w[0] :}
      ){

       event phone(long	long k){
	 profile_t p;
	 sfprintf(sfstdout, "lookup key %lld\n", k);
	 p = m<:k:>;
	 printProfile(&p, pht);
       }
  }
}



void initProfile(profile_t *p){
  int i;
  for(i=0;i<TOPN;i++){
    p->tower[i]	= CTD;
    p->count[i]	= 0.0;
  };
  p->other = 0.0;
}

void aggregateTower(profile_t *p, unsigned int tower){
  static int count = 0;
  static int slotcount = 0;
  int i;
  count++;

  for (i=0; i<TOPN; i++){
    if (p->tower[i] == tower){	    /* already seen tower */
      p->count[i] += 1.0;	    /* bump count */
      break;
    } else if (p->tower[i] == CTD) {  /* have an empty slot */
      p->tower[i] = tower;	      /* store tower */
      p->count[i] = 1.0;	      /* set count to 1 */
      break;
    }
  }
  if (TOPN == i) {		     /* tower didn't fit */
    int	try = count % (TOPN + 1);    /* bumping */
    if (TOPN !=	try) {		     /* TOPN out of TOPN + 1 times */
      p->other += 1.0;		     /* increment other field */
    } else {			     /* 1 out of (TOPN + 1) times */
      int slot = slotcount % TOPN;   /* get next slot */
      p->other += p->count[slot];    /* move count of slot to other */
      p->tower[slot] = tower;	     /* insert tower into slot */
      p->count[slot] = 1.0;	     /* set tower's count to 1.0 */
    }
  }
}

void aggregate(profile_t *p, wc_t lcr, int dir){
  unsigned int primary,	secondary;

  if (ORIGIN ==	dir) {
    primary = lcr.poct;
    secondary =	lcr.soct;
  } else {
    primary = lcr.pdct;
    secondary =	lcr.sdct;
  }
  aggregateTower(p,primary);
  if (CTD != secondary)
    aggregateTower(p,secondary);
}

void integrateTower(profile_t *p, unsigned int tower, float tcount){
  static int count = 0;
  static int slotcount = 0;
  int i;
  count++;

  for (i=0; i<TOPN; i++){
    if (p->tower[i] == tower){	      /* already seen tower */
      p->count[i] += tcount;
      break;
    } else if (p->tower[i] == CTD) {  /* have an empty slot */
      p->tower[i] = tower;	      /* store tower */
      p->count[i] = tcount;	      /* set count to 1 */
      break;
    }
  }
  if (TOPN == i) {		     /* tower didn't fit */
    int	try = count % (TOPN + 1);    /* bumping */
    if (TOPN !=	try) {		     /* TOPN out of TOPN + 1 times */
      p->other += tcount;	     /* blend other field */
    } else {			     /* 1 out of (TOPN + 1) times */
      int slot = slotcount % TOPN;   /* get next slot */
      p->other += p->count[slot];    /* move count of slot to other */
      p->tower[slot] = tower;	     /* insert tower into slot */
      p->count[slot] = tcount;	     /* set tower's count to 1.0 */
    }
  }
}


void integrate(profile_t *op, profile_t	*tp){
  int i;
  for(i=0; i<TOPN; i++){
    integrateTower(op, tp->tower[i], tp->count[i] * LAMBDA);
  };
  op->other += (tp->other * LAMBDA);
}

void doOrigin(char *callsLoc, cellTower_m m, pht_p pht){
  wireless_s calls(:pht:) = callsLoc;
  profile_t p;
  iterate (
    over calls
    filteredby completeOriginCellCall
    sortedby origin
    withevents originDetect){

    event line_begin(long long origin){
      initProfile(&p);
    }

    event call(wc_t lcr){
      aggregate(&p, lcr, ORIGIN);
    }

    event line_end(long	long origin){
      profile_t	op2;
      profile_t	op = m<:origin:>;
      integrate(&op,&p);
      m<:origin:> = op;
      op2 = m<:origin:>;
    }
  }
}

void doDialed(char *callsLoc, cellTower_m m, pht_p pht){
  wireless_s calls(:pht:) = callsLoc;
  profile_t p;
  iterate (
    over calls
    filteredby completeDialedCellCall
    sortedby dialed
    withevents dialedDetect){

    event line_begin(long long dialed){
      initProfile(&p);
    }

    event call(wc_t lcr){
      aggregate(&p, lcr, DIALED);
    }

    event line_end(long	long dialed){
      profile_t	op = m<:dialed:>;
      integrate(&op,&p);
      m<:dialed:> = op;
    }
  }
}

void age(cellTower_m m){
  iterate( over	m ){
       event phone(long	long *k){
	 int i;
	 profile_t p = m<:*k:>;
	 for (i=0;i<TOPN;i++){
	   p.count[i] *= (1 - LAMBDA);	 /* age existing data */
	 }
	 p.other *= (1 - LAMBDA);
	 m<:*k:> = p;
     }
  }
}

int sig_main(const exists cellTower_d oldCT <d:, "Old directory">,
	     new cellTower_d newCT <D:,	"New directory">,
	     char *callsLoc <c:, "Wireless call detail">,
	     char *date	<t:, "Processing date">	 default "unspecified" ) {

  newCT	:=: oldCT;

  age(newCT->outMap);
  age(newCT->inMap);

  printMapRange(newCT->ctHashTable,newCT->outMap,MINPN,MAXPN);
  printMapRange(newCT->ctHashTable,newCT->inMap,MINPN,MAXPN);

  doOrigin(callsLoc,newCT->outMap, newCT->ctHashTable);
  doDialed(callsLoc,newCT->inMap, newCT->ctHashTable);

  printMapRange(newCT->ctHashTable,newCT->outMap,MINPN,MAXPN);
  printMapRange(newCT->ctHashTable,newCT->inMap,MINPN,MAXPN);
  newCT->lastUpdated = date;
  sfprintf(sfstdout, "Date of last update = %s.\n", newCT->lastUpdated);
}
