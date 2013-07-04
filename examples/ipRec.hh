/* Header file for describing general stream of tcpdump data,
 * includes stream for describing stream of ip addresses.      */

#define	MAXOCT 256
#define	MAXPRIMARY (MAXOCT*(MAXOCT-1)+(MAXOCT-1))

void longToDot(unsigned	long l,
	       unsigned	char *v0, unsigned char	*v1,
	       unsigned	char *v2, unsigned char	*v3){
  *v0 =	l % MAXOCT;    l = l / MAXOCT;
  *v1 =	l % MAXOCT;    l = l / MAXOCT;
  *v2 =	l % MAXOCT;    l = l / MAXOCT;
  *v3 =	l % MAXOCT;
}

unsigned long dotToLong(unsigned char v0, unsigned char	v1,
			unsigned char v2, unsigned char	v3){
  return v3 * MAXOCT * MAXOCT *	MAXOCT +
	 v2 * MAXOCT * MAXOCT +
	 v1 * MAXOCT + v0;
}

/* Logical representation of ip address. */
typedef	 struct	{
  unsigned char	v0;
  unsigned char	v1;
  unsigned char	v2;
  unsigned char	v3;
  unsigned long	hash_value;
} ipAddr_t;

/* Logical representation of tcmdump record. */
typedef	struct{
  unsigned int ts1, ts2;
  unsigned char	version;
  unsigned char	tos;
  unsigned short length;
  unsigned short identification;
  unsigned short fragment;
  unsigned char	ttl;
  unsigned char	protocol;
  ipAddr_t source;
  ipAddr_t dest;
} ipPacket_t;


void printAddr(ipAddr_t	l){
  sfprintf(sfstdout, "%d.%d.%d.%d\n", l.v0, l.v1, l.v2,	l.v3);
}

void printPacketInfo(ipPacket_t	*p){
  sfprintf(sfstdout, "ts1: %d\t ts2: %d\n", p->ts1, p->ts2);
  sfprintf(sfstdout, "version: %d\n", p->version);
  sfprintf(sfstdout, "tos: %d\n", p->tos);
  sfprintf(sfstdout, "length: %d\n", p->length);
  sfprintf(sfstdout, "identification: %d\n", p->identification);
  sfprintf(sfstdout, "fragment: %d\n", p->fragment);
  sfprintf(sfstdout, "ttl: %d\n", p->ttl);
  sfprintf(sfstdout, "protocol: %d\n", p->protocol);
  sfprintf(sfstdout, "source: \t");
  printAddr(p->source);
  sfprintf(sfstdout, "destination: \t");
  printAddr(p->dest);
  sfprintf(sfstdout, "\n\n");
}

void eatToEOL(Sfio_t *input)
{ int c;
   while (((c =	sfgetc(input)) != EOF) && (c !=	'\n'))
     ;
}

/* Translation function for general stream of tcmdump packets. */
int getValidIPPacket(Sfio_t *input, ipPacket_t *ipP)
{ int v0, v1, v2, v3;
  unsigned int c0, c1;
  unsigned int s0;
  int found;

  found	= sfscanf(input, "%d.%d|IP|", &ipP->ts1, &ipP->ts2);
  if ((found ==	EOF) ||	(found != 2)) return HRS_STREAM_DROP_REC;

  found	= sfscanf(input, "%d.%d.%d.%d|", &v0, &v1, &v2,	&v3);
  if ((found ==	EOF) ||	(found != 4)) return HRS_STREAM_DROP_REC;
  if ((v0 < 0) || (v0 >	MAXOCT)	||
      (v1 < 0) || (v1 >	MAXOCT)	||
      (v2 < 0) || (v2 >	MAXOCT)	||
      (v3 < 0) || (v3 >	MAXOCT)) {
    sfprintf(sfstdout, "%d.%d\n", ipP->ts1, ipP->ts2);
    eatToEOL(input);
    return HRS_STREAM_DROP_REC;
  }
  ipP->source.v0 = v0; ipP->source.v1 =	v1;
  ipP->source.v2 = v2; ipP->source.v3 =	v3;
  ipP->source.hash_value = dotToLong(v0,v1,v2,v3);

  found	= sfscanf(input, "%d.%d.%d.%d|", &v0, &v1, &v2,	&v3);
  if ((found ==	EOF) ||	(found != 4)) return HRS_STREAM_DROP_REC;

  if ((v0 < 0) || (v0 >	MAXOCT)	||
      (v1 < 0) || (v1 >	MAXOCT)	||
      (v2 < 0) || (v2 >	MAXOCT)	||
      (v3 < 0) || (v3 >	MAXOCT)) {
    sfprintf(sfstdout, "%d.%d\n", ipP->ts1, ipP->ts2);
    eatToEOL(input);
    return HRS_STREAM_DROP_REC;
  }
  ipP->dest.v0 = v0; ipP->dest.v1 = v1;
  ipP->dest.v2 = v2; ipP->dest.v3 = v3;
  ipP->dest.hash_value = dotToLong(v0,v1,v2,v3);

  found	= sfscanf(input, "%d|%d|", &c0,	&c1);
  if ((found ==	EOF) ||	(found != 2)) return HRS_STREAM_DROP_REC;
  ipP->version = (char)	c0;
  ipP->protocol	= (char) c1;

  if (ipP->version != 4) {
    sfprintf(sfstdout, "%d.%d",	ipP->ts1, ipP->ts2);
    eatToEOL(input);
    return HRS_STREAM_DROP_REC;
  }

  found	= sfscanf(input, "%d|",	&s0);
  if ((found ==	EOF) ||	(found != 1)) return HRS_STREAM_DROP_REC;
  ipP->length =	(short)	s0;

  found	= sfscanf(input, "%d|",	&s0);
  if ((found ==	EOF) ||	(found != 1)) return HRS_STREAM_DROP_REC;
  ipP->identification =	(short)	s0;

  found	= sfscanf(input, "%d|",	&c0);
  if ((found ==	EOF) ||	(found != 1)) return HRS_STREAM_DROP_REC;
  ipP->ttl = (char) c0;

  found	= sfscanf(input, "%d|",	&c0);
  if ((found ==	EOF) ||	(found != 1)) return HRS_STREAM_DROP_REC;
  ipP->tos = (char) c0;

  found	= sfscanf(input, "%d|",	&s0);
  if ((found ==	EOF) ||	(found != 1)) return HRS_STREAM_DROP_REC;
  ipP->fragment	= s0;

  eatToEOL(input);
  return HRS_STREAM_KEEP_REC;
}

/* General stream declaration for tcpdump stream. */
stream ipPacket_s { getValidIPPacket : Sfio_t => ipPacket_t;};

/* Translation function for general stream of ip addresses. */
int getvalidIPaddr(Sfio_t *input, ipAddr_t *addr)
{
  int v0, v1, v2, v3, found;

  found	= sfscanf(input, "%d.%d.%d.%d\n", &v0, &v1, &v2, &v3);
  if ((found ==	EOF) ||	(found != 4))
    return HRS_STREAM_DROP_REC;
  if ((v0 < 0) || (v0 >	MAXOCT)	||
      (v1 < 0) || (v1 >	MAXOCT)	||
      (v2 < 0) || (v2 >	MAXOCT)	||
      (v3 < 0) || (v3 >	MAXOCT)) {
    return HRS_STREAM_DROP_REC;
 }
  addr->v0 = v0;   addr->v1 = v1;
  addr->v2 = v2;   addr->v3 = v3;
  addr->hash_value = dotToLong(v0,v1,v2,v3);
  return HRS_STREAM_KEEP_REC;
}

/* General stream declaration for stream of ip addresses. */
stream ipAddr_s	{ getvalidIPaddr : Sfio_t => ipAddr_t;};



