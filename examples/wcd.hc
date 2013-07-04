#include "wcd.hh"
int getNext(pht_p p, char **s, int *lim, char delim, unsigned int *h){
  int i;     char t;	 int result = HASH_OK;

  for(i=0; (i<*lim) && (delim != (*s)[i]); i++){};

  if (i) {
    t =	(*s)[i];
    (*s)[i] = '\0';
    result = insert_pht(p,*s,h);
    (*s)[i] = t;
  } else *h = PHT_UNKNOWN;
  *s = *s + i +	1;
  *lim = *lim -	(i+1);
  return result;
}

int getValidCall(char paramSet,	pht_p p, Sfio_t	*fp, wc_t* lcr){
  int l, result;     char *s;

  if (!paramSet) return	HRS_STREAM_ERROR;    /* stream failure */

  result = sfscanf(fp, "%llu|%llu|", &(lcr->origin), &(lcr->dialed));
  if (2	!= result) return HRS_STREAM_DROP_REC;

  if ((MINPN > lcr->origin) || (MAXPN <	lcr->origin))
    return HRS_STREAM_DROP_REC;

  if ((MINPN > lcr->dialed) || (MAXPN <	lcr->dialed))
    return HRS_STREAM_DROP_REC;

  s = sfgetr(fp, '\n', 1);     /* read rest of line: cell tower info */
  if (NULL == s) return	HRS_STREAM_DROP_REC;
  l = sfvalue(fp);	       /* number of bytes just read. */

  result = getNext(p, &s, &l, '|', &lcr->poct);
  if (HASH_OK != result) return	HRS_STREAM_DROP_REC;

  result = getNext(p,&s, &l, '|', &lcr->soct);
  if (HASH_OK != result)  return HRS_STREAM_DROP_REC;

  result = getNext(p,&s, &l, '|', &lcr->pdct);
  if (HASH_OK != result)  return HRS_STREAM_DROP_REC;

  result = getNext(p,&s, &l, '|', &lcr->sdct);
  if (HASH_OK != result)  return HRS_STREAM_DROP_REC;

  return HRS_STREAM_KEEP_REC;			 /* valid record */
}

wline_e	originDetect (wc_t *w[3:1]){
  wc_t *prev = w[0];
  wc_t *current	= w[1];
  wc_t *next = w[2];
  wline_e b,e;

  if ((0 == prev) || (prev->origin != current->origin))
    b =	{: line_begin =	current->origin	:};
  else
    b =	(wline_e){: :};
  if ((0 == next) || (next->origin != current->origin))
    e =	{: line_end = current->origin :};
  else
    e =	(wline_e){: :};
  return b :+: {: call = *w[1] :} :+: e;
}

wline_e	dialedDetect (wc_t *w[3:1]){
  wc_t *prev = w[0];
  wc_t *current	= w[1];
  wc_t *next = w[2];
  wline_e b,e;

  if ((0 == prev) || (prev->dialed != current->dialed))
    b =	{: line_begin =	current->dialed	:};
  else
    b =	(wline_e){: :};
  if ((0 == next) || (next->dialed != current->dialed))
    e =	{: line_end = current->dialed :};
  else
    e =	(wline_e){: :};
  return b :+: {: call = *w[1] :} :+: e;
}
