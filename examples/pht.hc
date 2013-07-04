#include "pht.hh"
/* File representation:
 * hash0:len:string_0
 * hash1:len:string_1
 * ...
 * hashn:len:string_n
 */

int init_pht(Sfio_t *fp, pht_rep *data,	char readonly){
  int result, len;
  unsigned int hash;
  char *s;
  data->ht = hash_empty();
  data->readonly = readonly;
  data->updated	= 0;
  if (0	== sfsize(fp)) return HRS_OK; /* empty file, okay */
  else
    while (!sfeof(fp)) {
      if (2 != sfscanf(fp, "%u:%d:", &hash, &len))
	return HRS_ERROR;
      s	= (char	*)malloc(len+1);
      if (NULL == s) return HRS_ERROR;
      if (1 != sfscanf(fp, "%s\n", s)) return HRS_ERROR;
      if (HASH_OK != hash_insert_pair(data->ht,	s, hash))
	return HRS_ERROR;
    }
  return HRS_OK; /* okay */
}

int flush_pht(Sfio_t *fp, pht_rep *data, char close){
  /* add error checking. */
  int len;
  char *s;
  unsigned int hash;
  hash_iter i;

  if ((!data->readonly)	&& (data->updated)) {
    Sfoff_t curLoc;
    sfseek(fp, (Sfoff_t) 0, SEEK_SET);
    i =	init_iter_pht(data);
    while (HASH_ITER_FOUND == next_iter_pht(data, i, &s, &hash)) {
      sfprintf(fp, "%u:%d:", hash, strlen(s));
      sfprintf(fp, "%s\n", s);
    }
    close_iter_pht(data, i);
    curLoc = sfseek(fp,	(Sfoff_t) 0, SEEK_CUR);
    ftruncate(sffileno(fp), curLoc);
  }
  if (close)
    hash_close(data->ht);
  return HRS_OK; /* okay */
}
int insert_pht(pht_p data, char	*s, unsigned int *h){
  if (!data->readonly){
    data->updated = 1;
    return hash_insert(data->ht, s, h);
  } else
    return PHT_ERROR;
}

char* lookup_pht(pht_p data, unsigned int h){
  return hash_lookup(data->ht, h);
}

hash_iter init_iter_pht(pht_p data){
  return hash_init_iter(data->ht);
}

int next_iter_pht(pht_p	data, hash_iter	i,
		  char **s, unsigned int *h){
  return hash_next(data->ht, i,	s, h);
}

void close_iter_pht(pht_p data,	hash_iter i){
  hash_close_iter(data->ht, i);
}
