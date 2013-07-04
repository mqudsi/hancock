#include "hash.h"
#include <stdlib.h>
//#include <stdio.h>
#include <assert.h>
#include <string.h>

hash_value hash_default	= "";
unsigned int hash_function(char	* s) {
  unsigned int i = 0;
  unsigned int j;
  for (j=0; j<strlen(s); j++){
    i =	7*i + s[j];
  };
  return i % HASH_TABLE_SIZE;
}

hash_table * hash_empty() {
  hash_table *s	= (hash_table *)malloc(sizeof(hash_table));
  int i;

  assert(s!=NULL);
  for(i=0;i<HASH_TABLE_SIZE;i++)
    s->data[i]=HASH_DEFAULT;
  return s;
}

void hash_close(hash_table *s){
  free(s);
}

int hash_insert(hash_table *s, hash_value v, unsigned int *hash) {

  unsigned int index=hash_function(v);
  int offset;
  for(offset=0;offset<HASH_TABLE_SIZE;offset++)	{
    int	real_index = (offset+index) % HASH_TABLE_SIZE;

    if (HASH_DEFAULT ==	s->data[real_index] ) {
      s->data[real_index] = (char *)malloc(strlen(v) + 1);
      strcpy(s->data[real_index], v);
      *hash = real_index;
      return HASH_OK;
    }

    if (0 == strcmp(s->data[real_index],v)) {
      *hash = real_index;
      return HASH_OK;
    }

  }
  *hash	= HASH_ERROR;
  return HASH_FULL;
}

int hash_insert_pair(hash_table	*s, hash_value v, unsigned int hash){
   if (HASH_DEFAULT == s->data[hash] ) {
      s->data[hash] = (char *)malloc(strlen(v) + 1);
      strcpy(s->data[hash], v);
      return HASH_OK;
    }

    if (0 == strcmp(s->data[hash],v)) {
      return HASH_OK;
    }
    return HASH_ERROR;
}

hash_value hash_lookup(hash_table *s, unsigned int hash){
  return s->data[hash];
}


hash_iter hash_init_iter(hash_table *s){
  int *val=(int*)malloc(sizeof(int *));
  *val = 0;
  return val;
}

int hash_next(hash_table *s, hash_iter i, hash_value *v, unsigned int *hash){
  /* advance to next value */
  while	((*i < HASH_TABLE_SIZE)	&& (HASH_DEFAULT == s->data[*i])) (*i)++;

  if (*i < HASH_TABLE_SIZE) {
    *v = s->data[*i];
    *hash = *i;
    (*i)++;
    return HASH_ITER_FOUND;
  } else {
    return HASH_ITER_DONE;
  }
}

void hash_close_iter(hash_table	*s, hash_iter i) {
  free(i);
}
