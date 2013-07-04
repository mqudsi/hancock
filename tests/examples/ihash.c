#include "ihash.h"
#include <stdio.h>
#include <stdlib.h>
int hash_function(unsigned int e) { return e % HASH_TABLE_SIZE;	}

hash_table * hash_empty() {
  hash_table *s	= (hash_table *)malloc(sizeof(hash_table));
  int i;

  for(i=0;i<HASH_TABLE_SIZE;i++)
    s->key[i]=HASH_EMPTY;

  return s;
}

hash_value hash_get(hash_table *s, unsigned int	m) {

  int index = hash_function(m);
  int offset;
  for(offset=0;offset<HASH_TABLE_SIZE;offset++)	{
    int	real_index = (offset+index) % HASH_TABLE_SIZE;

    if(s->key[real_index]==m) return s->data[real_index];
    if(s->key[real_index]==HASH_EMPTY) return HASH_DEFAULT;
    }

  return HASH_DEFAULT;
}

int hash_insert(hash_table *s, unsigned	int k, hash_value v) {

  int index=hash_function(k);
  int offset;

  for(offset=0;offset<HASH_TABLE_SIZE;offset++)	{
    int	real_index = (offset+index) % HASH_TABLE_SIZE;

    if(s->key[real_index] == HASH_EMPTY) {
      s-> key[real_index] = k;
      s->data[real_index] = v;
      return HASH_OK;
    }

    if(s->key[real_index] == k)	{
      s->data[real_index] = v;
      return HASH_OVERWRITTEN;
    }

  }

  return HASH_FULL;
}
