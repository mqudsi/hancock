#ifndef	__PHT_H_
#define	__PHT_H_

#include "hash.h"
#define	PHT_ERROR -1
#define	PHT_UNKNOWN HASH_DEFAULT
int ftruncate (int fildes, off_t length);
typedef	struct {
  hash_table *ht;
  char readonly;
  char updated;
} pht_rep;

int init_pht(Sfio_t *fp, pht_rep *data,	char readonly);

int flush_pht(Sfio_t *fp, pht_rep *data, char close);

pickle pht_p {init_pht => pht_rep => flush_pht};

int insert_pht(pht_p data, char	*s, unsigned int *h);
char *lookup_pht(pht_p data, unsigned int h);

hash_iter init_iter_pht(pht_p data);
int next_iter_pht(pht_p	data, hash_iter	i,
		  char **s, unsigned int *h);
void close_iter_pht(pht_p data,	hash_iter i);
#endif
