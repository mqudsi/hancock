#ifndef	__HASH_H
#define	__HASH_H

/* Trivial linear hashing. */

#define	HASH_EMPTY -1
#define	HASH_TABLE_SIZE	100001
#define	HASH_FULL -1
#define	HASH_OK	0
#define	HASH_OVERWRITTEN 1
#define	HASH_DEFAULT -1

typedef	int hash_value;
typedef	struct {
  unsigned int key[HASH_TABLE_SIZE];
  hash_value data[HASH_TABLE_SIZE];
} hash_table;

hash_table * hash_empty(void);

/* return DEFAULT if no value present. */
hash_value hash_get(hash_table *s, unsigned int	m);

int hash_insert(hash_table *s, unsigned	int k, hash_value v);


#endif

