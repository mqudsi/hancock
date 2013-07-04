#ifndef	__HASH_H_
#define	__HASH_H_

/* Trivial linear hashing for strings. */
#define	HASH_TABLE_SIZE	100001

#define	HASH_OK	0
#define	HASH_FULL -1
#define	HASH_ERROR -1
#define	HASH_ITER_FOUND	1
#define	HASH_ITER_DONE 0
#define	HASH_DEFAULT 0

typedef	char * hash_value;

typedef	struct {
  hash_value data[HASH_TABLE_SIZE];
} hash_table;

/* Create new, empty hash-table. */
hash_table * hash_empty();

/* Close hash table */
void hash_close(hash_table *s);

/* Insert copy of v in table, return hash code as out parameter. 
 * If v already in table, returns existing hash. 
 * Return HASH_OK, HASH_ERROR or HASH_FULL. */
int hash_insert(hash_table *s, hash_value v, unsigned int *hash);

/* Insert copy of v in table with hash code hash. 
 * Return HASH_OK, HASH_ERROR or HASH_FULL. */
int hash_insert_pair(hash_table	*s, hash_value v, unsigned int hash);

/* Lookup hash in table, returns HASH_DEFAULT if no value present. */
hash_value hash_lookup(hash_table *s, unsigned int hash);

/* Iterator type for walking over hash table. */
typedef	int* hash_iter;

/* Create new iterator. */
hash_iter hash_init_iter(hash_table *s);

/* Get next hash value, hash code pair. 
 * Non-copy semantics for hash value. 
 * Return HASH_ITER_FOUND or HASH_ITER_DONE */
int hash_next(hash_table *s, hash_iter i,
	      hash_value *v, unsigned int *hash);

/* Close iterator. */
void hash_close_iter(hash_table	*s, hash_iter i);

#endif
