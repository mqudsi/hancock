#include "ctp.hh"
#define	COMPRESSION_LEVEL_DEFAULT   6

/* truncate file fd to length. */
int ftruncate (int fd, off_t length);

int init_ct(char set, int level, Sfio_t	*fp,
	    ct_rep *data, char readonly){
  if (0	== sfsize(fp)) {     /* empty file */
    if (set)			    /* Was parameter given? */
      data->level = level;	    /* Yes: Use parameter */
    else			    /* No:  Use default */
      data->level = COMPRESSION_LEVEL_DEFAULT;
    return HRS_OK;
    /* Construct empty compression table */
  } else {
    int	result = sfscanf(fp, "%d", &data->level);
    if (1 == result)
      return HRS_OK; /* Retrieved compression level from disk. */
    /* Intialize compression table from contents of file. */
  }
  return HRS_ERROR; /* Couldn't initialize compression table. */
}

int flush_ct(Sfio_t *fp, ct_rep	*data, char close){
  Sfoff_t curLoc;

  /* Seek to beginning of the file. */
  sfseek(fp, (Sfoff_t) 0, SEEK_SET);
  sfprintf(fp, "%d\n", data->level);

  /* Find current seek address. */
  curLoc = sfseek(fp, (Sfoff_t)	0, SEEK_CUR);

  /* Truncate file fp to curLoc (end of currnet contents).  */
  ftruncate(sffileno(fp), curLoc);
  return HRS_OK;
}
int get_ctl(ct_p data){
  return data->level;
}
