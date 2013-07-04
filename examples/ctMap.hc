#include "ctMap.hh"
int ctSqueeze(char set,	ct_p ctab, profile_t *from,
	      unsigned char *buffer, int32 bufferSize){
  /* Halt if no compression table. */
  if (!set) return HRS_ERROR;

  if (0	== ctab->level % 2)
    from->other	=  -1 *	from->other;
  if (sizeof(profile_t)	> bufferSize)
    return HRS_ERROR;
  memcpy ((profile_t *)buffer,
	  (void	const *)from, sizeof(profile_t));
  return sizeof(profile_t);
}

int ctUnsqueeze(char set, ct_p ctab, uint8 *buffer,
		int32 bufferSize, profile_t *to){
  /* Halt if no compression table. */
  if (!set) return HRS_ERROR;

  if (sizeof(profile_t)	> bufferSize)
    return HRS_ERROR;
  memcpy (to, (void const *)buffer, sizeof(profile_t));
  if (0	== ctab->level % 2)
    to->other =	-1 * to->other;
  return sizeof(profile_t);
}
