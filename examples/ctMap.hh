
#include "limits.hh"
#include "pht.hh"
#include "ctp.hh"
#define	TOPN 5
#define	CTD PHT_UNKNOWN

typedef	struct {
  unsigned int tower[TOPN];
  float	count[TOPN];
  float	other;
} profile_t;
int ctSqueeze(char set,	ct_p ctab, profile_t *from,
	      uint8 *buffer, int32 bufferSize);

int ctUnsqueeze(char set, ct_p ctab, uint8 *from,
		int32 bufferSize, profile_t *to);

#define	CTM_DEFAULT  {{CTD, CTD, CTD, CTD, CTD}, \
		      {0.0, 0.0, 0.0, 0.0, 0.0}, \
		      0.0}

map cellTower_m(ct_p ctab)  {
  key (MINPN ..	MAXPN);
  split	(10000,	100);
  value	profile_t;
  default CTM_DEFAULT;
  compress ctSqueeze(:ctab:);
  decompress ctUnsqueeze(:ctab:);
};
munion ctProfile_e {: long long	phone :}
