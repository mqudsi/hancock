typedef	struct {
  int level;
  char readonly;
} ct_rep;

int init_ct(char set, int level,
	    Sfio_t *fp,	ct_rep *data, char readonly);
int flush_ct(Sfio_t *fp, ct_rep	*data, char close);

pickle ct_p(int	level) {init_ct(:level:) => ct_rep => flush_ct};

int get_ctl(ct_p data);

