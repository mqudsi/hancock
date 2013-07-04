/* implemented in map.c*/
int32 flushStripesInMap(HRSmap_t m, int all);
void flushSomeStripesOFFDIR(HRSmap_t m);

/* implemented in util.c */
void HRSerrorExit(const char *s);
void HRSerrorExit1(const char *s, char *d);
void HRSfree(void *s);

/* implemented in hrs.c */
void HRSexitSetup();

/* implemented in stream.c */
void HRSstreamDone();
int amrRSORT(int argc, char *const *argv);

