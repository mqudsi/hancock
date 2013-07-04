#include <stdio.h>

/* Reads ascii call detail records from stdin.
 * Writes scampRec binary call detail records on stdout.
 */
typedef struct
{
  int onpa, obase;
  int tnpa, tbase;
  int dnpa, dbase;
  int con;
  int dur;  
} CscampRec;




int readBinary(FILE *in, CscampRec *r){
  int success =  fread(r, sizeof(*r), 1, stdin);
  if (success == 1)
    fprintf(stderr, "%03d %07d %03d %07d %03d %07d %d %d\n",
	    r->onpa, r->obase, r->dnpa, r->dbase, r->tnpa, r->tbase, 
	    r->con, r->dur); 
  return success;
}

void writeAscii(FILE *out, CscampRec *r){
  fprintf(out, "%03d %07d %03d %07d %03d %07d %d %d\n",
	  r->onpa, r->obase, r->dnpa, r->dbase, r->tnpa, r->tbase, 
	  r->con, r->dur); 
}

int main(){
  CscampRec current;
  while (readBinary(stdin, &current) == 1)
    writeAscii(stdout, &current);
  return 0;
}
