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



int readAscii(FILE *in, CscampRec *r){
  int success =  fscanf(in, "%d %d %d %d %d %d %d %d\n",
		&(r->onpa), &(r->obase), 
                &(r->dnpa), &(r->dbase),
                &(r->tnpa), &(r->tbase), 
		&(r->con),  &(r->dur)); 
  return success;
}

void writeBinary(FILE *out, CscampRec *r){
  fwrite(r, sizeof(CscampRec), 1, out);
}

int main(){
  CscampRec current;
  while (readAscii(stdin, &current) != EOF)
    writeBinary(stdout, &current);
  return 0;
}
