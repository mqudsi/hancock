#include "../scampRec.hh"

map test_m{
  key ppn_t;
  value int;
  default 0;
}


directory dir_d {
  test_m myMap;
};

int sig_main(char *myMap2Name <m:>, char *myDirName<d:>, new dir_d myDir <D:>){
    new test_m myMap2 = myMap2Name;
        test_m myMap3 = myDir->myMap;
    const test_m myMap4 = myDir->myMap;

    dir_d myNewDir = myDirName; 
    const dir_d myNewDir2 = myDir;

    lpn_t lpn = {973, 360, 8675};

    if (myMap2@<:lpn:>)
     sfprintf(sfstdout, "Number: 973-360-8675 in myMap2.\n");
    else
     sfprintf(sfstdout, "Number: 973-360-8675 not in myMap2.\n");

    if (myMap3@<:lpn:>)
     sfprintf(sfstdout, "Number: 973-360-8675 in myMap3.\n");
    else
     sfprintf(sfstdout, "Number: 973-360-8675 not in myMap3.\n");

    if (myMap4@<:lpn:>)
     sfprintf(sfstdout, "Number: 973-360-8675 in myMap4.\n");
    else
     sfprintf(sfstdout, "Number: 973-360-8675 not in myMap4.\n");

    if (myNewDir->myMap@<:lpn:>)
     sfprintf(sfstdout, "Number: 973-360-8675 in %s->myMap.\n",myDirName);
    else
     sfprintf(sfstdout, "Number: 973-360-8675 not in %s->myMap.\n",myDirName);

    if (myNewDir2->myMap@<:lpn:>)
     sfprintf(sfstdout, "Number: 973-360-8675 in myNewDir2->myMap.\n");
    else
     sfprintf(sfstdout, "Number: 973-360-8675 not in myNewDir2->myMap.\n");

    return 1;
}

