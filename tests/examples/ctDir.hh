#include "wcd.hh"
#include "ctMap.hh"


directory cellTower_d(int level) {
  pht_p	ctHashTable;
  ct_p	ctab(:level:);
  cellTower_m outMap(:ctab:);
  cellTower_m inMap(:ctab:);
  char *lastUpdated default "never";
}
