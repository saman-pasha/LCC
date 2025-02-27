#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "method.h"
void Sample_PrintAttrA (Sample * this) {
  printf("AttrA: %d", (this.AttrA));
}
void Sample_PrintAttrB (Sample * this) {
  printf("AttrB: %s", (this.AttrB));
}
void Sample_PrintBoth (Sample * this) {
  _PrintAttrA(this);
  _PrintAttrB(this);
}
int main () {
  { /* SCOPE177 */
    Sample s = {100, "domain.com"};
    _PrintBoth(s);
    return 0;
  } /* SCOPE177 */
}

