typedef struct Sample {
  int AttrA;
  char * AttrB;
} Sample; /* Sample */
void Sample_PrintAttrA (Sample * this);
typedef void (*Sample_PrintAttrA_t) ();
void Sample_PrintAttrB (Sample * this);
typedef void (*Sample_PrintAttrB_t) ();
void Sample_PrintBoth (Sample * this);
typedef void (*Sample_PrintBoth_t) ();

