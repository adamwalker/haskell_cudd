#ifndef _CUDDWRAP_H
#define _CUDDWRAP_H

#include "util.h"
#include "cudd.h"

void wrappedCuddRef(DdNode *f);
void wrappedCuddDumpDot(DdManager *m, DdNode *f, char *filename);

#endif
