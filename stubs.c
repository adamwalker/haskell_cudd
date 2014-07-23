#include <stdio.h>
#include <assert.h>
#include "cudd.h"
#include "dddmp.h"

DdNode *Cudd_bddVarMap_s(DdManager *m, DdNode *x){
    DdNode *r = Cudd_bddVarMap(m, x);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_ReadOne_s(DdManager *m){
    DdNode *r = Cudd_ReadOne(m);
    assert(r);
    return r;
}

DdNode *Cudd_ReadLogicZero_s(DdManager *m){
    DdNode *r = Cudd_ReadLogicZero(m);
    assert(r);
    return r;
}

DdNode *Cudd_ReadOne_withRef_s(DdManager *m){
    DdNode *r = Cudd_ReadOne(m);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_ReadLogicZero_withRef_s(DdManager *m){
    DdNode *r = Cudd_ReadLogicZero(m);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddIthVar_s(DdManager *m, int i){
    DdNode *r = Cudd_bddIthVar(m, i);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddNewVarAtLevel_s(DdManager *m, int i){
    DdNode *r = Cudd_bddNewVarAtLevel(m, i);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddAnd_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddAnd(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddOr_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddOr(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddNand_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddNand(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddNor_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddNor(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddXor_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddXor(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddXnor_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddXnor(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_Not_s(DdNode *x){
    DdNode *r = Cudd_Not(x);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_NotNoRef_s(DdNode *x){
    DdNode *r = Cudd_Not(x);
    return r;
}

DdNode *Cudd_bddExistAbstract_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddExistAbstract(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddUnivAbstract_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddUnivAbstract(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddIte_s(DdManager *m, DdNode *x, DdNode *y, DdNode *z){
    DdNode *r = Cudd_bddIte(m, x, y, z);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddSwapVariables_s(DdManager *m, DdNode *x, DdNode **y, DdNode **z, int i){
    DdNode *r = Cudd_bddSwapVariables(m, x, y, z, i);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddPermute_s(DdManager *m, DdNode *x, int *p){
    DdNode *r = Cudd_bddPermute(m, x, p);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_Xgty_s(DdManager *m, int i, DdNode **x, DdNode **y, DdNode **z){
    DdNode *r = Cudd_Xgty(m, i, x, y, z);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_Xeqy_s(DdManager *m, int i, DdNode **x, DdNode **y){
    DdNode *r = Cudd_Xeqy(m, i, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_Inequality_s(DdManager *m, int i, int j, DdNode **x, DdNode **y){
    DdNode *r = Cudd_Inequality(m, i, j, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}
    
DdNode *Cudd_Disequality_s(DdManager *m, int i, int j, DdNode **x, DdNode **y){
    DdNode *r = Cudd_Disequality(m, i, j, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddInterval_s(DdManager *m, int i, DdNode **x, int j, int k){
    DdNode *r = Cudd_bddInterval(m, i, x, j, k);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_IndicesToCube_s(DdManager *m, int *i, int j){
    DdNode *r = Cudd_IndicesToCube(m, i, j);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddLICompaction_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddLICompaction(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddMinimize_s(DdManager *m, DdNode *x, DdNode *y){
    DdNode *r = Cudd_bddMinimize(m, x, y);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Dddmp_cuddBddLoad_s(DdManager *m, int i, char **s, int *j, int *k, int l, char *t, FILE *f){
    DdNode *r = Dddmp_cuddBddLoad(m, i, s, j, k, l, t, f);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddPickOneMinterm_s(DdManager *m, DdNode *x, DdNode **y, int i){
    DdNode *r = Cudd_bddPickOneMinterm(m, x, y, i);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddAndAbstract_s(DdManager *m, DdNode *x, DdNode *y, DdNode *z){
    DdNode *r = Cudd_bddAndAbstract(m, x, y, z);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddXorExistAbstract_s(DdManager *m, DdNode *x, DdNode *y, DdNode *z){
    DdNode *r = Cudd_bddXorExistAbstract(m, x, y, z);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddMakePrime_s(DdManager *m, DdNode *c, DdNode *f){
    DdNode *r = Cudd_bddMakePrime(m, c, f);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddRestrict_s(DdManager *m, DdNode *f, DdNode *c){
    DdNode *r = Cudd_bddRestrict(m, f, c);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddSqueeze_s(DdManager *m, DdNode *l, DdNode *u){
    DdNode *r = Cudd_bddSqueeze(m, l, u);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddConstrain_s(DdManager *m, DdNode *f, DdNode *c){
    DdNode *r = Cudd_bddConstrain(m, f, c);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_LargestCube_s(DdManager *m, DdNode *f, int *length){
    DdNode *r = Cudd_LargestCube(m, f, length);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddComputeCube_s(DdManager *m, DdNode **vars, int *phase, int n){
    DdNode *r = Cudd_bddComputeCube(m, vars, phase, n);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddCompose_s(DdManager *m, DdNode *f, DdNode *g, int v){
    DdNode *r = Cudd_bddCompose(m, f, g, v);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_Support_s(DdManager *m, DdNode *f) {
    DdNode *r = Cudd_Support(m, f);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddNewVar_s(DdManager *m){
    DdNode *r = Cudd_bddNewVar(m);
    assert(r);
    Cudd_Ref(r);
    return r;
}

DdNode *Cudd_bddVectorCompose_s(DdManager *m, DdNode *x, DdNode **xs){
    DdNode *r = Cudd_bddVectorCompose(m, x, xs);
    assert(r);
    Cudd_Ref(r);
    return r;
}

