#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "cuddwrap.h"

#include "util.h"
#include "cudd.h"

void wrappedCuddRef(DdNode *f){
	assert(f);
	Cudd_Ref(f);
}

void wrappedCuddDumpDot(DdManager *m, DdNode *f, char *filename){
	printf("filename: %s\n", filename);
	FILE *file = fopen(filename, "w");
	assert(file);
	Cudd_DumpDot(m, 1, &f, NULL, NULL, file);
	//Cudd_ReadOne(m);
	//Cudd_bddIthVar(m, 5);
	//Cudd_RecursiveDeref(m, f);
	fclose(file);
}

void deref(DdManager *m, DdNode *d){
	printf("deref\n");
}

DdNode *wrappedCuddNot(DdNode *f){
    return Cudd_Not(f);
}

int wrappedCuddIsComplement(DdNode *f){
    return Cudd_IsComplement(f);
}

int **allSat(DdManager *m, DdNode *n, int *nterms, int *nvars){
    CUDD_VALUE_TYPE value;
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    int num = ceil(Cudd_CountPathsToNonZero(n));
    int i=0;

    *nterms = num;
    *nvars = size;
    printf("Support: %d\n", size);
    printf("num minterms: %f %d\n", Cudd_CountPathsToNonZero(n), num);

    int **result = malloc(sizeof(int *)*num);
    assert(result);
    Cudd_ForeachCube(m, n, gen, cube, value){
        result[i] = malloc(sizeof(int *)*size);
        assert(result[i]);
        int j;
        for(j=0; j<size; j++){
            printf("%d ", cube[j]);
            result[i][j] = cube[j];
        }
        printf("\n");
        i++;
    }

    return result;
}

int *oneSat(DdManager *m, DdNode *n, /*int *nterms,*/ int *nvars){
    CUDD_VALUE_TYPE value;
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    //int num = ceil(Cudd_CountPathsToNonZero(n));
    int j;

    //*nterms = num;
    *nvars = size;
    printf("Support: %d\n", size);
    //printf("num minterms: %f %d\n", Cudd_CountPathsToNonZero(n), num);

    gen = Cudd_FirstCube (m, n, &cube, &value);
    //printf("gen= %p\n", gen);
    if (Cudd_IsGenEmpty(gen)) {
	printf("empty\n");
	Cudd_GenFree (gen);
	return NULL;
    }
    
    int *result = malloc(sizeof(int) * size);
    assert(result);
    for(j=0; j<size; j++){
	printf("%d ", cube[j]);
	result[j] = cube[j];
    }
    printf("\n");
    Cudd_GenFree (gen);

    return result;
}


int *testnew(){
    int *i = malloc(sizeof(int));
    *i = 0;
    return i;
}

int testnext(int *i){
    printf("called\n");
    (*i)++;
    return *i;
}

