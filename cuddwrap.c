#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "cuddwrap.h"

#include "util.h"
#include "cudd.h"

//Function wrappers around macros
DdNode *wrappedRegular(DdNode *f){
    assert(f);
    return Cudd_Regular(f);
}

void wrappedCuddRef(DdNode *f){
	assert(f);
	Cudd_Ref(f);
}

int wrappedCuddIsComplement(DdNode *f){
    return Cudd_IsComplement(f);
}

//Garbage collection hooks
int preGCHook_sample(DdManager *dd, const char *str, void *data){
	printf("Performing %s garbage collection...", str);
	return 1;
}

int postGCHook_sample(DdManager *dd, const char *str, void *data){
	printf("%s GC done\n", str);
	return 1;
}

void wrappedCuddDumpDot(DdManager *m, DdNode *f, char *filename){
	printf("filename: %s\n", filename);
	FILE *file = fopen(filename, "w");
	assert(file);
	Cudd_DumpDot(m, 1, &f, NULL, NULL, file);
	fclose(file);
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

    int **result = malloc(sizeof(int *)*num);
    assert(result);
    Cudd_ForeachCube(m, n, gen, cube, value){
        result[i] = malloc(sizeof(int *)*size);
        assert(result[i]);
        int j;
        for(j=0; j<size; j++){
            result[i][j] = cube[j];
        }
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
    //printf("num minterms: %f %d\n", Cudd_CountPathsToNonZero(n), num);

    gen = Cudd_FirstCube (m, n, &cube, &value);
    //printf("gen= %p\n", gen);
    if (Cudd_IsGenEmpty(gen)) {
        Cudd_GenFree (gen);
        return NULL;
    }
    
    int *result = malloc(sizeof(int) * size);
    assert(result);
    for(j=0; j<size; j++){
        result[j] = cube[j];
    }
    Cudd_GenFree (gen);

    return result;
}

/*
int **allPrimes(DdManager *m, DdNode *l, DdNode *u, int *nterms, int *nvars){
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    int num = ceil(Cudd_CountPathsToNonZero(l));
    int i=0;

    *nterms = num;
    *nvars = size;

    int **result = malloc(sizeof(int *)*num);
    assert(result);
    Cudd_ForeachPrime(m, l, u, gen, cube){
        result[i] = malloc(sizeof(int *)*size);
        assert(result[i]);
        int j;
        for(j=0; j<size; j++){
            result[i][j] = cube[j];
        }
        i++;
    }

    return result;
}
*/

int *onePrime(DdManager *m, DdNode *l, DdNode *u, int *nvars){
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    int j;

    *nvars = size;

    gen = Cudd_FirstPrime(m, l, u, &cube);
    if (Cudd_IsGenEmpty(gen)) {
        Cudd_GenFree (gen);
        return NULL;
    }
    
    int *result = malloc(sizeof(int) * size);
    assert(result);
    for(j=0; j<size; j++){
        result[j] = cube[j];
    }
    Cudd_GenFree (gen);

    return result;
}

