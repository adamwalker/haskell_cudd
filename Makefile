include ${COMMON}/Pathdefs.inc
#CUDD_PATH=../../3rd_party/lib/cudd-2.4.2
INCLUDE=-I${CUDD_PATH}/include

CFLAGS+=${INCLUDE} -g

default: libcuddwrap.a Cudd.hs CuddInternal.hs CuddReorder.hs CuddGC.hs CuddHook.hs MTR.hs CuddST.hs CuddC.hs CuddFile.hs

cuddwrap.o: cuddwrap.c cuddwrap.h
	@echo "[CC] $@"
	@$(CC) $(CFLAGS) -c $< -o $@

stubs.o: stubs.c stubs.h
	@echo "[CC] $@"
	@$(CC) $(CFLAGS) -c $< -o $@

libcuddwrap.a: cuddwrap.o stubs.o
	@echo "[AR] $@"
	@$(AR) rcs libcuddwrap.a cuddwrap.o stubs.o

%.hs: %.hsc
	@echo "[HSC2HS] $@"
	@hsc2hs $< $(INCLUDE)

%.hs: %.chs
	@echo "[C2HS] $@"
	@c2hs --cppopts="$(INCLUDE)" $<
