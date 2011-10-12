CUDD_PATH=../../3rd_party/lib/cudd-2.4.2
INCLUDE=-I${CUDD_PATH}/include

CFLAGS+=${INCLUDE} -g

default: libcuddwrap.a Cudd.hs CuddInternal.hs CuddReorder.hs CuddGC.hs CuddHook.hs

cuddwrap.o: cuddwrap.c cuddwrap.h
	@echo "[CC] $@"
	@$(CC) $(CFLAGS) -c $< -o $@

libcuddwrap.a: cuddwrap.o
	@echo "[AR] $@"
	@$(AR) rcs libcuddwrap.a cuddwrap.o

%.hs: %.hsc
	hsc2hs $< $(INCLUDE)

%.hs: %.chs
	c2hs --cppopts="$(INCLUDE)" $<
