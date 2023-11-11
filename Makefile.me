# Makefile for MUMPS for OSX, Solaris and linux (uses gnumake)
# see BSDmakefile for the various BSDs
# Copyright (c) Raymond Douglas Newman, 1999 - 2016
# with help from Sam Habiel

OSTYPE	= darwin
CC	= gcc -g
LIBS	= -L$(HOME)/lib -lnanomsg -lmapm

EXTRA   = -O -fsigned-char -Wall -Iinclude -Imv1api -D_FILE_OFFSET_BITS=64 \
		-DMV1_DGP=1 \
      -I$(HOME)/include
# 		-DMV1_DLMALLOC=1
#     -DMV1_DEVSYS=1

#     -DMV1_OSSEM=1
#     -DMV1_WRITER=1
#	   -DMV1_CKIT=1 \
#     -DNDEBUG=1 \
# 		-DMV1_MAPM=1 \
#		-DMV1_DLMALLOC=1

ifeq ($(MAKECMDGOALS),test)
EXTRA   = -O0 -g -fsigned-char -Wall -Iinclude -Imv1api -D_FILE_OFFSET_BITS=64
endif

ifeq ($(findstring solaris,$(OSTYPE)),solaris)
LIBS    = -lcrypt -lnsl -lsocket -lm
endif

ifeq ($(OSTYPE),linux)
LIBS   += -lcrypt -lrt
endif


ifeq ($(OSTYPE),darwin)
LIBS    += -lm -framework CoreServices -framework DirectoryService -framework Security
EXTRA += -Wno-deprecated-declarations

endif

SUBDIRS=compile database init mv1api runtime seqio symbol util xcall

RM=rm -f

PROG    = mv2

COMOBJS	= 	compile/dollar.o \
		compile/eval.o \
		compile/localvar.o \
		compile/parse.o \
		compile/routine.o \
 		database/db_buffer.o \
		database/db_daemon.o \
		database/db_get.o \
		database/db_ic.o \
		database/db_kill.o \
		database/db_locate.o \
		database/db_locbuf.o \
		database/db_main.o \
		database/db_mount.o \
		database/db_rekey.o \
		database/db_set.o \
		database/db_uci.o \
		database/db_util.o \
		database/db_view.o \
		database/dgp.o \
		database/dgp_main.o \
		init/init_create.o \
		init/init_run.o \
		init/init_start.o \
		runtime/runtime_attn.o \
		runtime/runtime_buildmvar.o \
		runtime/runtime_debug.o \
		runtime/runtime_func.o \
		runtime/runtime_math.o \
		runtime/runtime_pattern.o \
		runtime/runtime_run.o \
		runtime/runtime_ssvn.o \
		runtime/runtime_util.o \
		runtime/runtime_vars.o \
		runtime/dlmalloc.o \
		runtime/decnumber.o \
		seqio/SQ_Util.o \
		seqio/SQ_Signal.o \
		seqio/SQ_Device.o \
		seqio/SQ_File.o \
		seqio/SQ_Pipe.o \
		seqio/SQ_Seqio.o \
		seqio/SQ_Socket.o \
		seqio/SQ_Tcpip.o \
		symbol/symbol_new.o \
		symbol/symbol_util.o \
		util/util_key.o \
		util/util_lock.o \
		util/util_memory.o \
		util/util_routine.o \
      util/util_rwlock.o \
		util/util_share.o \
		util/util_strerror.o \
      util/util_usync.o \
		util/util_bswap.o \
		util/util_rle.o \
		xcall/xcall.o


LIBOBJS =       mv1api/mv1api_connect.o \
		mv1api/mv1api_glb.o \
		mv1api/mv1api_var.o

OBJS =          ${COMOBJS} \
		init/mumps.o

.c.o:
	${CC} ${EXTRA} -c $< -o $@

all: ${PROG} api_test

${PROG}: ${OBJS}
	${CC} ${EXTRA} -o ${PROG} ${OBJS} ${LIBS}

libmv1api.a: ${COMOBJS} ${LIBOBJS}
	${AR} cru $@ ${COMOBJS} ${LIBOBJS}

test: ${OBJS}
	${CC} ${EXTRA} -o ${PROG} ${OBJS} ${LIBS}

api_test: api_test.o libmv1api.a
	${CC} ${EXTRA} -o $@ api_test.o libmv1api.a ${LIBS}

clean:
	rm -f ${OBJS} ${PROG} ${PROG}.core
	rm -f ${LIBOBJS} libmv1api.a api_test.o api_test

# vim:ts=3:sw=3:et:
