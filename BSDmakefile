# Makefile for MUMPS BSD
# Copyright (c) Raymond Douglas Newman, 1999 - 2014
# with help from Sam Habiel

CC	=	gcc
LIBS	  = -lm -lcrypt
EXTRA     = -O -Wall -Iinclude

.ifmake test
EXTRA     = -O0 -g -Wall -Iinclude
.endif

SUBDIRS=compile database init runtime seqio symbol util xcall

RM=rm -f

PROG      = mumps

OBJS	= 	compile/dollar.o \
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
		database/db_main.o \
		database/db_rekey.o \
		database/db_set.o \
		database/db_uci.o \
		database/db_util.o \
		database/db_view.o \
		init/init_create.o \
		init/init_run.o \
		init/init_start.o \
		init/mumps.o \
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
		util/util_share.o \
		util/util_strerror.o \
		xcall/xcall.o

.c.o:
	${CC} ${EXTRA} -c $< -o $@

all: ${OBJS}
	${CC} ${EXTRA} -o ${PROG} ${OBJS} ${LIBS}

test: ${OBJS}
	${CC} ${EXTRA} -o ${PROG} ${OBJS} ${LIBS}

clean:
	rm -f ${OBJS} ${PROG} ${PROG}.core
