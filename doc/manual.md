# A Terse Guide to MV1R2DEV

Copyright 2018, 2019 Andras Pahi. All rights reserved.

[Foreword](#foreword)

[Features](#features)

[Commands](#commands)

[Functions](#functions)

[Variables](#variables)

[Structured System Variables](#ssvns)

[Limits](#limits)



## Foreword

MV1 and its derivative MV1R2DEV is small and efficient like a Swiss 
army knife. I do not repeat the MUMPS V1 manual of Ray Newman which 
can be found at `doc/MUMPS.epub`. Beware it is not a pure 
MUMPS-1995 implementation. If you want a MUMPS which does not contain 
implementation specific commands and functions use MV1. In this text
MUMPS refers to the MV1R2DEV implementation of the language which is
based on MUMPS V1.



## Features

### Adaptive daemon rest time

By default MUMPS starts up a write daemon per 10 jobs up to a maximum of
10 write daemons. The MUMPS engine and the daemons are connected by two
queues: the dirty queue and the garbage queue respectively. Each daemon
checks the queues for work to be done. By default each daemon polls the
queues every 1000ms. When you make multiple pages dirty in the global cache
and the queue gets full you need to wait for the next poll time. Here
helps the adaptive rest time, which checks the statistics counters in the 
MUMPS environment and when there are processes waiting for free queue slots
it reduces the poll time. When the load vanishes it increases the poll time
again. To query the current rest time between write daemon polls, query
the *RESTTIME* system parameter.


### Buffered files

In MUMPS files are buffered: the `READ` and `WRITE` commands buffers
their input/output respectively.


### Compressing globals

The *ZMINSPACE* system parameter controls how much space should remain free
in each database block when compressing globals in bytes.
By default there should be 1024 bytes of free space in each 
block. The minimum value is 128 bytes, the maximum is 90%
of the database block size of the first volume.

    SET ^$SYSTEM("ZMINSPACE")=1024 ;Leave 1024 bytes of free space


### Device terminator characters

In a `USE` command you can specify the input terminators with the
`TERMINATOR=$C(n,...)` parameter. The terminators are **not** restricted
to the control characters, any character can be specified.


### Journal buffer

Journaling works using a journal buffer, which gets flushed when full
or at least 1 seconds time is elapsed since the last journal entry.

Journal flushing is performed either synchronously or asynchronously.

When you use synchronous journal buffer flush, the journal file is
fsync()-ed to disk on every buffer flush. When you use asynchronous
journal buffer flush, the journal file is fsync()-ed only by a write
daemon once in every 1 seconds.

The size of the journal buffer can be specified on the command line
when you start up the MUMPS environment. The `-l` switch specifies the
size of the journal buffer in kilobytes. When you specify a positive
number the journal flush will be performed synchronously. When you
specify a negative number the flush will be asynchronous.

Example:

    # 1024KB journal buffer, synchronous buffer flush
    mumps -l 1024 testdb

    # 512KB journal buffer, asynchronous journal flush
    mumps -l -512 testdb


### Journal file change

You do not need to stop the MUMPS environment to change the journal 
file. While the environment is on-line, you can simply change the
journal file.

Example:

    SET ^$SYSTEM("VOL",1,"JOURNAL_FILE")="/path/to/newjrn"


### KILLing data

When you `KILL` globals in the database, MUMPS does not overwrite
the free blocks with zeroes. If you need to ensure that the
previous data is not recoverable from the database blocks, set
the system parameter *ZOTDATA* to 1. By default *ZOTDATA* is
0 which means MUMPS does not zeroes free blocks.

    SET ^$SYSTEM("ZOTDATA")=0 ;Do not zero free blocks
    SET ^$SYSTEM("ZOTDATA")=1 ;Zero every free block


### Translation table

You could map globals with these feature from one volume set/uci to
another. This is not new, just the implementation is changed. You could
have up to 256 translations in the table.


### Volume syncing

By default every 300 seconds the contents of each volume is fsync()-ed
to disk. There is a per volume parameter *GLOBAL_BUFFER_SYNC* which
controls the syncing. It contains the number of seconds between volume
syncs. Set to 0 to turn off volume syncing.

    SET ^$SYSTEM("VOL",1,"GLOBAL_BUFFER_SYNC")=180 ;Sync every 3 minutes
    SET ^$SYSTEM("VOL",1,"GLOBAL_BUFFER_SYNC")=0 ;Turn off vol sync


### Maximum volume size

The map block size defines the maximum possible volume size for a single
volume. It's minimum size is the block size of the volume (ie. the volume
file can grow to `8 * block size` blocks). The map block is allocated 
in 4K chunks when the volume is created, with the maximum size of 4MB 
(ie. the maximum volume file size is about 32M blocks). That means you
can have a maximum volume file size of about 128GB using 4KB blocks.


### Multiple volume sets

You can mount up to 16 volume sets in a multi-user MUMPS environment.
The volumes should be mounted in sequence, and cannot be unmounted
individually. Global buffers and journaling is per volume sets.
When you start up the MUMPS environment you should specify the initial
volume set which gets mounted, with additional shared memory space for
the global and journal buffer space of the additional volume sets.

The following command starts the MUMPS environment for 40 jobs with
20MB of global buffer space, 1024KB of journal buffer space with
asynchronous buffer flush for the initial volume set *testdb* and
32MB of additional space for volume mounting.

    mumps -j 40 -g 20 -l -1024 -a 32 testdb

To mount a new volume set, you can simply set the *FILE* parameter for
the next volume to the full path of the database file:

    SET ^$SYSTEM("VOL",2,"FILE")="/home/user/MUMPS/db/user.dat"

You can specify the global buffer size in megabytes and the journal
buffer size for the volume to be mounted before setting the *FILE*
parameter:

    SET ^$SYSTEM("VOL",2,"GLOBAL_BUFFER_SIZE")=8 ;8MB global buffer
    SET ^$SYSTEM("VOL",2,"JOURNAL_BUFFER_SIZE")=-256 ;256KB jnl buffer, async flush
    SET ^$SYSTEM("VOL",2,"FILE")="/home/user/MUMPS/db/app.dat"


### Remote volume sets

DGP (Distributed Global Protocol) is not compatible with any MUMPS networking
standards. It uses the [nanomsg library](http://nanomsg.org) and you should
compile the sources with the following additions in the Makefile:

    EXTRA += -DMV1_DGP=1
    LIBS  += -lnanomsg

The server MUMPS system starts network daemons. The maximum number of network
daemons is 10. The connection interface is specified as a transport protocol
base URL in the nanomsg library format and a base port number. Each network
daemon constructs its connection point from the URL and the base port number.

Example:

    mumps -i 1 -n 5 -u tcp://192.168.1.23 -p 2000 datadb

The command above will start the MUMPS environment which ID is 1, with 5 
network daemons, with the following connection point URLs:

    tcp://192.168.1.23:2000
    tcp://192.168.1.23:2001
    tcp://192.168.1.23:2002
    tcp://192.168.1.23:2003
    tcp://192.168.1.23:2004

The client MUMPS systems can mount a volume from a server MUMPS system,
with the following commands:

    SET ^$SYSTEM("VOL",2,"LOCAL_NAME")="AAA"
    SET ^$SYSTEM("VOL",2,"FILE")="tcp://192.168.1.23:2002/BBB"

This commands specify that the local volume "AAA" stands for the remote
volume "BBB" on the remote server. The client will connect using TCP/IP
protocol to the IP address 192.168.1.23 on port 2002. The remote volume 
"BBB" should be local on the server (ie. cannot be a remote volume set).

Using the volume "AAA" on the client system will transfer each database
command to the remote system and the results are coming from there.

Each MUMPS environment participating in a remote environment (either as a 
server or client) should have a unique system ID, which should be specified
on the command line, when you start the environment. The unique system ID
is necessary to support remote locking, because each JOB will get a unique
job number which is constructed from the system ID and the local JOB number.

The maximum number of connected client and server MUMPS environments is 254.

Timeouts specified on remote LOCKs are not used. Instead the system
parameter `DGP_LOCK_TIMEOUT` is used on the server system to specify a
timeout for the LOCK commands. First the locks are established on the local
system. If they succeed locks are placed on the remote system.

On normal job termination or client MUMPS environment shutdown the remote 
LOCKs are removed for the corresponding job or the client system as a whole.

When a client MUMPS environment is restarted, it removes all remaining
remote LOCKs from the server environments.

When a server MUMPS environment is restarted, the client environments get
notified and the client environments remove all local LOCKs related to
the server MUMPS environment and the corresponding local MUMPS jobs get 
lost remote LOCKs errors.


### MUMPS environment replicas

This requires the nanomsg library as for the remote volume sets.
You could set upto 16 environment replicas. The journaling phase sends remote messages
on SET and KILL commands to the specified replicas. If the replica is mandatory then
any error got from the replica will stop the operation. If the replica is optional
the replication errors are ignored.

To set replicas issue the following commands:

	SET ^$S("REPLICA",1,"TYPE")="MANDATORY"
	SET ^$S("REPLICA",1,"CONNECTION")="tcp://192.168.1.23:2000"
	SET ^$S("REPLICA",2,"TYPE")="OPTIONAL"
	SET ^$S("REPLICA",2,"CONNECTION")="tcp://192.168.1.23:3000"


### MV1API

You can connect to a MUMPS environment with the use of the [MV1 connect API](https://github.com/pahihu/mumps/blob/development/mv1api/mv1api.h) using the
C language binding. It is somewhat similar to the DSM and MSM C APIs. The MUMPS
environment code is not thread-safe, thus do not use MV1 connect API from 
multiple threads. The code is not tested against a multi-volume set environment yet.


## User defined commands and functions

You could extend the command and function set of MUMPS with
user defined commands and functions. The only restriction is
that you should begin your commands and functions with the *ZZ*
prefix. The bytecode compiler will translate each such command
and function to a call to the corresponding tag in the routine
*ZZCMD* or *ZZFN* respectively.

---

`ZZcmd a1:a2...`

Calls `ZZcmd^ZZCMD(a1,a2...)`. Note the colon in the syntax to
enter multiple parameters for the command.

---

`$ZZfn(a1,a2...)`

Calls `ZZfn^ZZFN(a1,a2...)`.


## Commands

`H[ANG] expr`

Supports fractional seconds.

---

### KILL commands

`KV[ALUE] glvn,...`

`KV[ALUE] (lvn,...)`

`KS[UBSCRIPTS] glvn,...`

`KS[UBSCRIPTS] (lvn)`

`KVALUE` kills only the value of a global node, but leaves intact
the descendants. `KSUBSCRIPTS` kills only the descendants of a
global node, but leaves the node intact.

---

## Functions

With these extensions you can spare an additional global read to get the
value of a global node. It used to be insignificant, for my problems it
is not.

`$D[ATA](glvn[,target])`

Works like `$D[ATA]`, except when a *target* is given it is
set to the value of *glvn*.

---

`$O[RDER](glvn[,dir[,target]])`

Works like `$O[RDER]`, but it sets *target* to the value of
*glvn* if given.

---

`$Q[UERY](glvn[,dir[,target]])`

Works like `$Q[UERY]`, but it sets *target* to the value of
*glvn* if given.

---

`$ZINCR[EMENT](glvn[,expr])`

Atomically increments *glvn* by 1, or with the value of
*expr*.

---

`$ZSEND(obj,"tag"[,arg1,arg2...])`

Calls dynamically `tag^class(obj,arg1,arg2...)`.
*obj* should be in the form *value[@class]*. Note the class part
is optional. When it is empty it will use `%Object` as routine
name. For more information see README.OOP.

---

### List functions

Lists solve the problem of embedding lists inside lists. The canonical
storage of multiple variable length fields in a single global is to use
a separator character. When you want to embed a list in another list you
should use another separator and so on.

`$LIST(lst[,from[,to]])`

`$LI(lst[,from[,to]])`

Returns list element in the range [*from*,*to*]. *From* defaults to 1
and *to* defaults to *from*. If *from* is -1 returns the last element of
list *lst*. If both *from* and *to* is given it returns a list.
If the element at position *from* is empty, it generates an error.

---

`$LISTBUILD([elt[,elt...]])`

`$LB([elt[,elt...]])`

Builds a list from *elt* elements. *Elt* can be empty.
You can concatenate lists to get a new list.

---

`$LISTDATA(lst[,pos])`

`$LD(lst[,pos])`

Returns 1 if the element is present in *lst* at position *pos*.
*Pos* defaults to 1.

---

`$LISTFIND(lst,val[,after])`

`$LF(lst,val[,after])`

Returns the position of *val* in list *lst*. If *val* is not found
it returns 0. Search begins after the position *after*. *After*
defaults to 0.

---

`$LISTGET(lst[,pos[,def]])`

`$LG(lst[,pos[,def]])`

Returns element at *pos* in list *lst*. *Pos* defaults to 1. If list
element is undefined at *pos* it returns the empty string or *def*
if given.

---

`$LISTLENGTH(lst)`               

`$LL(lst)`

Returns the number of elements in list *lst*.

---

`SET $LIST(lst[,from[,to]])=expr` 

`SET $LI(lst[,from[,to]])=expr`

It is similar to `SET $PIECE(...)`. Set the list *lst* to the value
given. *From* defaults to 1, *to* defaults to -1. If the 2 argument form
is used it replaces the single element at *from* with the value of
*expr*. If the value of *expr* is a list, it is stored as a single
element. If the 3 argument form is used, it replaces the sublist from
position *from* to position *to* with the value of *expr*. If the
value of *expr* is a list, it replaces the sublist [*from*,*to*].

---

### Bit string functions

A bit string contains an arbitrary number of bits. For the bitwise 
operators the bit string arguments are not required to be equal in length. 
The *missing* tail parts are assumed to be zero.


`$ZBITAND(bstr1,bstr2)`

`$ZBA(bstr1,bstr2)`

Returns the bitwise logical AND of the bit string arguments. The length
of the result is the length of the shorter argument.

---

`$ZBITCOUNT(bstr)`

`$ZBC(bstr)`

Returns the number of 1s in the bit string.

---

`$ZBITFIND(bstr,flag[,pos])`

`$ZBF(bstr,flag[,pos])`

Returns the position of the first bit beginning at *pos* in the bit
string. If *flag* is zero it searches for the first 0, if not zero
it searches for the first 1. If *pos* is greater than the length of
the bit string 0 is returned. If the *flag* indicated bit is not found
then 0 is returned.

---

`$ZBITGET(bstr,pos)`

`$ZBG(bstr,pos)`

Returns the bit at position *pos* in the bit string. Position is counted
from 1. If *pos* is greater than the length of the bit string 0 is returned.

---

`$ZBITLEN(bstr)`

`$ZBL(bstr)`

Returns the length of the bit string, ie. the number of bits.

---

`$ZBITNOT(bstr)`

`$ZBN(bstr)`

Returns the bitwise logical NOT of the bit string.

---

`$ZBITOR(bstr1,bstr2)`

`$ZBO(bstr1,bstr2)`

Returns the bitwise logical OR of the bit string arguments. The length of
the result is the length of the longer argument.

---

`$ZBITSET(bstr,pos,flag)`

`$ZBS(bstr,pos,flag)`

Sets the bit at *pos* in the bit string according to the *flag* specified.
If *flag* is not zero, the bit is set, if zero then it is cleared. If *pos*
is greater than the length of the string the bit string is extended. It
returns the modified bit string.

---

`$ZBITSTR(len[,flag])`

Returns a bit string of length *len*. If *flag* is not zero, the bit string
is initialized with 1s, otherwise with 0s. The default value of *flag* is
zero.

---

`$ZBITXOR(bstr1,bstr2)`

`$ZBX(bstr1,bstr2)`

Returns the bitwise logical XOR of the bit string arguments. The length of
the result is the length of the longer argument.

---


## Variables

`$S[TORAGE]`

Returns the number of free slots in the local symbol table.

---

`$ZH[OROLOG]`

Similar to `$H[OROLOG]`, but returns fractional seconds with
microsecond resolution.


## SSVNs

Additional `^$SYSTEM` variables or changed behavior.

| Subscript          | Contains                  | Setable |
| ------------------ | ------------------------- | ------- |
| BACKUP_FILE        | Backup file name          | set with priv |
| BACKUP_TYPE        | Type of backup FULL, CUMULATIVE or SERIAL | set with priv |
| BACKUP_VOLMASK     | Backup VOL masks          | set with priv |
| DGP_ID             | Network ID of the MUMPS environment  | no |
| DGP_LOCK_TIMEOUT   | LOCK timeout for network locks       | set with priv |
| DGP_PORT           | Base port number for network daemons | no |
| DGP_URL            | Transport URL for network daemons    | no |
| DQLEN              | Dirty queue length        | no |
| REPLICA,n,CONNECTION | Replica connection URL   | set with priv |
| REPLICA,n,TYPE       | Replica type MANDATORY or OPTIONAL | set with priv |
| RESTTIME           | Daemon rest time          | no |
| TSIZE              | sizeof(time_t)            | no |
| ZMINSPACE          | Min. free space in blocks | set with priv |
| ZOTDATA   	     | Zero free blocks          | set with priv |
| VOL,n,FILE         | file for volset n         | set with priv  (to mount volset) |
| VOL,n,BACKUP_RUNNING      | Volume backup is running | set with priv |
| VOL,n,BLOCKS_CHANGED	    | Number of blocks changed | set with priv |
| VOL,n,GLOBAL_BUFFER_SIZE  | Global buffer (in MB)    | set with priv |
| VOL,n,GLOBAL_BUFFER_SYNC  | Volume sync in seconds   | set with priv |
| VOL,n,JOURNAL_BUFFER_SIZE | Journal buffer (in KB)   | set with priv |
| VOL,n,JOURNAL_FILE        | Journal file (incl path) | set with priv |
| VOL,n,TRACK_CHANGES	    | Track block changes in VOL | set with priv |
| VOL,n,param               | Usage parameters for vol set | no |

| Parameters | Contains |
| ---------- | ------------------- |
| dirty      | No. of dirty blocks |
| dqstall    | Dirty Queue Stalled |
| gbswait    | No. of waits before global write |
| gbwait     | No. of waits before global read |
| gqstall    | Garbage Queue Stalled |
| lastwtok   | Last Write Success |
| lastwttry  | Last Write Tries |
| lckwait    | No. of waits before `LOCK` |
| rdwait     | No. of waits because of block read |


## More information

The additional MUMPS commands and functions are inspired by several
MUMPS implementations. If you want to know more please consult the
respective descriptions below:

- `KVALUE`, `KSUBSCRIPT` see "The Annotated M[UMPS] Standards" by Ed de Moel
- `$DATA()`,`$ORDER()`,`$QUERY()` see current Cache documentation
- `$LIST` functions see documentation of MiniM or Cache 2.1
- `$ZBIT` functions see documentation of GT.M or MiniM or Cache 2.1
- `$ZSEND()` is similar to $ZMETHOD() of current Cache, but the inner
workings are entirely different. See README.OOP
- `$ZINCREMENT` function see documentation of GT.M
- `ZZ` commands and functions are similar to DTM, you can extend
MUMPS with commands/functions written in MUMPS
- `^%G` global lister, see the documentation in `doc/GlobalExtLister.pdf`

## Limits

| Feature                           | Value                    |
| --------------------------------- | ------------------------ |
| Language			    | M-1995 with extensions   |
| Maximum identifier length	    | 31 chars		       |
| Maximum routine object size	    | min(block size, 32KB)    |
| Maximum local variable length	    | 32KB		       |
| Maximum number of volumes	    | 16		       |
| Volume block size		    | 4KB - 256KB	       |
| Volume size            	    | 32M blocks (128GB - 8TB) |
| Namespaces per volume (UCI)	    | 63		       |
| Maximum global key size	    | 255		       |
| Maximum global value size	    | min(block size, 32KB)    |
| Number of global subscripts	    | 63                       |
| Global translation table size	    | 255		       |
| Maximum number of jobs            | 256                      |
| Maximum number of daemons	    | 10		       |
| Maximum JOB command length        | 32KB                     |
| Maximum number of network daemons | 10                       |
| Maximum number of connected environments | 254               |
| Maximum number of environment replicas | 16                  |

