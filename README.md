

MUMPS V1R2
==========

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.
The master branch gets only bug fixes.

If you are looking for speed, additional MUMPS commands and functions, 
object-oriented syntactic sugar, multiple volume sets, remote volume
sets, online backup use the [**stable**](https://github.com/pahihu/mumps/tree/stable) branch.

Changes:

  * works on big-endian processors
  * works on macOS with clang
  * has 31 character label/routine/local/global names
  * MCL shell prompt changed to [UCI,VOL], SHUTDOWN command
  * %MV1 routine contains the implementation constants
  * supports global buffer area over 2GB
  * daemon log contains GMT timestamp, process id, restart info
  * blkalloc/blkdeall/blkreorg/dqstall statistics

Fixes:
  * fixed "Too many tags" error in localvar()
  * display all compilation messages ("Too many tags" got garbaged)
  * parser/runtime fix for LOCK (@GLVN1[,...])
  * stack overflow during ST_Restore()
  * XECUTE could overwrite process stack
  * DB lock-up with a small global buffer
  * faster Locate()
  * db_rekey fixes (level > 3)
  * prevent changed Garbit() blocks to leave the global buffer
  * fixes from MUMPS V1 1.66 and 1.70
  * recursive indirection
  * $BP handling
  * $FNUMBER() memory leaks
  * EINTR handling during semaphore operations
  * $JUSTIFY() with negative numbers
  * external variable call memory leaks
  * standard handle redirection in JOB
  * socket communication options (SO_REUSEADDR, SO_NOSIGPIPE)
  * robust error handling in MUMPS environment initialization
  * no writer deadlock due to full dirty queue
  * removed race in global KILL (block removal)
  * atomic access in dirty/garbage queue
  * reverse $QUERY() for locals
  * $FNUMBER() suppress sign for 0
  * JOB argument passing
  * fixed cached block usage in Get_data() when journaling turned off and writing
  * fixed Control-C handling
  * close database and journal file on dismount

**NOTE**: Don't forget you can use [RSM](https://gitlab.com/Reference-Standard-M/rsm). Good luck!
