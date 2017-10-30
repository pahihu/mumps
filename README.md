MUMPS V1R2
==========

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.

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

**NOTE**: Use the code! Good luck!
