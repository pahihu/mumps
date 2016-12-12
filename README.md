MUMPS V1R2
==========

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.
Work in progress.

Changes:

  * works on big-endian processors
  * works on macOS with clang
  * 31 character label/routine/local/global names
  * numbers in keys stored as packed decimals
  * 3 argument $ORDER(), $QUERY() and 2 argument $DATA() functions
  * KVALUE and KSUBSCRIPTS commands
  * $LIST functions
  * $ZBIT functions
  * $ZINCREMENT() function
  * $ZSEND() function and method chaining with dot operator (see README.OOP)
  * HANG supports fractional seconds
  * $ZHOROLOG special variable, returns fractional seconds
  * ZZ commands and functions (calls tags in ZZCMD and ZZFN)

**NOTE**: There are excellent M implementations like Intersystems Cache, FIS GT.M or MiniM. They have excellent support. Otherwise here is the code. Good luck!
