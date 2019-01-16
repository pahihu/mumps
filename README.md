MUMPS V1R2DEV
=============

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.
Work in progress. For more information see the [manual](https://github.com/pahihu/mumps/blob/development/doc/manual.md).

Changes:

  * works on big-endian processors
  * works on macOS with clang
  * 31 character label/routine/local/global names
  * numbers in keys stored as packed decimals
  * multiple volume sets
  * remote volume sets
  * HANG supports fractional seconds
  * 3 argument $ORDER(), $QUERY() and 2 argument $DATA() functions
  * KVALUE and KSUBSCRIPTS commands
  * $LIST functions
  * $ZBIT functions
  * $ZINCREMENT() function
  * $ZSEND() function and method chaining with dot operator (see README.OOP)
  * $ZHOROLOG special variable, returns fractional seconds
  * ZZ commands and functions (calls tags in ZZCMD and ZZFN)

**NOTE**: There are excellent M implementations like Intersystems Cache, FIS GT.M, YottaDB or MiniM. They have very good support. Otherwise here is the code. Good luck!
