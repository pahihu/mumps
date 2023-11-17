MUMPS V2
========

**NOTE**: This is the **stable** branch. It contains fixes and changes considered stable from the [**development**](https://github.com/pahihu/mumps/tree/development) branch.

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.
Work in progress. For more information see the [manual](https://github.com/pahihu/mumps/blob/stable/doc/manual.md).

Changes:

  * works on big-endian processors
  * works on macOS with clang
  * 31 character label/routine/local/global names
  * numbers in keys stored as packed decimals
  * multiple volume sets
  * remote volume sets, local execution of remote routines
  * online backup and restore
  * global translation, replication
  * HANG supports fractional seconds
  * 3 argument $ORDER(), $QUERY() and 2 argument $DATA() functions
  * KVALUE and KSUBSCRIPTS commands
  * $LIST functions
  * $ZBIT functions, bit strings in RLE format
  * $ZINCREMENT() function
  * $ZSEND() function and method chaining with dot operator (see README.OOP)
  * $ZHOROLOG special variable, returns fractional seconds
  * ZZ commands and functions (calls tags in %ZZCMD and %ZZFN)

**NOTE**: There are excellent M implementations like [InterSystems IRIS](https://www.intersystems.com/products/intersystems-iris), [YottaDB](https://yottadb.com), [FIS GT.M](https://sourceforge.net/projects/fis-gtm) or [MiniM](http://minimdb.com). They have very good support. Otherwise here is the code. Good luck!
