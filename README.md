MUMPS V1R2
==========

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.

Changes:

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

Fixes:

  * works on big-endian machines
  * works on OS X 10.10 Yosemite using clang

**NOTE**: The database format is not compatible with MUMPS V1, it 
cannot open databases created with MUMPS V1.
