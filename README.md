MUMPS V1R2
==========

Based on [MUMPS V1 1.65](http://sf.net/projects/mumps) from Ray Newman.

Fixes:

  * works on big-endian machines
  * works on OS X 10.10 Yosemite using clang

Enhancement:

  * 31 character label/routine/local/global names

You can compile a MUMPS V1 compatible version if you define
MAX_NAME_BYTES in include/mumps.h as 8.

**NOTE**: The database format is not compatible with MUMPS V1, it 
cannot open databases created with MUMPS V1.
