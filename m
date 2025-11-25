#!/bin/bash
GNUMAKE=gnumake
$GNUMAKE -f Makefile.me clean
$GNUMAKE -f Makefile.me darwin.rpath 2>&1 | tee make.log
