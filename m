#!/bin/bash
gnumake -f Makefile.me clean
gnumake -f Makefile.me darwin.rpath 2>&1 | tee make.log
