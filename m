#!/bin/bash
gnumake -f Makefile.me clean
gnumake -f Makefile.me 2>&1 | tee make.log
