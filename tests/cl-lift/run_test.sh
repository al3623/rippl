#!/bin/sh

cat closure_tests/test$1 | ./compiler.native
cat closure_tests/test$1
