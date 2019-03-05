#!/bin/bash
make
cd lexing_tests

num_tests=$(ls -1q test* | wc -l)

for var in `seq 1 1 $num_tests`
do
    cat test$var | ../lexdriver.native | tee "$var.out"
done

cd -
