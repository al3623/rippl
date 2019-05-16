#!/bin/bash
#assert.sh

num_tests=$(ls -1q test* | wc -l)

for var in `seq 1 1 $num_tests`
do
	rippl -t test$var.rpl &> /dev/null
	if [ $? -ne 0 ]  
	then echo "failed test$var"
   	else  echo "passed test$var"
	fi
done

