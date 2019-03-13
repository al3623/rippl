#!/bin/bash
#assert.sh

make
cd lexing_tests

num_tests=$(ls -1q test* | wc -l)

for var in `seq 1 1 $num_tests`
do
    cat test$var | ../lexdriver.native > "$var.out"
	file_diff=$(diff $var.out ../lexing_output)
	rm "$var.out"
	if [ -z "$file_diff"] 
	then echo "passed $var"
   	else  exit 1 
	fi
done

cd -
