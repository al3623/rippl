#!/bin/bash
#assert.sh

for file in *.rpl
do
	rippl $file
	if [ $? -ne 0 ]  
	then echo "failed test$var"
   	else  echo "passed test$var"
	fi

	./${file::-4}
	if [ $? -ne 0 ]  
	then echo "failed test$var"
   	else  echo "passed test$var"
	fi

done

