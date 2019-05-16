#!/bin/bash
#assert.sh

for file in *.rpl
do
	rippl $file -t &> /dev/null
	if [ $? -ne 0 ]  
	then echo -e "\e[39mfailed \e[91m$file"
   	else  echo -e "\e[39mpassed \e[92m$file"
	fi
done

