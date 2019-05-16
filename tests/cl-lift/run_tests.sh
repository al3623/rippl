#!/bin/bash
#assert.sh

for file in *.rpl
do
	rippl -l $file &> /dev/null
	if [ $? -ne 0 ]  
	then echo -e "\e[39m$file \e[91mfailed"
   	else  echo -e "\e[39m$file \e[92mpassed"
	fi
done

