#!/bin/zsh
#assert.sh


for file in *.rpl
do
	rippl $file &> /dev/null
	if [ $? -ne 0 ]  
	then echo -e "\e[39m$file \e[91mfailed"
   	else  echo -e "\e[39m$file \e[92mpassed"
	fi

	./${file: : -4} &> /dev/null
	if [ $? -ne 0 ]  
	then echo -e "\e[39m$file \e[91mfailed"
   	else echo -e "\e[39m$file \e[92mpassed"
	fi
	
	rm -rf ${file: : -4} &> /dev/null
        rm -rf *.byte *.byte.s &> /dev/null
done

