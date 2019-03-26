#!/bin/bash
make

#if [ "$#" -lt 1 ]; then
#    echo "missing arg"
#    exit 1
#fi

while IFS='' read -r line || [[ -n "$line" ]]; do
    echo $line | ./driver.native
done < "parsing_tests/test1"

