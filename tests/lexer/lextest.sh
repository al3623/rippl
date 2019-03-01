#!/bin/bash
if [ "$#" -lt 1 ]; then
    echo "PLEASE SPECIFY 1 OR MORE FILES TO LEX >:O XD"
    exit 1
fi

for var in "$@"
do
    cat $var | ./lexdriver.native | tee "$var.out"
done
