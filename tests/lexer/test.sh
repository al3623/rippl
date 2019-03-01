#!/bin/bash

cat $1 | ./lexdriver.native | tee output
