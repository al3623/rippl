#!/bin/bash

cat $1 | ./lexer_driver.native | tee output
