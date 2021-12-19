#!/bin/bash

if [ $# -lt 1 ]; then
  INPUT_FILE=day1.in
else
  INPUT_FILE=$1
fi

cat $INPUT_FILE | sed -e '1i0' | paste -s -d' ' - | bc
