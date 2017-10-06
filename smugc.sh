#!/bin/sh

if [ -z "$1" ] 
	then
		echo "No input files!"
		exit 1
fi

OUTPUT=$2
if [ -z "$OUTPUT" ] 
	then 
		OUTPUT="a.out" 
fi


mkdir -p .smug
gcc -xc $(./bin/smugc $1) -o $OUTPUT -Wno-implicit-function-declaration
