#!/bin/sh

if [ -z "$1" ] 
	then
		echo "No input files!"
		exit 1
fi

OUTPUT=$2
if [ -z "$OUTPUT" ] 
	then 
		OUTPUT=".out" 
fi

RESULT=$(./bin/smugc $1)

if [ -n "$RESULT" ] 
	then
		mkdir -p .smug
		# Compile llvm ir output
		clang -x ir -Wno-override-module $RESULT.ll -o llvm$OUTPUT
		exit 0
fi

exit 1
