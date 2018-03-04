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

if [ -n "$OUTPUT" ] 
	then
		mkdir -p .smug
		# Compile c output
		gcc -xc $RESULT.c -o c$OUTPUT -Wno-implicit-function-declaration  -Wno-parentheses-equality -Wno-unused-value
		# Compile llvm ir output
		clang -x ir -Wno-override-module $RESULT.ll -o llvm$OUTPUT
fi		