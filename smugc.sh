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


mkdir -p .smug
# Compile c output
gcc -xc $(./bin/smugc $1).c -o c$OUTPUT -Wno-implicit-function-declaration  -Wno-parentheses-equality -Wno-unused-value
# Compile llvm ir output
clang -x ir -Wno-override-module $(./bin/smugc $1).ll -o llvm$OUTPUT