#!/bin/sh
g++ -std=c++1y -g source/compiler.cpp -o bin/smugc -Wall -pedantic `llvm-config --cppflags --ldflags --system-libs --libs`
