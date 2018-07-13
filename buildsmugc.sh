#!/bin/sh
g++ -std=c++1z -g source/compiler.cpp -o bin/smugc -Wall -pedantic -Wno-unused-const-variable `llvm-config --cppflags --ldflags --system-libs --libs`
