#!/bin/bash

BASENAME="$(echo $1 | cut -f1 -d".")"
echo $BASENAME
./latc $1 >"${BASENAME}.s"
gcc -m32 -fPIC -o runtime.o -c runtime.c
gcc -m32 -fPIC -o "${BASENAME}.o" -c "${BASENAME}.s"
gcc -m32 -fPIC -o "${BASENAME}" "${BASENAME}.o" runtime.o
