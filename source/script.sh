#!/usr/bin/env bash

for FILE in $@;
do
    if ! ./pgoc "${FILE}"; then
        exit 1
    fi

    if ! gcc -no-pie "${FILE/.go}.s"; then
        exit 1
    fi

    if ! ./a.out; then
        exit 1
    fi    
done