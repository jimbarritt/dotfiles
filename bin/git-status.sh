#!/bin/bash

echo "Going to find git status of all subdirs from here"

for dir in `PWD`/*
do
    dir=${dir%*/}
    echo ${dir##*/}
    cd ${dir}; git st; cd ..
done
