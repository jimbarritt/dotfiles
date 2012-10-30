#!/bin/sh

echo "Going to link up all the nice dot-files for you..."

#ln -s bin ~/bin

CURRENT_DIR=$(pwd)

ln -s ${CURRENT_DIR}/bin ~/bin
ln -s ${CURRENT_DIR}/img ~/img

