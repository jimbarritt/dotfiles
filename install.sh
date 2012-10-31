#!/bin/sh

echo "Going to link up all the nice dot-files for you..."

CURRENT_DIR=$(pwd)

if [ -e ~/bin ] 
then
    unlink ~/bin
fi

if [ -e ~/img ] 
then
    unlink ~/img
fi


ln -sv ${CURRENT_DIR}/bin ~/bin
ln -sv ${CURRENT_DIR}/img ~/img

for file in .*
do
    if [[ ${file} != ".git" && ${file} != "." && ${file} != ".." ]]
    then	
	ln -svf ${CURRENT_DIR}/${file} ~/${file}
    fi
done

echo "# Here is where you put local customisations" > ~/.bashrc_workstation

echo "source ~/.bashrc"
source ~/.bashrc
