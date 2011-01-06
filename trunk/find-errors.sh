#!/usr/bin/env bash

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` projectPath valeName"
  exit 1
fi

project_path=$1
value=$2

for file in $(find $project_path -name item.Rdata) ; do 
	dir=$(dirname $file)
	count=$(ls  ${dir} | grep report | wc -l| grep -v value )
	echo "${dir}:${count}"
done
