#!/usr/bin/env bash

if [ $# -ne 4 ]
then
  echo "Usage: `basename $0` projectPath results|summary depth valueName"
  exit 1
fi

project_path=$1
report=$2
maxdepth=$3
value=$4

for folder in $(find $project_path -maxdepth $maxdepth -mindepth 2 -name "report-${value}" -type d)
do
	itemfolder=$(dirname $folder)
	key=$(basename $itemfolder)	
	for row in $(grep -v VALUE ${folder}/item-${report}.csv)
	do
		echo "${itemfolder},${row}"
	done
done
