#!/usr/bin/env bash

if [ $# -ne 3 ]
then
  echo "Usage: `basename $0` projectPath results|summary valueName"
  exit 1
fi

project_path=$1
report=$2
value=$3

target_file=${project_path}/report-${value}/children-${report}.csv

head=$(head -1 $project_path/report-${value}/item-${report}.csv)
echo "Key,${head}" 
for folder in $(find $project_path -name "report-${value}" -type d -maxdepth 2 -mindepth 2) 
do
	itemfolder=$(dirname $folder)
	key=$(basename $itemfolder)	
	prediction=$(tail -1 ${folder}/item-${report}.csv)
	echo "${key},${prediction}"
done
