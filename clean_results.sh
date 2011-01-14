#!/usr/bin/env bash

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` projectPath valueName"
  exit 1
fi

project_path=$1
value=$2

echo $project_path

for dir in $(find $project_path -name "report-${value}" -type d ) ; do 
	rm -r  ${dir}
done
