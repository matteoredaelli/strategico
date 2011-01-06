#!/usr/bin/env bash

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` projectPath valeName"
  exit 1
fi

project_path=$1
value=$2

echo Creating XLS file
find $project_path -name "*-${value}.summary" -exec cat {} \; > ${project_path}/${value}-summary.xls 

echo Deleting .summary files
find $project_path -name "*-${value}.summary" | xargs rm
