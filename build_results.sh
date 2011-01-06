#!/usr/bin/env bash

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` projectPath valeName"
  exit 1
fi

project_path=$1
value=$2

find $project_path -name "*-${value}-results.csv" -exec cat {} \; > ${project_path}/${value}-results.xls
