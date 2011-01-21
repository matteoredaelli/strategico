#!/usr/bin/env bash

if [ $# -ne 3 ]
then
  echo "Usage: `basename $0` projectPath results|summary valueName"
  exit 1
fi

project_path=$1
report=$2
value=$3

find $project_path -name "report-${value}" -type d -maxdepth 2 -mindepth 2  -exec tail -1 {}/item-${report}.csv \; > ${project_path}/report-${value}/children-${report}.csv

