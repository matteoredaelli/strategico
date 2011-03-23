#!/usr/bin/env bash

if [ $# -lt 2 ]
then
  echo "Usage: `basename $0` project_name value"
  exit 1
fi

project_name=$1
value=$2

script_path=/apps/strategico

cd $script_path
	
command="${script_path}/eval_items_from_db.Rscript ${project_name} ${value}"
echo "Running ${command}"

/apps/R/bin/Rscript $command

