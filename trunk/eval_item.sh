#!/usr/bin/env bash

if [ $# -lt 3 ]
then
  echo "Usage: `basename $0` project_path item_path value [params]"
  exit 1
fi

project_path=$1
item_path=$2
value=$3
params=$4

script_path=/apps/strategico

cd $script_path
	
#command="${script_path}/eval_item.Rscript ${project_path} ${item_path} ${value} \"${params}\""
command="${script_path}/eval_item.Rscript ${project_path} ${item_path} ${value} ${params}"
echo "Running ${command}"

/apps/R/bin/Rscript $command

