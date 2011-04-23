#!/usr/bin/env bash

if [ $# -lt 4 ]
then
  echo "Usage: `basename $0` project_name id_min id_max values [params]"
  exit 1
fi

script_path=$(dirname $0)
cd ${script_path}

project_name=$1
id_min=$2
id_max=$3
values=$4
params=$5


command="${script_path}/eval_items.Rscript ${project_name} ${id_min} ${id_max} ${values} ${params}"
echo "Running ${command}"

Rscript $command
