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

user=$(/usr/bin/whoami)

all_params="${project_path} ${new_item_path} ${value} \"${params}\""
job_name=${project_name}-/${id_min}:${id_max}
echo "Running params ${all_params}"

if [ "${user}" == "r" ] ; then
	sbatch --workdir=/tmp --job-name=${job_name} ${script_path}/eval_items.sh ${all_params}
else
	sudo sbatch --uid=1001 --gid=1001 --job-name=${job_name} --workdir=/tmp ${script_path}/eval_items.sh ${all_params}
fi
done

