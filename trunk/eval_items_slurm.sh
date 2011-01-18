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

user=$(/usr/bin/whoami)

script_path=${STRATEGICO_SCRIPTS}

project_name=$(basename $project_path)

cd $project_path/$item_path
for file in $(find . -maxdepth 2 -mindepth 2 -name item.Rdata) ; do
	cd $script_path
	file_path=$(dirname $file)
	new_item_path=$(echo ${item_path}/${file_path} | sed -e 's://:/:g' -e "s:\./::g")
	all_params="${project_path} ${new_item_path} ${value} \"${params}\""
	job_name=${project_name}/${new_item_path}/${value}
	echo "Running params ${all_params}"
	if [ "${user}" == "r" ] ; then
		sbatch --workdir=/tmp --job-name=${job_name} ${script_path}/eval_items.sh ${all_params}
	else
		sudo sbatch --uid=1001 --gid=1001 --job-name=${job_name} --workdir=/tmp ${script_path}/eval_items.sh ${all_params}
	fi
done

