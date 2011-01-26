#!/usr/bin/env bash

if [ $# -lt 3 ]
then
  echo "Usage: `basename $0` project_path item_path depth value [params]"
  exit 1
fi

project_path=$1
item_path=$2
maxdepth=$3
value=$4
params=$5

script_path=${STRATEGICO_SCRIPTS}

cd $project_path/$item_path
for file in $(find . -name item.Rdata  -maxdepth ${maxdepth} -mindepth 1) ; do
	cd $script_path
	file_path=$(dirname $file)
	new_item_path=$(echo ${item_path}/${file_path} | sed -e 's://:/:g' -e "s:\./::g")
	all_params="${project_path} ${new_item_path} ${value} ${params}"
	echo "Running params ${all_params}"
	${script_path}/eval_item.sh ${all_params}
done

