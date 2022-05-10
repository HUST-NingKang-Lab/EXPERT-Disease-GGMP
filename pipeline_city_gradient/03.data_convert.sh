#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/" 
for i in {1..4}
do
	for disease in $(ls $path"exp"$i"/")
	do
		for city in $(ls $path"exp"$i"/"$disease"/data/")
		do
			expert convert \
				-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/path_train.tsv \
				-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/trainCM.h5 --in-cm
			expert convert \
				-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/path_query.tsv \
				-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/queryCM.h5 --in-cm 
		done
	done
done