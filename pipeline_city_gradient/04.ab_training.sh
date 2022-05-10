#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/" 
for i in {1..4}
do
	for disease in $(ls $path"exp"$i"/")
	do
		for city in $(ls $path"exp"$i"/"$disease"/data/")
		do
			expert train \
				-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/trainCM.h5 \
				-l /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/trainlabels.h5 \
				-t /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/ontology.pkl \
				-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/Independent	
		done
	done
done