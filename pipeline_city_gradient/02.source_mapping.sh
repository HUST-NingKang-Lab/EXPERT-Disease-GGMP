#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/" 
for i in {1..4}
do
	for disease in $(ls $path"exp"$i"/")
	do
		for city in $(ls $path"exp"$i"/"$disease"/data/")
		do
			expert map --to-otlg \
				-t /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/ontology.pkl \
				-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/trainmapper.tsv \
				-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/trainlabels.h5
			expert map --to-otlg \
				-t /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/ontology.pkl \
				-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/querymapper.tsv \
				-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/querylabels.h5
		done
	done
done
