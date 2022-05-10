#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/" 
for i in {1..4}
do
	for disease in $(ls $path"exp"$i"/")
	do
		for city in $(ls $path"exp"$i"/"$disease"/data/")
		do
			expert construct \
				-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/biome.tsv \
				-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city/ontology.pkl
		done
	done
done


