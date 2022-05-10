#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/" 
for i in {2..4}
do
	for disease in $(ls $path"exp"$i"/")
	do
		for city1 in $(ls $path"exp"$i"/"$disease"/exp/")
		do
			for city2 in $(ls $path"exp"$i"/"$disease"/exp/"$city1"/")
			do
				expert transfer \
					-i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/trainCM.h5 \
					-l /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/trainlabels.h5 \
					-m /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/Independent \
					-t /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/ontology.pkl	\
					-o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Transfer \
					--finetune --update-statistics
			done
		done
	done
done
