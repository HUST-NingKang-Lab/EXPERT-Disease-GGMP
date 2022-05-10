#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/"
for i in {1..4}
do
        for disease in $(ls $path"exp"$i"/")
        do
                for city1 in $(ls $path"exp"$i"/"$disease"/exp/")
                do
                        expert evaluate \
                                -i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/Search_Independent \
                                -l /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/querylabels.h5 \
                                -o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/Evaluation_Independent

                        for city2 in $(ls $path"exp"$i"/"$disease"/exp/"$city1"/")
                        do
                                expert evaluate \
                                        -i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Search_Independent \
                                        -l /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/querylabels.h5 \
                                        -o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Evaluation_Independent
        	                expert evaluate \
                                        -i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Search_Transfer \
                                        -l /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/querylabels.h5 \
                                        -o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2//Evaluation_Transfer
                        done
                done
        done
done
