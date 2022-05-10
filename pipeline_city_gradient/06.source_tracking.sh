#!/bin/bash

path="/home/wangnan/GGMP-SGMP/exp_region/city_gradient/" 
for i in {1..4}
do
        for disease in $(ls $path"exp"$i"/")
        do
                for city1 in $(ls $path"exp"$i"/"$disease"/exp/")
                do
                        expert search \
                                -i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/queryCM.h5 \
                                -o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/Search_Independent \
                                -m /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/Independent

                        for city2 in $(ls $path"exp"$i"/"$disease"/exp/"$city1"/")
                        do
                                expert search \
                                        -i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/queryCM.h5 \
                                        -o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Search_Independent \
                                        -m /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city1/Independent
       		                expert search \
                                        -i /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/data/$city2/queryCM.h5 \
                                        -o /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Search_Transfer \
                                        -m /home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp$i/$disease/exp/$city1/$city2/Transfer
                        done
                done
        done
done
