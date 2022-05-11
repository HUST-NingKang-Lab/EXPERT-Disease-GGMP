# EXPERT-Disease-GGMP
Recently, machine learning based on microbial features has drawn increasing interests in diagnosis of diverse diseases such as IBD, Type-2 Diabate, etc. However,  current methods were unable to mitigate the regional effects, which made it difficult to perform microbial-based cross-regional diagnosis of diseases.

In this study, we proposed a machine learning framework EXPERT-Disease which integrated the neural network and transfer learning for microbial-based cross-regional diagnosis of diseases.We selected Guangdong Gut Microbiome Project (GGMP, EBI ID: [PRJEB18535](https://www.ebi.ac.uk/ena/browser/view/PRJEB18535?show=reads)) samples and categorized them into 14 cities according to the sample sources. EXPERT-Disease trains from scratch to build a disease neural network model (DNN) based on the training set of each city, and then uesd training set from other cities for transfer learning to obtain the transferred neural network model (TDNN). Finally, the TDNN can be used for disease diagnosis in other cities.

To get the the disease model of each city, please download [DNN model for each city](https://github.com/HUST-NingKang-Lab/EXPERT-Disease-GGMP/releases/tag/models).

## Get and use
To learn how to install the model and how to use it, click [here](https://github.com/HUST-NingKang-Lab/EXPERT)

<img src="https://github.com/HUST-NingKang-Lab/EXPERT-Disease-GGMP/blob/main/transfer%20learning.png" style="zoom:150%;" />

## Example
Here we choose Shenzhen(City A) and Guangzhou(City B) as the source city and target city, and we obtained genus-level species abundance tables for these two cities, 
individuals with T2D are the positive samples and controls are the negitive samples.

Genus-level species abundance tables: city1.tsv city2.tsv
Biome profiles([reference format](https://github.com/HUST-NingKang-Lab/EXPERT)): biome.tsv
Mapper profiles([reference format](https://github.com/HUST-NingKang-Lab/EXPERT)): mapper1.tsv mapper2.tsv
###
