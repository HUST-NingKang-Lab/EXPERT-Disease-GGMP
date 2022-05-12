# EXPERT-Disease-GGMP
Recently, machine learning based on microbial features has drawn increasing interests in diagnosis of diverse diseases such as IBD, Type-2 Diabate, etc. However,  current methods were unable to mitigate the regional effects, which made it difficult to perform microbial-based cross-regional diagnosis of diseases.

In this study, we proposed a machine learning framework EXPERT-Disease which integrated the neural network and transfer learning for microbial-based cross-regional diagnosis of diseases.We selected Guangdong Gut Microbiome Project (GGMP, EBI ID: [PRJEB18535](https://www.ebi.ac.uk/ena/browser/view/PRJEB18535?show=reads)) samples and categorized them into 14 cities according to the sample sources. EXPERT-Disease trains from scratch to build a disease neural network model (DNN) based on the training set of each city, and then uesd training set from other cities for transfer learning to obtain the transferred neural network model (TDNN). Finally, the TDNN can be used for disease diagnosis in other cities.
To get the the disease model of each city, please download [DNN model for each city](https://github.com/HUST-NingKang-Lab/EXPERT-Disease-GGMP/releases/tag/models).

## Get and use
To learn how to install the model and how to use it, click [here](https://github.com/HUST-NingKang-Lab/EXPERT)

<img src="https://github.com/HUST-NingKang-Lab/EXPERT-Disease-GGMP/blob/main/transfer%20learning.png" style="zoom:150%;" />

``
The transfer learning workflow of microbial-based cross-regional diagnosis of diseases: (1) Foundamental model establishment: ab initio train the disease neural network (DNN) model on the training set of city A. (2) Transfer model establishment: implement transfer learning to generate a transfer DNN for another city B by using the training set of city B. (3)Classification test: test the transfer DNN on the testing set of city B.
``
## Example
Here we choose Shenzhen(City A) and Guangzhou(City B) as the source city and target city, and we obtained genus-level species abundance tables for these two cities, 
individuals with T2D are the positive samples and controls are the negitive samples.

Genus-level species abundance tables([reference format](https://github.com/HUST-NingKang-Lab/EXPERT)): city1_train.tsv  city1_test.tsv  city2_train.tsv  city2_test.tsv       
Biome profiles([reference format](https://github.com/HUST-NingKang-Lab/EXPERT)): biome.tsv      
Mapper profiles([reference format](https://github.com/HUST-NingKang-Lab/EXPERT)): city1_train_mapper.tsv  city1_test_mapper.tsv  city2_train_mapper.tsv city2_test_mapper.tsv      
#### Ontology construct
- Construct a biome ontology representing stages of T2D. You'll see constructed ontology like a tree in the printed message.
```
expert construct -i biome.tsv -o ontology.pkl
```
#### Source mapping
- Map microbial community samples to the biome ontology to obtain hierarchical labels. You'll see counts of the samples on each biome ontology layer in the printed message.
```
expert map --to-otlg -i city1_train_mapper.csv -t ontology.pkl -o city1_train_labels.h5
expert map --to-otlg -i city1_test_mapper.csv -t ontology.pkl -o city1_test_labels.h5
expert map --to-otlg -i city2_train_mapper.csv -t ontology.pkl -o city2_train_labels.h5
expert map --to-otlg -i city2_test_mapper.csv -t ontology.pkl -o city2_test_labels.h5
```
#### Data convert
- Convert input abundance data to model-acceptable hdf file. The EXPERT model only accepts standardized abundance data. Here we standardize the abundance data using convert mode.
```
ls city1_train.tsv > inputlist; expert convert -i inputlist -o city1_trainCM.h5 --in-cm;
ls city1_test.tsv > inputlist; expert convert -i inputlist -o city1_testCM.h5 --in-cm;
ls city2_train.tsv > inputlist; expert convert -i inputlist -o city2_trainCM.h5 --in-cm;
ls city2_test.tsv > inputlist; expert convert -i inputlist -o city2_testCM.h5 --in-cm;
```
#### Ab initio training
- Train the disease neural network model from scratch. Here we will use ontology.pkl and hdf files.
```
expert train -i city1_trainCM.h5 -l city1_train_labels.h5 -t ontology.pkl -o city1_DNN
```
#### Transfer learning
- Transfer the knowledge of city B to the DNN model of city A for better performance in disease diagnosis on city B. You'll see running log and training process in the printed message.
```
expert transfer -i city2_trainCM.h5 -l city2_train_labels.h5 -t ontology.pkl -m  city1_DNN -o Transfer_DNN
```
#### Search
- Search the test set of city B against the transferred DNN model.
```
expert search -i city2_testCM.h5 -m Transfer_DNN -o Search_Transfer_DNN
```
#### Evaluation
- Evaluate the performance of the Transferred DNN model. You'll obtain a performance report.
```
expert evaluate -i Search_Transfer_DNN -l city2_test_labels.h5 -o Evaluation
```
