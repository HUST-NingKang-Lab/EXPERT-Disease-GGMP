library(tidyverse)
library(tidyr)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggsignif)  
library(ggsci)
library(randomForest)
library(dplyr)
library(plyr)

#广东省不同市级之间的迁移
  disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
  city <- read.xlsx("/home/wangnan/GGMP-SGMP/data/Guangzhou prefecture level city.xlsx")

  #样本抽样
    #数据准备阶段
      #读取广东的metadata
      ggmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/GGMP.metadata.orignial.xlsx")
      colnames(ggmp_metadata)[which(colnames(ggmp_metadata)=="T2DM")]= "T2D"
      ggmp_metadata <- ggmp_metadata[,disease]

      #读取广东的level6的丰度文件
      ggmp_level6 <- read_tsv("/home/wangnan/GGMP-SGMP/data/ggmp/taxonomy-report/table-filtered-feature-rarefied5k-L6.tsv")

      #认为metadata中的NA都是健康人
      ggmp_metadata[is.na(ggmp_metadata)] <- "n"

      #选取出在metadata和level6中都用到的样本
      ggmp_inter <- intersect(ggmp_metadata[,1],(ggmp_level6 %>% pull(`Sample ID`)))
      ggmp_level6_train <- filter(ggmp_level6,ggmp_level6$`Sample ID` %in% ggmp_metadata[,1])
      ggmp_metadata <- filter(ggmp_metadata,ggmp_metadata$`SampleID` %in% ggmp_inter)
    
    #先选出各地区的样本
      for(i in 2:length(disease)) 
      {
        for (j in 1:nrow(city)) 
        {
          #地区总样本
          pos <- grep(city[j,1],ggmp_metadata[,1])
          metadata <- ggmp_metadata[pos,]
          genus <- filter(ggmp_level6_train,ggmp_level6_train$`Sample ID` %in% metadata[,1])
          
          #为了消除数据的偏好性带来的影响，因此将病例的数量尽量拉到和健康人的数目一致
            pos <- which(metadata[,i] == "y")
            id <- metadata[pos,1]
            for (k in 2:(nrow(metadata)%/%length(pos)-1)) 
            {
              for (l in 1:length(pos)) 
              {
                metadata <- rbind(metadata,metadata[pos[l],])
                metadata[nrow(metadata),1] <- paste0(metadata[nrow(metadata),1],".",k)
                p <- which(genus[,1] == id[l])
                genus <- rbind(genus,genus[p,])
                genus[nrow(genus),1] <- paste0(genus[nrow(genus),1],".",k)
              }
            }
  
          #抽样
          s <- sample(length(metadata[,1]),(length(metadata[,1])*2) %/% 10,replace = FALSE)
          genus_query <- filter(genus,genus$`Sample ID` %in% metadata[s,1])
          genus_train <- filter(genus,genus$`Sample ID` %in% metadata[setdiff(1:nrow(metadata),s),1])

          #行列转置
          genus_query <- tibble::rownames_to_column(as.data.frame(t(genus_query),stringsAsFactors = FALSE),"V0")
          genus_train <- tibble::rownames_to_column(as.data.frame(t(genus_train),stringsAsFactors = FALSE),"V0")
          write_tsv(genus_train,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/trainsource.tsv"),col_names = FALSE)
          write_tsv(genus_query,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/querysource.tsv"),col_names = FALSE)
    
          #制作mapper文件
          metadata_query <- filter(metadata,metadata$SampleID %in% metadata[s,1])
          metadata_train <- filter(metadata,metadata$SampleID %in% metadata[setdiff(1:nrow(metadata),s),1])

          train_mapper <- data.frame(Env = "1",stringsAsFactors = FALSE)
          query_mapper <- data.frame(Env = "1",stringsAsFactors = FALSE)
    
          for (k in 1:nrow(metadata_query)) 
          {
            if(metadata_query[,disease[i]][k] == "n")
            {
              query_mapper <- rbind(query_mapper,paste0(k-1,",","root:healthy",",",metadata_query[k,1]))
            }else
            {
              query_mapper <- rbind(query_mapper,paste0(k-1,",","root:",disease[i],",",metadata_query[k,1]))
            }
          }
          for (k in 1:nrow(metadata_train)) 
          {
            if(metadata_train[,disease[i]][k] == "n")
            {
              train_mapper <- rbind(train_mapper,paste0(k-1,",","root:healthy",",",metadata_train[k,1]))
            }else
            {
              train_mapper <- rbind(train_mapper,paste0(k-1,",","root:",disease[i],",",metadata_train[k,1]))
            }
          }
    
          train_mapper <- as.data.frame(as_tibble(train_mapper[-1,]))
          query_mapper <- as.data.frame(as_tibble(query_mapper[-1,]))
          colnames(train_mapper) <- ",Env,SampleID"
          colnames(query_mapper) <- ",Env,SampleID"
          write_tsv(train_mapper,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/trainmapper.tsv"))
          write_tsv(query_mapper,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/querymapper.tsv"))
    

          #制作biome文件
          biome <- tibble(biome = 1)
          biome <- rbind(biome,"root:healthy")
          biome <- rbind(biome,paste0("root:",disease[i]))
          biome <- biome[-1,]
          write_tsv(biome,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/biome.tsv"),col_names = FALSE)
    

          #制作路径文件
          path_train <- tibble(path_ggmp_train = 1)
          path_query <- tibble(path_sgmp_query = 1)
          path_train <- rbind(path_train,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/trainsource.tsv"))
          path_query <- rbind(path_query,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/querysource.tsv"))
          path_train <- path_train[-1,]
          path_query <- path_query[-1,]
          write_tsv(path_train,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/path_train.tsv"),col_names = FALSE)
          write_tsv(path_query,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/path_query.tsv"),col_names = FALSE)
        }
      }

