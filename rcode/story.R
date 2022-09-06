library(tidyverse)
library(openxlsx)
library(randomForest)

    disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
    city <- read.xlsx("/home/wangnan/GGMP-SGMP/data/Guangzhou prefecture level city.xlsx")


    #for(i in c(2:8))
    #{
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i]))
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei"))
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/data"))
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp"))
        for(j in 1:14)
        {
            dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/data/exp",j))
            dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j))

            for(k in 1:nrow(city))
            {
                if(city[k,2] != "Shanwei")
                {
                    dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2]))
                }
            }
        }
    #}

    #创建一个空表，用于后面收集菌种的信息
        importance <- data.frame(matrix(nrow = 20*14,ncol =8))
            colnames(importance) <- disease
            importance$SampleID  <- rep(city[,2],each = 20)

    importance <- read.csv("/home/wangnan/GGMP-SGMP/figure/exp_region/importance.csv")
    importance <- importance[,-1]
    #筛选在大多数城市出现的菌种以及多数病或者少数病中出现的菌种
        d1 <- as.data.frame(table(importance$Constipation)[table(importance$Constipation) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d1 <- d1[order(d1$Freq,decreasing = T),]
        d2 <- as.data.frame(table(importance$COPD)[table(importance$COPD) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d2 <- d2[order(d2$Freq,decreasing = T),]
        d3 <- as.data.frame(table(importance$Gastritis)[table(importance$Gastritis) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d3 <- d3[order(d3$Freq,decreasing = T),]
        d4 <- as.data.frame(table(importance$Kidneystone)[table(importance$Kidneystone) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d4 <- d4[order(d4$Freq,decreasing = T),]
        d5 <- as.data.frame(table(importance$Metabolic_syndrome)[table(importance$Metabolic_syndrome) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d5 <- d5[order(d5$Freq,decreasing = T),]
        d6 <- as.data.frame(table(importance$Rheumatoid_arthritis)[table(importance$Rheumatoid_arthritis) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d6 <- d6[order(d6$Freq,decreasing = T),]
        d7 <- as.data.frame(table(importance$T2D)[table(importance$T2D) > 6], stringsAsFactors = FALSE) #选择在一半及以上城市出现的菌种
            d7 <- d7[order(d7$Freq,decreasing = T),]

        #Reduce(intersect,list(d1$Var1,d2$Var1,d3$Var1,d4$Var1,d5$Var1,d6$Var1,d7$Var1)) #七种疾病都出现的菌种
        d <- as.data.frame(table(c(d1$Var1,d2$Var1,d3$Var1,d4$Var1,d5$Var1,d6$Var1,d7$Var1)),stringsAsFactors = FALSE) #所有筛选的菌种
            d <- d[order(d$Freq,decreasing = T),]
            d$Freq[21] <- "Rheumatoid_arthritis"
            d$Freq[25] <- "Constipation"
            d$Freq[26] <- "Gastritis"
            d$Freq[28] <- "COPD"
        d_spe <- d[c(21,25,26,28),] #特异性疾病菌种
        d_com <- d[c(1:5,7:11),] #至少有5种疾病都有的菌种（基石菌种）

 
    #分别去掉对应的菌种，然后开始   
        species <- rbind(d_com,d_spe)
        for(i in 2:8)
        {
            for(j in 1:nrow(species))
            {
                pos <- as.numeric(strsplit(strsplit(species$Var1[j],".",fixed = T)[[1]][1],"X",fixed = T)[[1]][2]) #去掉菌种的位置
                #训练集
                    data <- read_tsv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/Shanwei/trainsource.tsv"))
                        data <- data[-pos,]
                        write_tsv(data,paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/data/exp",j,"/trainsource.tsv"))
                #路径    
                    path <- paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/data/exp",j,"/trainsource.tsv")
                        write_tsv(as.data.frame(path),paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/data/exp",j,"/trainpath.tsv"),col_names =FALSE)
                #mapper
                    file.copy(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/Shanwei/trainmapper.tsv"),
                        paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/data/exp",j,"/trainmapper.tsv"),overwrite = TRUE)

                for (k in 1:nrow(city)) 
                {
                    if(city[k,2] != "Shanwei")
                    {
                        #迁移集
                            data <- read_tsv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[k,2],"/trainsource.tsv"))
                                data <- data[-pos,]
                                write_tsv(data,paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/transfersource.tsv"))
                        #路径    
                            path <- paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/transfersource.tsv")
                                write_tsv(as.data.frame(path),paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/transferpath.tsv"),col_names =FALSE)
                        #transfermapper
                            file.copy(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[k,2],"/trainmapper.tsv"),
                                paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/trainmapper.tsv"),overwrite = TRUE)
                            file.rename(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/trainmapper.tsv"),
                                paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/transfermapper.tsv"))

                        #搜索集
                            data <- read_tsv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[k,2],"/querysource.tsv"))
                                data <- data[-pos,]
                                write_tsv(data,paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/querysource.tsv"))
                        #路径    
                            path <- paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/querysource.tsv")
                                write_tsv(as.data.frame(path),paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/querypath.tsv"),col_names =FALSE)
                        #querymapper
                            file.copy(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[k,2],"/querymapper.tsv"),
                                paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",disease[i],"/Shanwei/exp/exp",j,"/",city[k,2],"/querymapper.tsv"),overwrite = TRUE)
                    }
                }
            }
        }

        write.csv(importance,"/home/wangnan/GGMP-SGMP/figure/exp_region/importance.csv")
        
    #统计去掉菌种前后迁移学习模型准确性的平均变化@
        accuracy <- data.frame(matrix(nrow = 14,ncol =9))
            colnames(accuracy) <- c("species",disease[2:8],"variance")
            accuracy$species  <- species$Var1
            for(i in 1:nrow(accuracy))
            {
                tmp <- unlist(strsplit(accuracy$species[i],".",fixed = T))
                tmp <- tmp[2:length(tmp)]
                accuracy$species[i] <- paste(tmp,collapse = ".")
            }
        c <- city$City[c(1:10,12:14)] #选择除了Shanwei以外的城市
        for(i in 1:nrow(accuracy))
        {
            for(j in 2:(ncol(accuracy)-1))
            {   
                #去掉菌种之前的AUROC
                    auc_before <- c()
                    for (k in c) 
                    {
                        auc <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp4/",colnames(accuracy)[j],"/exp/Shanwei/",k,"/Evaluation_Transfer/overall.csv"))
                        auc_before <- c(auc_before,auc$ROC.AUC)
                    }
                #去掉菌种之后的AUROC
                    auc_after <- c()
                    for (k in c) 
                    {
                        auc <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_story/",colnames(accuracy)[j],"/Shanwei/exp/exp",i,"/",k,"/Evaluation_Transfer/overall.csv"))
                        auc_after <- c(auc_after,auc$ROC.AUC)
                    }
                accuracy[i,j] <- round(mean(auc_before) - mean(auc_after),4) #算出AUC的平均变化值
            }

            #计算每个疾病的方差
                accuracy$variance[i] <- round(var(as.numeric(accuracy[i,2:8])),4)
        }
        view(accuracy)
        write.csv(accuracy,"/home/wangnan/GGMP-SGMP/figure/exp_region/accuracy.csv")