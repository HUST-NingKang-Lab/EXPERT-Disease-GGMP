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
library(cowplot)
#选择可能可以利用的疾病
  disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")

#构建文件
#for(i in 2:5)
{
  for(j in 2:length(disease))
  {
    dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",i,"/",disease[j]))
    for (k in 1:9) 
    {
           dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",i,"/",disease[j],"/exp",k))
    }
  }
}

#先整体进行广东到山东的迁移，按照疾病划分，找出可能的疾病(为了画数据量和AUROC的相关性，一共做了五次实验)1
for (n in 1:5) 
{   
  for(i in 2:length(disease)) 
  {
    #读取山东的metadata
    sgmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/sgmp/SGMP_metadata_700_selected.xlsx")
    colnames(sgmp_metadata)[which(colnames(sgmp_metadata)=="host_status")]= "T2D"
    #读取广东的metadata
    ggmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/GGMP.metadata.orignial.xlsx")
    colnames(ggmp_metadata)[which(colnames(ggmp_metadata)=="T2DM")]= "T2D"
    ggmp_metadata <- ggmp_metadata[,disease]
    sgmp_metadata <- sgmp_metadata[,disease]

    #读取广东的level6的丰度文件
    ggmp_level6 <- read_tsv("/home/wangnan/GGMP-SGMP/data/ggmp/taxonomy-report/table-filtered-feature-rarefied5k-L6.tsv")
    #读取山东的level6的丰度文件
    sgmp_level6 <- read_tsv("/home/wangnan/GGMP-SGMP/data/sgmp/taxonomy-report/table-filtered-feature-rarefied5k-L6.tsv")

    #认为metadata中的NA都是健康人
    ggmp_metadata[is.na(ggmp_metadata)] <- "n"
    sgmp_metadata[is.na(sgmp_metadata)] <- "n"
    sgmp_metadata$T2D[which(sgmp_metadata$T2D == "Type 2 diabetes")] <- "y"
    sgmp_metadata$T2D[which(sgmp_metadata$T2D == "Health")] <- "n"

    #选取出在metadata和level6中都用到的样本
    ggmp_inter <- intersect(ggmp_metadata[,1],(ggmp_level6 %>% pull(`Sample ID`)))
    ggmp_level6_train <- filter(ggmp_level6,ggmp_level6$`Sample ID` %in% ggmp_metadata[,1])
    ggmp_metadata <- filter(ggmp_metadata,ggmp_metadata$`SampleID` %in% ggmp_inter)
    sgmp_level6_train <- filter(sgmp_level6,sgmp_level6$`Sample ID` %in% sgmp_metadata[,1])

    #挑出来绝对健康患病的样本
      #广东
        pos <- c()
        for (k in 1:nrow(ggmp_metadata)) 
        {
          if(all(ggmp_metadata[k,2:ncol(ggmp_metadata)] == "n")|ggmp_metadata[k,i] == "y")
          {
            pos <- c(pos,k)
          }
        }
        ggmp_metadata <- ggmp_metadata[pos,]
      #山东
        pos <- c()
        for (k in 1:nrow(sgmp_metadata)) 
        {
          if(all(sgmp_metadata[k,2:ncol(sgmp_metadata)] == "n")|sgmp_metadata[k,i] == "y")
          {
            pos <- c(pos,k)
          }
        }
        sgmp_metadata <- sgmp_metadata[pos,]
        ggmp_level6_train <- filter(ggmp_level6_train,ggmp_level6_train$`Sample ID` %in% ggmp_metadata[,1])
        sgmp_level6_train <- filter(sgmp_level6_train,sgmp_level6_train$`Sample ID` %in% sgmp_metadata[,1])


    #为了消除数据的偏好性带来的影响，因此将病例的数量尽量拉到和健康人的数目一致
    #广东
      pos <- which(ggmp_metadata[,i] == "y")
      id <- ggmp_metadata[pos,1]
      for (k in 2:(nrow(ggmp_metadata)%/%length(pos)-1)) 
      {
        for (l in 1:length(pos)) 
        {
          ggmp_metadata <- rbind(ggmp_metadata,ggmp_metadata[pos[l],])
          ggmp_metadata[nrow(ggmp_metadata),1] <- paste0(ggmp_metadata[nrow(ggmp_metadata),1],".",k)
          p <- which(ggmp_level6_train[,1] == id[l])
          ggmp_level6_train <- rbind(ggmp_level6_train,ggmp_level6_train[p,])
          ggmp_level6_train[nrow(ggmp_level6_train),1] <- paste0(ggmp_level6_train[nrow(ggmp_level6_train),1],".",k)
        }
      }
      ggmp_level6_train <- tibble::rownames_to_column(as.data.frame(t(ggmp_level6_train),stringsAsFactors = FALSE),"V0")
  
    #山东
      if (disease[i] != "T2D") 
      {
        pos <- which(sgmp_metadata[,i] == "y")
      }else 
      {
        pos <- which(sgmp_metadata[,i] == "n")
      }
      id <- sgmp_metadata[pos,1]
      for (k in 2:(nrow(sgmp_metadata)%/%length(pos)-1))
      {
          for (l in 1:length(pos)) 
          {
            sgmp_metadata <- rbind(sgmp_metadata,sgmp_metadata[pos[l],])
            sgmp_metadata[nrow(sgmp_metadata),1] <- paste0(sgmp_metadata[nrow(sgmp_metadata),1],".",k)
            p <- which(sgmp_level6_train[,1] == id[l])
            sgmp_level6_train <- rbind(sgmp_level6_train,sgmp_level6_train[p,])
            sgmp_level6_train[nrow(sgmp_level6_train),1] <- paste0(sgmp_level6_train[nrow(sgmp_level6_train),1],".",k)
          }
      }


    for(j in 1:9)
    {
      #抽样丰度表
      s <- sample(length(sgmp_metadata[,1]),(length(sgmp_metadata[,1])*j) %/% 10,replace = FALSE)
      sgmp_level6_transfer <- filter(sgmp_level6_train,sgmp_level6_train$`Sample ID` %in% sgmp_metadata[s,1])
      sgmp_level6_query <- filter(sgmp_level6_train,sgmp_level6_train$`Sample ID` %in% sgmp_metadata[setdiff(1:nrow(sgmp_metadata),s),1])
    
      sgmp_level6_transfer <- tibble::rownames_to_column(as.data.frame(t(sgmp_level6_transfer),stringsAsFactors = FALSE),"V0")
      sgmp_level6_query <- tibble::rownames_to_column(as.data.frame(t(sgmp_level6_query),stringsAsFactors = FALSE),"V0")

      write_tsv(ggmp_level6_train,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/ggmp_level6_train.tsv"),col_names = FALSE)
      write_tsv(sgmp_level6_query,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/sgmp_level6_query.tsv"),col_names = FALSE)
      write_tsv(sgmp_level6_transfer,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/sgmp_level6_transfer.tsv"),col_names = FALSE)
    
      #制作mapper文件
      sgmp_metadata_transfer <- filter(sgmp_metadata,sgmp_metadata$SampleID %in% sgmp_metadata[s,1])
      sgmp_metadata_query <- filter(sgmp_metadata,sgmp_metadata$SampleID %in% sgmp_metadata[setdiff(1:nrow(sgmp_metadata),s),1])
    
      ggmp_train_mapper <- data.frame(Env = "1",stringsAsFactors = FALSE)
      sgmp_transfer_mapper <- data.frame(Env = "1",stringsAsFactors = FALSE)
      sgmp_query_mapper <- data.frame(Env = "1",stringsAsFactors = FALSE)
    
      for (k in 1:nrow(ggmp_metadata)) 
      {
        if(ggmp_metadata[,disease[i]][k] == "n")
        {
          ggmp_train_mapper <- rbind(ggmp_train_mapper,paste0(k-1,",","root:healthy",",",ggmp_metadata[k,1]))
        }else
        {
          ggmp_train_mapper <- rbind(ggmp_train_mapper,paste0(k-1,",","root:",disease[i],",",ggmp_metadata[k,1]))
        }
      }
      for (k in 1:nrow(sgmp_metadata_transfer)) 
      {
        if(sgmp_metadata_transfer[,disease[i]][k] == "n")
        {
          sgmp_transfer_mapper <- rbind(sgmp_transfer_mapper,paste0(k-1,",","root:healthy",",",sgmp_metadata_transfer[k,1]))
        }else
        {
          sgmp_transfer_mapper <- rbind(sgmp_transfer_mapper,paste0(k-1,",","root:",disease[i],",",sgmp_metadata_transfer[k,1]))
        }
      }
      for (k in 1:nrow(sgmp_metadata_query)) 
      {
        if(sgmp_metadata_query[,disease[i]][k] == "n")
        {
          sgmp_query_mapper <- rbind(sgmp_query_mapper,paste0(k-1,",","root:healthy",",",sgmp_metadata_query[k,1]))
        }else
        {
          sgmp_query_mapper <- rbind(sgmp_query_mapper,paste0(k-1,",","root:",disease[i],",",sgmp_metadata_query[k,1]))
        }
      }
    
      ggmp_train_mapper <- as.data.frame(as_tibble(ggmp_train_mapper[-1,]))
      sgmp_transfer_mapper <- as.data.frame(as_tibble(sgmp_transfer_mapper[-1,]))
      sgmp_query_mapper <- as.data.frame(as_tibble(sgmp_query_mapper[-1,]))
      colnames(ggmp_train_mapper) <- ",Env,SampleID"
      colnames(sgmp_transfer_mapper) <- ",Env,SampleID"
      colnames(sgmp_query_mapper) <- ",Env,SampleID"
    
      write_tsv(ggmp_train_mapper,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/ggmp_train_mapper.tsv"))
      write_tsv(sgmp_transfer_mapper,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/sgmp_transfer_mapper.tsv"))
      write_tsv(sgmp_query_mapper,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/sgmp_query_mapper.tsv"))
    
      #制作biome文件
      biome <- tibble(biome = 1)
      biome <- rbind(biome,"root:healthy")
      biome <- rbind(biome,paste0("root:",disease[i]))
      biome <- biome[-1,]
      write_tsv(biome,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/biome.tsv"),col_names = FALSE)
    
      #制作路径文件
      path_ggmp_train <- tibble(path_ggmp_train = 1)
      path_sgmp_transfer <- tibble(path_sgmp_transfer = 1)
      path_sgmp_query <- tibble(path_sgmp_query = 1)
    
      path_ggmp_train <- rbind(path_ggmp_train,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/ggmp_level6_train.tsv"))
      path_sgmp_query <- rbind(path_sgmp_query,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/sgmp_level6_query.tsv"))
      path_sgmp_transfer<- rbind(path_sgmp_transfer,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/sgmp_level6_transfer.tsv"))
    
      path_ggmp_train <- path_ggmp_train[-1,]
      path_sgmp_query <- path_sgmp_query[-1,]
      path_sgmp_transfer <- path_sgmp_transfer[-1,]
    
      write_tsv(path_ggmp_train,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/path_ggmp_train.tsv"),col_names = FALSE)
      write_tsv(path_sgmp_query,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/path_sgmp_query.tsv"),col_names = FALSE)
      write_tsv(path_sgmp_transfer,paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/path_sgmp_transfer.tsv"),col_names = FALSE)
    }
  }
}

#将整体的AUROC可视化
  disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Rheumatoid_arthritis","T2D")
  sum <- data.frame(method = "1" ,value = "2" ,group = "3",stringsAsFactors = FALSE)
  for(n in 1:5)
  {
    for(i in 2:length(disease))
    {
      for(j in 1:9)
      {

          a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/Evaluation_Independent/overall.csv"))
          b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/Evaluation_Independent_sd/overall.csv"))
          c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/Evaluation_Transfer/overall.csv"))

        sum <- rbind(sum,c(disease[i],as.character(a[1,])[2],"GGMP_model"))
        sum <- rbind(sum,c(disease[i],as.character(a[2,])[2],"GGMP_model"))
        sum <- rbind(sum,c(disease[i],as.character(b[1,])[2],"SGMP_model"))
        sum <- rbind(sum,c(disease[i],as.character(b[2,])[2],"SGMP_model"))
        sum <- rbind(sum,c(disease[i],as.character(c[1,])[2],"Transfer"))
        sum <- rbind(sum,c(disease[i],as.character(c[2,])[2],"Transfer"))
      }
    }
  }
  sum <- sum[-1,]
  sum[,2] <- as.numeric(sum[,2])
  
  compaired <- list(c("GGMP_model", "SGMP_model"),c("SGMP_model","Transfer"),c("GGMP_model","Transfer"))
  q <- ggplot(sum, aes(x = group,y = value)) +
    geom_boxplot(aes(fill = group),position=position_dodge(0.7),width = 0.3) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 15,colour = "black"),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 15,colour = "black"),
          legend.text = element_text(size = 15,colour = "black"),
          legend.title = element_text(size = 17,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20,colour = "black",face = "bold")) +
          geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = F,test = t.test)+
          scale_fill_npg()+
          facet_grid(~method,scales = "free")+
          ylab("AUROC")+
    labs(title = "AUROC of different models in inter provincial prediction",fill = "Model")
  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/province/figure 1.pdf"),q,width = 20,height = 8)


#画出来样本量和准确性关系图
  disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
  sum <- data.frame(method = "1" ,value = "2" ,group = "3",stringsAsFactors = FALSE)
  for(i in 2:length(disease))
  {
    for(n in 1:5)
    {
      for(j in 1:9)
      {
          a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/Evaluation_Independent/overall.csv"))
          b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/Evaluation_Independent_sd/overall.csv"))
          c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_province/exp",n,"/",disease[i],"/exp",j,"/Evaluation_Transfer/overall.csv"))
          sum <- rbind(sum,c(j/10,mean(a[,2]),"GGMP_model"))
          sum <- rbind(sum,c(j/10,mean(b[,2]),"SGMP_model"))
          sum <- rbind(sum,c(j/10,mean(c[,2]),"Transfer"))
      }
    }
    sum <- sum[-1,]
    sum[,2] <- as.numeric(sum[,2])

    q <- ggplot(sum,aes(x = method,y = value,colour = group)) +
      geom_boxplot(position=position_dodge(0.6),width = 0.3) +
      #geom_point(aes(group = group,color = group)) +
      stat_summary(fun=median, geom="line", aes(group = group)) +
      stat_summary(fun=median, geom="point", aes(group = group)) +
      theme_classic()+
      theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size =15,colour = "black"),
          axis.title.y = element_text(size = 15,colour = "black"),
          axis.text.y = element_text(size = 15,colour = "black"),
          axis.title = element_text(size = 15,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20,colour = "black",face = "bold"),
          legend.text = element_text(size = 15,colour = "black"),
          legend.title = element_blank(),
          legend.position = "top") +
          scale_fill_npg() + 
          ylab("AUROC") +
    labs( title = disease[i])  
    ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/relevance/",disease[i],".pdf"),q,height = 8,width = 8)
  }



#与随机森林进行比较
for(i in 2:length(disease)) 
{
  #预处理
      #读取山东的metadata
      sgmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/sgmp/SGMP_metadata_700_selected.xlsx")
      colnames(sgmp_metadata)[which(colnames(sgmp_metadata)=="host_status")]= "T2D"
      #读取广东的metadata
      ggmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/GGMP.metadata.orignial.xlsx")
      colnames(ggmp_metadata)[which(colnames(ggmp_metadata)=="T2DM")]= "T2D"
      ggmp_metadata <- ggmp_metadata[,disease]
      sgmp_metadata <- sgmp_metadata[,disease]

      #读取广东的level6的丰度文件
      ggmp_level6 <- read_tsv("/home/wangnan/GGMP-SGMP/data/ggmp/taxonomy-report/table-filtered-feature-rarefied5k-L6.tsv")
      #读取山东的level6的丰度文件
      sgmp_level6 <- read_tsv("/home/wangnan/GGMP-SGMP/data/sgmp/taxonomy-report/table-filtered-feature-rarefied5k-L6.tsv")

  #训练模型，进行预测
      #认为metadata中的NA都是健康人
        ggmp_metadata[is.na(ggmp_metadata)] <- "n"
        sgmp_metadata[is.na(sgmp_metadata)] <- "n"
        sgmp_metadata$T2D[which(sgmp_metadata$T2D == "Type 2 diabetes")] <- "y"
        sgmp_metadata$T2D[which(sgmp_metadata$T2D == "Health")] <- "n"

      #选取出在metadata和level6中都用到的样本
        ggmp_inter <- intersect(ggmp_metadata[,1],(ggmp_level6 %>% pull(`Sample ID`)))
        ggmp_level6_train <- filter(ggmp_level6,ggmp_level6$`Sample ID` %in% ggmp_metadata[,1])
        ggmp_metadata <- filter(ggmp_metadata,ggmp_metadata$`SampleID` %in% ggmp_inter)
        sgmp_level6_train <- filter(sgmp_level6,sgmp_level6$`Sample ID` %in% sgmp_metadata[,1])

      #挑出来绝对健康患病的样本
        #广东
          pos <- c()
          for (k in 1:nrow(ggmp_metadata)) 
          {
            if(all(ggmp_metadata[k,2:ncol(ggmp_metadata)] == "n")|ggmp_metadata[k,i] == "y")
            {
              pos <- c(pos,k)
            }
          }
          ggmp_metadata <- ggmp_metadata[pos,]
        #山东
          pos <- c()
          for (k in 1:nrow(sgmp_metadata)) 
          {
            if(all(sgmp_metadata[k,2:ncol(sgmp_metadata)] == "n")|sgmp_metadata[k,i] == "y")
            {
              pos <- c(pos,k)
            }
          }
          sgmp_metadata <- sgmp_metadata[pos,]
          ggmp_level6_train <- filter(ggmp_level6_train,ggmp_level6_train$`Sample ID` %in% ggmp_metadata[,1])
          sgmp_level6_train <- filter(sgmp_level6_train,sgmp_level6_train$`Sample ID` %in% sgmp_metadata[,1])

      #添加每个样本的表型  
        ggmp <- as.data.frame(ggmp_level6_train)  
        sgmp <- as.data.frame(sgmp_level6_train)  
          #ggmp和sgmp中没有的列要补齐
            inter <- intersect(colnames(ggmp),colnames(sgmp))
            pos1 <- which(!(colnames(sgmp) %in% inter))
            pos2 <- which(!(colnames(ggmp) %in% inter))
            ggmp <- rbind.fill(ggmp,sgmp[pos1])
            sgmp <- rbind.fill(sgmp,ggmp[pos2])
            ggmp[is.na(ggmp)] <- 0
            sgmp[is.na(sgmp)] <- 0
            ggmp <- ggmp[1:nrow(ggmp_level6_train),]
            sgmp <- sgmp[1:nrow(sgmp_level6_train),]
        ggmp$phenotype <- "1"
        sgmp$phenotype <- "1"
        for (j in 1:nrow(ggmp)) 
        {
           pos = which(ggmp_metadata[,1] == ggmp[j,1])
           ggmp[j,ncol(ggmp)] =  ggmp_metadata[pos,i]
        }
        for (j in 1:nrow(sgmp)) 
        {
           pos = which(sgmp_metadata[,1] == sgmp[j,1])
           sgmp[j,ncol(sgmp)] =  sgmp_metadata[pos,i]
        }

      #样本总体抽样，分别进行内部检验
        s1 <- sample(length(ggmp_metadata[,1]),(length(ggmp_metadata[,1])*2) %/% 10,replace = FALSE)
        ggmp_test <- filter(ggmp,ggmp$`Sample ID` %in% ggmp_metadata[s1,1])
        ggmp_train <- filter(ggmp,ggmp$`Sample ID` %in% ggmp_metadata[setdiff(1:nrow(ggmp_metadata),s1),1])
        s2 <- sample(length(sgmp_metadata[,1]),(length(sgmp_metadata[,1])*2) %/% 10,replace = FALSE)
        sgmp_test <- filter(sgmp,sgmp$`Sample ID` %in% sgmp_metadata[s2,1])
        sgmp_train <- filter(sgmp,sgmp$`Sample ID` %in% sgmp_metadata[setdiff(1:nrow(sgmp_metadata),s2),1])

        ggmp <- ggmp[,-1]
        ggmp_test <- ggmp_test[,-1]
        ggmp_train <- ggmp_train[,-1]
        sgmp <- sgmp[,-1]
        sgmp_test <- sgmp_test[,-1]
        sgmp_train <- sgmp_train[,-1]

      #模型训练
        #广东模型预测山东队列
          names(ggmp) <- make.names(names(ggmp))
          names(sgmp) <- make.names(names(sgmp))
          ggmp$phenotype <- factor(ggmp$phenotype)
          sgmp$phenotype <- factor(sgmp$phenotype)
          ggmp.rf <- randomForest(phenotype ~ ., data=ggmp, importance=TRUE, proximity=TRUE)
          #保存训练的随机森林模型  
            #save(ggmp.rf,file = "/home/wangnan/GGMP-SGMP/rcode/GGMPmodel.RData")
          sgmp.pr <- predict(ggmp.rf,sgmp[,1:(ncol(sgmp)-1)])
          sgmp.pr <- cbind(as.data.frame(sgmp.pr),as.data.frame(sgmp[,ncol(sgmp)]))
          colnames(sgmp.pr) <- c("prediction","phenotype")
          #保存预测结果
            write.csv(sgmp.pr,paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/province/randomforest/",disease[i],"_ggmp-sgmp.csv"))

        #广东模型自己检验
          names(ggmp_train) <- make.names(names(ggmp_train))
          names(ggmp_test) <- make.names(names(ggmp_test))
          ggmp_train$phenotype <- factor(ggmp_train$phenotype)
          ggmp_test$phenotype <- factor(ggmp_test$phenotype)
          ggmp.rf <- randomForest(phenotype ~ ., data=ggmp_train, importance=TRUE, proximity=TRUE)
          #保存训练的随机森林模型  
            #save(ggmp.rf,file = "/home/wangnan/GGMP-SGMP/rcode/GGMPmodel.RData")
          ggmp.pr <- predict(ggmp.rf,ggmp_test[,1:(ncol(ggmp_test)-1)])
          ggmp.pr <- cbind(as.data.frame(ggmp.pr),as.data.frame(ggmp_test[,ncol(ggmp_test)]))
          colnames(ggmp.pr) <- c("prediction","phenotype")
          #保存预测结果
            write.csv(ggmp.pr,paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/province/randomforest/",disease[i],"_ggmp-validation.csv"))

        #山东模型自己检验
          names(sgmp_train) <- make.names(names(sgmp_train))
          names(sgmp_test) <- make.names(names(sgmp_test))
          sgmp_train$phenotype <- factor(sgmp_train$phenotype)
          sgmp_test$phenotype <- factor(sgmp_test$phenotype)
          sgmp.rf <- randomForest(phenotype ~ ., data=sgmp_train, importance=TRUE, proximity=TRUE)
          #保存训练的随机森林模型  
            #save(ggmp.rf,file = "/home/wangnan/GGMP-SGMP/rcode/GGMPmodel.RData")
          sgmp.pr <- predict(sgmp.rf,sgmp_test[,1:(ncol(sgmp_test)-1)])
          sgmp.pr <- cbind(as.data.frame(sgmp.pr),as.data.frame(sgmp_test[,ncol(sgmp_test)]))
          colnames(sgmp.pr) <- c("prediction","phenotype")
          #保存预测结果
            write.csv(sgmp.pr,paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/province/randomforest/",disease[i],"_sgmp-validation.csv"))  
} 

#广东省不同市级之间的迁移
  disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
  city <- read.xlsx("/home/wangnan/GGMP-SGMP/data/Guangzhou prefecture level city.xlsx")

  #创建文件夹
    #不同疾病命名的文件
      for(i in 2:length(disease))
      {
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i]))
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data"))
        dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/exp"))
      }
    #按照每个疾病，对不同城市的样本进行抽样，放入一个文件夹中，后续不用重复抽样，并且能保证每次使用样本是相同的
      for(i in 2:length(disease))
      {
        for(j in 1:nrow(city))
        {
          dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2]))
        }
      }
    #创建用来实验的文件夹
      for(i in 2:length(disease))
      {
        for(j in 1:nrow(city))
        {
          dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/exp/",city[j,2]))
          for (k in 1:nrow(city)) 
          {
             if (j != k) 
             {
                dir.create(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/exp/",city[j,2],"/",city[k,2]))
             }
          }
        }
      }

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
    
    #先选出各地区的样本，然后选出来绝对健康和患病的样本
      for(i in 2:length(disease)) 
      {
        for (j in 1:nrow(city)) 
        {
          #地区总样本
          pos <- grep(city[j,1],ggmp_metadata[,1])
          metadata <- ggmp_metadata[pos,]
          genus <- filter(ggmp_level6_train,ggmp_level6_train$`Sample ID` %in% metadata[,1])

          #绝对健康的人和患病的样本
          pos <- c()
          for (k in 1:nrow(metadata)) 
          {
            if(all(metadata[k,2:ncol(metadata)] == "n")|metadata[k,i] == "y")
            {
              pos <- c(pos,k)
            }
          }
          metadata <- metadata[pos,]
          genus <- filter(genus,genus$`Sample ID` %in% metadata[,1])
          
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

#画出总体的AUROC,并画出不同城市的AUROC
    disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
    sum <- data.frame(method = "1" ,value = "2" ,group = "3",stringsAsFactors = FALSE)
    for (i in 2:length(disease)) 
    {
      for (j in 1:nrow(city)) 
      {
        a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/data/",city[j,2],"/Evaluation_Independent/overall.csv"))
        sum <- rbind(sum,c(city[j,2],as.character(a[1,])[2],"Independent"))
        sum <- rbind(sum,c(city[j,2],as.character(a[2,])[2],"Independent"))
        for (k in 1:nrow(city)) 
        {
          if (k != j ) 
          {
            b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/exp/",city[j,2],"/",city[k,2],"/Evaluation_Transfer/overall.csv"))
            sum <- rbind(sum,c(city[j,2],as.character(b[1,])[2],"Transfer"))
            sum <- rbind(sum,c(city[j,2],as.character(b[2,])[2],"Transfer"))
            c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/exp_city/",disease[i],"/exp/",city[j,2],"/",city[k,2],"/Evaluation_Independent/overall.csv"))
            sum <- rbind(sum,c(city[j,2],as.character(c[1,])[2],"Validation"))
            sum <- rbind(sum,c(city[j,2],as.character(c[2,])[2],"Validation"))

          }
        }
      }
    }
    sum <- sum[-1,]
    sum[,2] <- as.numeric(sum[,2])

    q <- ggplot(sum, aes(x = group,y = value)) +
    geom_boxplot(aes(fill = group),position=position_dodge(0.7),width = 0.3) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 15,colour = "black"),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 15,colour = "black"),
          legend.text = element_text(size = 15,colour = "black"),
          legend.title = element_text(size = 17,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20,colour = "black",face = "bold")) +
          scale_fill_npg()+
          #facet_grid(~method,scales = "free")+
          ylab("AUROC")+
    labs(title = "Overall AUROC",fill = "Model")

    p <- ggplot(sum, aes(x = group,y = value)) +
    geom_boxplot(aes(fill = group),position=position_dodge(0.7),width = 0.3) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 15,colour = "black"),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 15,colour = "black"),
          legend.text = element_text(size = 15,colour = "black"),
          legend.title = element_text(size = 17,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20,colour = "black",face = "bold")) +
          scale_fill_npg()+
          facet_grid(~method,scales = "free")+
          ylab("AUROC")+
    labs(title = "Model accuracy in medium geographic region ",fill = "Model")

  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city/All AUROC.pdf"),q,width = 6,height =7)
  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city/cities.pdf"),p,width = 20)

#与随机森林的比较
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
    
    #先选出各地区的样本，然后选出来绝对健康和患病的样本
      for(i in 2:length(disease)) 
      {
        for (j in 1:nrow(city)) 
        {
          #构建模型
            #总样本
              pos <- grep(city[j,1],ggmp_metadata[,1])
              metadata <- ggmp_metadata[pos,]
              genus <- filter(ggmp_level6_train,ggmp_level6_train$`Sample ID` %in% metadata[,1])
            #挑出绝对健康的人和患病的样本
              pos <- c()
              for (k in 1:nrow(metadata)) 
              {
                if(all(metadata[k,2:ncol(metadata)] == "n")|metadata[k,i] == "y")
                {
                  pos <- c(pos,k)
                }
              }
              metadata <- metadata[pos,]
              genus <- filter(genus,genus$`Sample ID` %in% metadata[,1])
            #添加每个样本的表型
              genus <- as.data.frame(genus)  
              genus$phenotype <- "1"
              for (l in 1:nrow(genus)) 
              {
                pos = which(metadata[,1] == genus[l,1])
                genus[l,ncol(genus)] =  metadata[pos,i]
              }
              genus <- genus[,-1]
            #训练模型
              genus$phenotype <- factor(genus$phenotype)
              names(genus) <- make.names(names(genus))
              model <- randomForest(phenotype ~ ., data=genus, importance=TRUE, proximity=TRUE)
            #自我检验
              #按照8:2的比例抽样
                s <- sample(nrow(genus),(nrow(genus)*2) %/% 10,replace = FALSE)
                train <- genus[setdiff(1:nrow(genus),s),]
                test <- genus[s,]
                names(train) <- make.names(names(train))
                names(test) <- make.names(names(test))

              #训练模型并进行预测
                trainmodel <- randomForest(phenotype ~ ., data=train, importance=TRUE, proximity=TRUE)
                pr1 <- predict(trainmodel,test[,1:(ncol(test)-1)])
                pr1 <- cbind(as.data.frame(pr1),as.data.frame(test[,ncol(test)]))
                colnames(pr1) <- c("prediction","phenotype")

          #用来分类的样本
            for (k in 1:nrow(city)) 
            {
              if (j != k) 
              {
                #总样本
                  pos <- grep(city[k,1],ggmp_metadata[,1])
                  metadata <- ggmp_metadata[pos,]
                  genus <- filter(ggmp_level6_train,ggmp_level6_train$`Sample ID` %in% metadata[,1])
                #挑出绝对健康的人和患病的样本
                  pos <- c()
                  for (l in 1:nrow(metadata)) 
                  {
                    if(all(metadata[l,2:ncol(metadata)] == "n")|metadata[l,i] == "y")
                    {
                    pos <- c(pos,l)
                    }
                  }
                  metadata <- metadata[pos,]
                  genus <- filter(genus,genus$`Sample ID` %in% metadata[,1])
                #添加每个样本的表型  
                  test <- as.data.frame(genus)  
                  test$phenotype <- "1"
                  for (l in 1:nrow(test)) 
                  {
                    pos = which(metadata[,1] == test[l,1])
                    test[l,ncol(test)] =  metadata[pos,i]
                  }
                  test <- test[,-1]
                  test$phenotype <- factor(test$phenotype)
                #模型预测
                  names(test) <- make.names(names(test))
                  pr2 <- predict(model,test[,1:(ncol(test)-1)])
                  pr2 <- cbind(as.data.frame(pr2),as.data.frame(test[,ncol(test)]))
                  colnames(pr2) <- c("prediction","phenotype")
                #保存预测结果
                write.csv(pr1,paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city/randomforest/",disease[i],"/",city[j,2],"/",city[k,2],"/internal_validation.csv"))  
                write.csv(pr2,paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city/randomforest/",disease[i],"/",city[j,2],"/",city[k,2],"/external_validation.csv"))  
              }
            }
          }
        }
      }

#
#############
#对CRC的结果验证(自己的结果)
  sum <- data.frame(method = "1" ,value = 2 ,group = "3",stringsAsFactors = FALSE)
  for(i in 0:4)
  {
    a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_CRC/exp/exp",i,"/Evaluation_Independent/overall.csv"),stringsAsFactors = FALSE)
    b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_CRC/exp/exp",i,"/Evaluation_Transfer_HM/overall.csv"),stringsAsFactors = FALSE)
    c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_CRC/exp/exp",i,"/Evaluation_Transfer_DM/overall.csv"),stringsAsFactors = FALSE)
    a <- cbind(a,"Independent")
    b <- cbind(b,"Transfer_HM")
    c <- cbind(c,"Transfer_DM")
    for (j in 1:nrow(a)) 
    {
      sum <- rbind(sum,c(a[j,1],a[j,2],as.character(a[j,4])))
      sum <- rbind(sum,c(b[j,1],b[j,2],as.character(b[j,4])))
      sum <- rbind(sum,c(c[j,1],c[j,2],as.character(c[j,4])))
    }
  }
  sum <- sum[-1,]
  sum[,2] <- as.numeric(sum[,2])

  sum <- subset(sum,sum$value>0)
  p <- ggplot(sum, aes(x = group,y = value,fill = group)) +
    geom_violin() +
    geom_boxplot(width=0.1)+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size =15,colour = "black"),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 15,colour = "black"),
          axis.title = element_text(size = 15,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))+
          scale_fill_npg()
  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/","CRC_ROAUC_validation_sum",".pdf"),p)

  sum <- subset(sum,sum$value>0)
  q <- ggplot(sum, aes(x = group,y = value)) +
    geom_boxplot(aes(fill = method)) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size =15,colour = "black"),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 15,colour = "black"),
          axis.title = element_text(size = 15,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))+
          scale_fill_npg()
  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/","CRC_ROAUC_validation",".pdf"),q)







#对CRC的结果验证(冲辉的结果)
  sum <- data.frame(method = "1" ,value = 2 ,group = "3",stringsAsFactors = FALSE)
  for(i in 0:4)
  {
    a <- read.csv(paste0("/data2/public/chonghui/EXPERT-use-cases/repr/CRC-progression/experiments/exp_",i,"/Eval_Independent/.ipynb_checkpoints/overall-checkpoint.csv"),stringsAsFactors = FALSE)
    b <- read.csv(paste0("/data2/public/chonghui/EXPERT-use-cases/repr/CRC-progression/experiments/exp_",i,"/Eval_Transfer_HM/.ipynb_checkpoints/overall-checkpoint.csv"),stringsAsFactors = FALSE)
    c <- read.csv(paste0("/data2/public/chonghui/EXPERT-use-cases/repr/CRC-progression/experiments/exp_",i,"/Eval_Transfer_DM/.ipynb_checkpoints/overall-checkpoint.csv"),stringsAsFactors = FALSE)
    a <- cbind(a,"Independent")
    b <- cbind(b,"Transfer_HM")
    c <- cbind(c,"Transfer_DM")
    for (j in 1:nrow(a)) 
    {
      sum <- rbind(sum,c(a[j,1],a[j,2],as.character(a[j,4])))
      sum <- rbind(sum,c(b[j,1],b[j,2],as.character(b[j,4])))
      sum <- rbind(sum,c(c[j,1],c[j,2],as.character(c[j,4])))
    }
  }
  sum <- sum[-1,]
  sum[,2] <- as.numeric(sum[,2])

  q <- ggplot(sum, aes(x = group,y = value)) +
    geom_boxplot(aes(fill = method)) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size =15,colour = "black"),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 15,colour = "black"),
          axis.title = element_text(size = 15,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/","CRC_ROAUC_origin",".pdf"),q)
#############

