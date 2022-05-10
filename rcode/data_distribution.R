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
library(RColorBrewer)
#广东队列和山东队列在不同的疾病的健康人数和患病人数
    #读取山东的metadata
    sgmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/sgmp/SGMP_metadata_700_selected.xlsx")
    colnames(sgmp_metadata)[which(colnames(sgmp_metadata)=="host_status")]= "T2D"
    #读取广东的metadata
    ggmp_metadata <- read.xlsx("/home/wangnan/GGMP-SGMP/data/GGMP.metadata.orignial.xlsx")
    colnames(ggmp_metadata)[which(colnames(ggmp_metadata)=="T2DM")]= "T2D"

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


    #统计广东和山东队列患病的人数
        #选出来能够直接用"n"和"y"来表示的疾病
        #广东
            gd <- data.frame(disease = "1",case = 2,stringsAsFactors = FALSE)
            disease <- c()
            for (i in 1:ncol(ggmp_metadata)) 
            {
                if (ggmp_metadata[1,i]=="n" | ggmp_metadata[1,i]=="y") 
                {
                disease <- c(disease,colnames(ggmp_metadata)[i])
                }
            }
            
            ggmp_metadata <- ggmp_metadata[,disease]
            #统计
            for (i in 1:ncol(ggmp_metadata)) 
            {
                num <- 0
                for (j in 1:nrow(ggmp_metadata)) 
                {
                    if(ggmp_metadata[j,i] == "y")
                {
                   num = num +1
                }
            }
            gd <- rbind(gd,c(colnames(ggmp_metadata)[i],num))
            }
            gd[,2] <- as.numeric(gd[,2])
            gd <- gd[-1,]
            gd <- gd[order(-gd[,2]),]

        #山东
            sd <- data.frame(disease = "1",case = 2,stringsAsFactors = FALSE)
            disease <- c()
            for (i in 1:ncol(sgmp_metadata)) 
            {
                if (sgmp_metadata[1,i]=="n" | sgmp_metadata[1,i]=="y") 
                {
                disease <- c(disease,colnames(sgmp_metadata)[i])
                }
            }
            sgmp_metadata <- sgmp_metadata[,disease]
            #统计
            for (i in 1:ncol(sgmp_metadata)) 
            {
                num <- 0
                for (j in 1:nrow(sgmp_metadata)) 
                {
                if(sgmp_metadata[j,i] == "y")
                {
                    num = num +1
                }
            }  
                sd <- rbind(sd,c(colnames(sgmp_metadata)[i],num))
            }
            sd[,2] <- as.numeric(sd[,2])
            sd <- sd[-1,]
            sd <- sd[(setdiff(1:nrow(sd),which(sd[,2] == 0))),]
            sd <- sd[order(-sd[,2]),]

    #画出广东和山东的统计图
        #指定颜色
            palette <- c(brewer.pal(8,"Set1"),brewer.pal(12,"Paired"),brewer.pal(9,"Set2"))
        g <- ggplot(gd,aes(x=reorder(disease,case),y=case,fill =disease)) + 
            geom_bar(stat = "identity") +
            theme_bw()+
            theme_classic()+
            ylab("Number of cases")+
            theme(axis.title.x = element_text(size =12,colour = "black"),
                axis.text.x = element_text(size =10,colour = "black"),
                axis.text.y = element_text(size =10,colour = "black"),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5,size = 15,colour = "black")) +
            ylab("Number of cases")+
            labs(title = "Guangdong")+
            #scale_fill_manual(values = palette)+
            coord_flip()

        s <- ggplot(sd,aes(x=reorder(disease,case),y=case,fill =disease)) + 
            geom_bar(stat = "identity")+
            theme_bw()+
            theme_classic()+
            ylab("Number of cases")+
            theme(axis.title.x = element_text(size =12,colour = "black"),
                axis.text.x = element_text(size =10,colour = "black"),
                axis.text.y = element_text(size =10,colour = "black"),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5,size = 15,colour = "black")) +
            ylab("Number of cases")+
            labs(title = "Shandong")+
            #scale_fill_manual(values = palette)+
            coord_flip()
        
        figure <- plot_grid(g,s,labels = c("a","b"))
        ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/data distribution.pdf"),figure,width = 8,height = 5)





            
