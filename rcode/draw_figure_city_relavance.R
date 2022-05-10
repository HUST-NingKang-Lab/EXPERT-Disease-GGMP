library(tidyverse)
library(tidyr)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggsignif)  
library(ggsci)


#画出来样本量和准确性关系图
  disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
  city <- read.xlsx("/home/wangnan/GGMP-SGMP/data/Guangzhou prefecture level city.xlsx")

  for(i in 2:length(disease))
  {
    sum <- data.frame(method = "1" ,value = "2" ,group = "3",stringsAsFactors = FALSE)
    for(n in 1:4)
    {
        for (j in 1:nrow(city)) 
        {
            a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp",n,"/",disease[i],"/data/",city[j,2],"/Evaluation_Independent/overall.csv"))
            sum <- rbind(sum,c(paste0(n*20,"%"),as.character(a[1,])[2],"Independent model"))
            sum <- rbind(sum,c(paste0(n*20,"%"),as.character(a[2,])[2],"Independent model"))
            for (k in 1:nrow(city)) 
            {
                if (k != j ) 
                {
                    b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp",n,"/",disease[i],"/exp/",city[j,2],"/",city[k,2],"/Evaluation_Transfer/overall.csv"))
                    sum <- rbind(sum,c(paste0(n*20,"%"),as.character(b[1,])[2],"Transfer model"))
                    sum <- rbind(sum,c(paste0(n*20,"%"),as.character(b[2,])[2],"Transfer model"))
                    c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp",n,"/",disease[i],"/exp/",city[j,2],"/",city[k,2],"/Evaluation_Independent/overall.csv"))
                    sum <- rbind(sum,c(paste0(n*20,"%"),as.character(c[1,])[2],"Regional model"))
                    sum <- rbind(sum,c(paste0(n*20,"%"),as.character(c[2,])[2],"Regional model"))
                }
            }
        }
    }
    sum <- sum[-1,]
    sum[,2] <- as.numeric(sum[,2])
    #更换分组名称
        sum[sum == "Independent model"] = "Independent assessment"
        sum[sum == "Regional model"] = "Regional assessment"
        sum[sum == "Transfer model"] = "Transfer assessment"
        sum$group <- factor(sum$group,levels = c("Independent assessment","Regional assessment","Transfer assessment"))

    q <- ggplot(sum,aes(x = method,y = value,colour = group)) +
      geom_boxplot(position=position_dodge(0.6),width = 0.5) +
      #geom_point(aes(group = group,color = group)) +
      scale_color_manual(values=c("#E64B35", "#4DBBD5", "#00A087"),aesthetics = "color")+
      #scale_color_manual(values=c("#E64B35", "#4DBBD5", "#00A087"),aesthetics = "fill")+ #箱线图的填充指定颜色
      stat_summary(fun=median, geom="line", aes(group = group)) +
      stat_summary(fun=median, geom="point", aes(group = group)) +
      theme_classic()+
      theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size =12,colour = "black"),
          axis.title.y = element_text(size = 12,colour = "black"),
          axis.text.y = element_text(size = 12,colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 15,colour = "black"),
          legend.text = element_text(size = 12,colour = "black"),
          legend.title = element_blank(),
          legend.position = "top") +
          #scale_color_npg() + 
          ylab("AUROC") +
          scale_y_continuous(limits = c(0.2, 1))+ #设置y轴范围
    labs( title = disease[i])  
    ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city_relevance/",disease[i],".pdf"),q,height = 6,width = 6)
  }
