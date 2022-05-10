library(tidyverse)
library(tidyr)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggsignif)  
library(ggsci)
library(randomForest)
    
    disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
    city <- read.xlsx("/home/wangnan/GGMP-SGMP/data/Guangzhou prefecture level city.xlsx")
    sum <- data.frame(method = "1" ,value = "2" ,group = "3",stringsAsFactors = FALSE)
    for (n in 4) 
    {
        for (i in 2:length(disease)) 
        {
            for (j in 1:nrow(city)) 
            {
                a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp",n,"/",disease[i],"/data/",city[j,2],"/Evaluation_Independent/overall.csv"))
                sum <- rbind(sum,c(city[j,2],as.character(a[1,])[2],"Independent model"))
                sum <- rbind(sum,c(city[j,2],as.character(a[2,])[2],"Independent model"))
                for (k in 1:nrow(city)) 
                {
                    if (k != j ) 
                    {
                        b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp",n,"/",disease[i],"/exp/",city[j,2],"/",city[k,2],"/Evaluation_Transfer/overall.csv"))
                        sum <- rbind(sum,c(city[j,2],as.character(b[1,])[2],"Transfer model"))
                        sum <- rbind(sum,c(city[j,2],as.character(b[2,])[2],"Transfer model"))
                        c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp",n,"/",disease[i],"/exp/",city[j,2],"/",city[k,2],"/Evaluation_Independent/overall.csv"))
                        sum <- rbind(sum,c(city[j,2],as.character(c[1,])[2],"Regional model"))
                        sum <- rbind(sum,c(city[j,2],as.character(c[2,])[2],"Regional model"))

                    }
                }
            }
        }
    }
    sum <- sum[-1,]
    sum[,2] <- as.numeric(sum[,2])
    #替换地域名,按照地区命名即可
        sum[sum == "Guangzhou(Yuexiu)"] = "Yuexiu"
        sum[sum == "Shaoguan(Qujiang)"] = "Qujiang"
        sum[sum == "Shaoguan(Nanxiong)"] = "Nanxiong"
        sum[sum == "Shenzhen(Nanshan)"] = "Nanshan"
        sum[sum == "Foshan(Shunde)"] = "Shunde"
        sum[sum == "Zhanjiang(Wuchuan)"] = "Wuchuan"
        sum[sum == "Maoming(Gaozhou)"] = "Gaozhou"
        sum[sum == "Zhaoqing(Sihui)"] = "Sihui"
        sum[sum == "Huizhou(Huiyang)"] = "Huiyang"
        sum[sum == "Meizhou(Wuhua)"] = "Wuhua"
        sum[sum == "Shanwei"] = "Shanwei"
        sum[sum == "Qingyuan(Qingcheng)"] = "Qingcheng"
        sum[sum == "Jieyang(Huilai)"] = "Huilai"
        sum[sum == "Yunfu(Yuncheng)"] = "Yuncheng"
    #更换分组名称
        sum[sum == "Independent model"] = "Independent assessment"
        sum[sum == "Regional model"] = "Regional assessment"
        sum[sum == "Transfer model"] = "Transfer assessment"
    aggregate(sum$value,by=list(type = sum$group),mean) #分组计算AUROC

    #设置分组
        comparision <- list(c("Independent assessment","Regional assessment"),c("Transfer assessment","Regional assessment"),c("Independent assessment","Transfer assessment"))
    

    q <- ggplot(sum, aes(x = group,y = value,color = group)) +
        geom_boxplot(aes(fill = group),position=position_dodge(0.7),width = 0.6) +
        scale_color_manual(values=c("#E64B35", "#4DBBD5", "#00A087"),aesthetics = "fill")+ #箱线图的外框指定颜色
        scale_color_manual(values=c("#000000", "#000000", "#000000"),aesthetics = "color")+ #箱线图的填充指定颜色
        theme_bw()+
        theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 12,colour = "black"),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 12,colour = "black"),
            #legend.text = element_text(size = 12,colour = "black"),
            legend.position= "top",
            legend.title = element_blank(),
            #legend.title = element_text(size = 12,colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5,size = 15,colour = "black")) +
            #scale_fill_npg()+
            geom_signif(comparisons = comparision,step_increase = 0.1,map_signif_level = T,test = wilcox.test)+
            #stat_compare_means(label = "p.format")+
            #facet_grid(~method,scales = "free")+
            ylab("AUROC")
        #labs(title = "Overall AUROC",fill = "Model")

    p <- ggplot(sum, aes(x = group,y = value)) +
    geom_boxplot(aes(fill = group),position=position_dodge(0.7),width = 0.7) +
    scale_color_manual(values=c("#E64B35", "#4DBBD5", "#00A087"),aesthetics = "fill")+
    scale_color_manual(values=c("#000000", "#000000", "#000000"),aesthetics = "color")+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          #axis.title.y = element_text(size = 12,colour = "black"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12,colour = "black"),
          legend.text = element_text(size = 12,colour = "black"),
          #legend.title = element_text(size = 12,colour = "black"),
          legend.title = element_blank(),
          legend.position = "top",
          strip.text.x = element_text(size = 12,colour = "black"),#修改facet_grid的标题的大小
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 15,colour = "black")) +
          #scale_fill_npg()+
          geom_signif(comparisons = comparision,step_increase = 0.1,map_signif_level = T,test = wilcox.test)+
          facet_grid(~method,scales = "free")
          #ylab("AUROC")+
    #labs(title = "Transfer learning among cities",fill = "Model")

  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city/All AUROC.pdf"),q,width = 3,height =6)
  ggsave(paste0("/home/wangnan/GGMP-SGMP/figure/exp_region/city/cities.pdf"),p,width = 13,height = 6)


