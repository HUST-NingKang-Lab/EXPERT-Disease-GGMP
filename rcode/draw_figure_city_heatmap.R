library(tidyverse)
library(ggplot2)
library(reshape2)

    disease <- c("SampleID","Constipation","COPD","Gastritis","Kidneystone","Metabolic_syndrome","Rheumatoid_arthritis","T2D")
    city <- read.xlsx("/home/wangnan/GGMP-SGMP/data/Guangzhou prefecture level city.xlsx")
    df1 <- as.data.frame(matrix(ncol = 14,nrow =14))
    df2 <- as.data.frame(matrix(ncol = 14,nrow =14))
    colnames(df1) <- city[,2]
    rownames(df1) <- city[,2]
    colnames(df2) <- city[,2]
    rownames(df2) <- city[,2]
    
    for(n in 1:14)
    {
        for(i in 1:14)
        { 
            sum1 <- c()
            sum2 <- c()
            sum3 <- c()
            for(j in 2:length(disease))
            {
                a <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp4/",disease[j],"/data/",colnames(df1)[n],"/Evaluation_Independent/overall.csv"))
                sum1 <- c(sum1,a[,2])
                if(n != i)
                {
                    b <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp4/",disease[j],"/exp/",colnames(df1)[n],"/",colnames(df1)[i],"/Evaluation_Transfer/overall.csv"))
                    c <- read.csv(paste0("/home/wangnan/GGMP-SGMP/exp_region/city_gradient/exp4/",disease[j],"/exp/",colnames(df1)[n],"/",colnames(df1)[i],"/Evaluation_Independent/overall.csv"))
                    sum2 <- c(sum2,b[,2])  
                    sum3 <- c(sum3,c[,2])
                }
            }
            if(length(sum1)>0){sum1 <- round(mean(sum1),2)}
            if(length(sum2)>0){sum2 <- round(mean(sum2),2)}
            if(length(sum3)>0){sum3 <- round(mean(sum3),2)}

            if(n == i)
            {
                df1[i,n] = sum1
                df2[i,n] = sum1
            }else 
            {
                df1[i,n] = sum2
                df2[i,n] = sum3
            }
        }
    }

    #替换地域名,按照地区命名即可
        colnames(df1) <- c("Yuexiu","Qujiang","Nanxiong","Nanshan","Shunde","Wuchuan","Gaozhou","Sihui","Huiyang","Wuhua","Shanwei",
            "Qingcheng","Huilai","Yuncheng")
        rownames(df1) <- c("Yuexiu","Qujiang","Nanxiong","Nanshan","Shunde","Wuchuan","Gaozhou","Sihui","Huiyang","Wuhua","Shanwei",
            "Qingcheng","Huilai","Yuncheng")
        colnames(df2) <- c("Yuexiu","Qujiang","Nanxiong","Nanshan","Shunde","Wuchuan","Gaozhou","Sihui","Huiyang","Wuhua","Shanwei",
            "Qingcheng","Huilai","Yuncheng")
        rownames(df2) <- c("Yuexiu","Qujiang","Nanxiong","Nanshan","Shunde","Wuchuan","Gaozhou","Sihui","Huiyang","Wuhua","Shanwei",
            "Qingcheng","Huilai","Yuncheng")

    melt_df1 <- melt(df1)
    melt_df1[,3] <- rep(colnames(df1),14)
    melt_df1 <- melt_df1[,c(3,1,2)]
    colnames(melt_df1) <- c("v1","v2","AUROC")
    melt_df2 <- melt(df2)
    melt_df2[,3] <- rep(colnames(df2),14)
    melt_df2 <- melt_df2[,c(3,1,2)]
    colnames(melt_df2) <- c("v1","v2","AUROC")

    melt_df1$v1 <- factor(melt_df1$v1,levels = colnames(df1))
    heatmap_transfer <-
        ggplot(melt_df1,aes(x=v2, y=v1, fill=AUROC))+
        geom_tile()+
        geom_text(aes(label = AUROC), color = "white", size = 5)+
        coord_fixed()+
        scale_fill_gradient(low = "#66CCFF", high = "#003366",limits = c(0.4, 1),breaks = c(0.4,0.6,0.8,1.0)) + ###设置热图标尺的颜色、范围以及刻度###
        theme(axis.title.x = element_text(size = 17,colour = "black"),
          axis.text.x = element_text(size = 15,colour = "black",angle =90,vjust = 0.5),
          axis.title.y = element_text(size = 17,colour = "black"),
          axis.text.y = element_text(size = 15,colour = "black"),
          plot.title = element_text(hjust = 0.5,size = 20,colour = "black"),
          legend.title = element_text(size = 15,colour = "black"),
          legend.text = element_text(size = 15,colour = "black")) +
          xlab("Target city")+
          ylab("Source city")+
    labs(title = "AUROC of transfer assessment")

    melt_df2$v1 <- factor(melt_df2$v1,levels = colnames(df2))
    heatmap_regional <- 
        ggplot(melt_df2,aes(x=v1, y=v2, fill=AUROC))+
        geom_tile()+
        geom_text(aes(label = AUROC), color = "white", size = 5)+
        coord_fixed()+
        scale_fill_gradient(low = "#66CCFF", high = "#003366",limits = c(0.4, 1),breaks = c(0.4,0.6,0.8,1.0)) +
        theme(axis.title.x = element_text(size = 17,colour = "black"),
          axis.text.x = element_text(size = 15,colour = "black",angle =90,vjust=0.5),
          axis.title.y = element_text(size = 17,colour = "black"),
          axis.text.y = element_text(size = 15,colour = "black"),
          plot.title = element_text(hjust = 0.5,size = 20,colour = "black"),
          legend.title = element_text(size = 15,colour = "black"),
          legend.text = element_text(size = 15,colour = "black")) +
          xlab("Target city")+
          ylab("Source city")+
    labs(title = "AUROC of regional assessment ")

  ggsave("/home/wangnan/GGMP-SGMP/figure/exp_region/city/city_heatmap_transfer.pdf",heatmap_transfer,width = 10,height =10)
  ggsave("/home/wangnan/GGMP-SGMP/figure/exp_region/city/city_heatmap_regional.pdf",heatmap_regional,width = 10,height =10)


