gene <- read.csv("patch/20200210_Voltage gated ion channel_expr.csv",header = TRUE,row.names = 1)
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#

library(ggpubr)
library(ggsci)
library(ggsignif)
library(car)

gene$group <- factor(gene$group,ordered = T)
#正态性检验#

#方差齐性检验#
leveneTest(CACNA1A~group,gene)
#残差检验正态性和方差齐性#
AOV1 <- aov(CACNA1A~group,gene)
summary(AOV1)




