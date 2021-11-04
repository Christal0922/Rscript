genel <- read.csv("E:/Rtest/Input/20210726_antigen.csv",header = TRUE)
library(ggplot2)
colnames(genel)
ggplot(genel,aes(x=RANK.IN.GENE.LIST,y=RUNNING.ES))+geom_line()+geom_rug(aes(x=RANK.IN.GENE.LIST))