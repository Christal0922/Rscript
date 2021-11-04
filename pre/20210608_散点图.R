Ephys <- read.table("E:/R/PCA/20210608_cell340E2.txt",row.names=1,head=TRUE,sep="\t")
FCD <- Ephys[Ephys$pathology=='FCD2',]
Val <- as.matrix(colnames(FCD))
#可忽略#
length(FCD) #返回FCD数据集的列数#

#具体内容
library(ggplot2)
#测试版#
windowsFonts(myFont = windowsFont("Arial")) 
pdf("E:/R/Morpho2/20210611_variation_2.pdf",width = 4.5,height = 4) 
for (i in 10:11){
  i=10
  p <- ggplot(FCD,aes(x=patient.ID,y=FCD[,i]))+
    geom_point()+
    labs(y=Val[i])+
    theme(axis.title.x = element_text(size=12,family="myFont"))
  p
     print (p)
  i=i+1
}
dev.off()

#不区分细胞类型版#
pdf("E:/R/Morpho2/20210611_variation_all.pdf",width = 4.5,height = 4) 
for (i in 10:length(FCD)){
  p <- ggplot(FCD,aes(x=patient.ID,y=FCD[,i]))+
    geom_point()+
    labs(y=Val[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90,hjust = 1),
          axis.line = element_line(),
          panel.background = element_blank())
  print (p)
  i=i+1
}
dev.off()

#不区分细胞类型，改变坐标轴字体#

#重命名列名
library(plyr)
Val[10:22] <- c("Rheobase(pA)","Resting Potential(mV)",
                "Input resistance(mO)","Cm(pF)",
                "Threshold(mV)","Peak(mV)",
                "Halfwidth(ms)","Ahp(mV)",
                "Latency(ms)","UDR",
                "Instaneous Frequency(Hz)","Frequency(Hz)","Adaptation index")
#改变x轴标签
a <- sort(unique(Ephys$patient.ID))
a[1:8] <- c("2FCDIIA","3FCDIIA","4FCDIIA","5FCDIIA","6FCDIIA",
            "7FCDIIA","8FCDIIA","9FCDIIA")

#作图

library(ggplot2)
pdf("E:/R/Morpho2/20210611_variation_all_Ephys.pdf",width = 4.5,height = 4) 
for (i in 10:length(FCD)){
  p <- ggplot(FCD,aes(x=patient.ID,y=FCD[,i]))+
    geom_point()+
    scale_x_discrete(labels=a)+
    labs(x="",y=Val[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=12,angle = 90,hjust = 1,color="black"),
          axis.text.y = element_text(size=12,vjust = 1,color = "black"),
          axis.title = element_text(size = 20),
          axis.line = element_line(),
          panel.background = element_blank())
  print (p)
  i=i+1
}
dev.off()


#区分细胞类型版#
pdf("E:/R/Morpho2/20210608_variation_cell4.pdf",width = 4.5,height = 4) 
for (i in 10:length(FCD)){
  p <- ggplot(FCD,aes(x=patient.ID,y=FCD[,i],color=cell.type2))+
    geom_point()+
    scale_color_manual(values=c('#c81e50','#001e96','#f08caa','#6482ff'))+
    labs(y=colnames(FCD)[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90,hjust = 1),
          axis.line = element_line(),
          panel.background = element_blank(),
          legend.position = "none")
  print (p)
  i=i+1
}
dev.off()
#cell type 区分DN，PY#
pdf("E:/R/Morpho2/20210608_variation_cell2.pdf",width = 4.5,height = 4) 
for (i in 10:length(FCD)){
  p <- ggplot(FCD,aes(x=patient.ID,y=FCD[,i],color=cell.type))+
    geom_point()+
    scale_color_manual(values=c('#ff0000','#0000ff'))+
    labs(y=colnames(FCD)[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90,hjust = 1),
          axis.line = element_line(),
          panel.background = element_blank(),
          legend.position = "none")
  p
  print (p)
  i=i+1
}
dev.off()





#morphology#
Morpho <- read.table("E:/R/PCA/20210608_cell89M2.txt",row.names=1,head=TRUE,sep="\t")
MFCD <- Morpho[which(Morpho$pathology.1=='FCD'),]
MVal <- as.matrix(colnames(MFCD))
library(ggplot2)
#不区分细胞类型版#
pdf("E:/R/Morpho2/20210611_variation_all_M.pdf",width = 3.5,height = 4) 
for (i in 16:26){
  p <- ggplot(MFCD,aes(x=patient.id,y=MFCD[,i]))+
    geom_point()+
    labs(y=MVal[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90,hjust = 1),
          axis.line = element_line(),
          panel.background = element_blank())
  print (p)
  i=i+1
}
dev.off()

#不区分细胞类型版，改变字体大小#
library(plyr)
MVal[16:26] <- c("Soma Surface Area(um2)","Maximum Branch Order",
                "Contraction","Soma Volume(um3)",
                "Local Angle()","Max Local Angle",
                "Dendrite Quantity","Dendrite Nodes",
                "Dendrite Ends","Dendrite Total Length(um)",
                "Tree Termination Distance(um)")
#改变x轴标签
b <- sort(unique(Morpho$patient.id))
b[1:3] <- c("1FCDIB","3FCDIIA","4FCDIIA")
#作图
attach(MFCD)
library(ggplot2)
pdf("E:/R/Morpho2/20210611_variation_all_Morpho.pdf",width = 4.5,height = 4) 
for (i in 16:26){
  p <- ggplot(MFCD,aes(x=patient.id,y=MFCD[,i]))+
    geom_point()+
    scale_x_discrete(labels=b)+
    labs(x="",y=MVal[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=12,angle = 90,hjust = 1),
          axis.text.y = element_text(size=12,vjust = 1),
          axis.title = element_text(size = 20),
          axis.line = element_line(),
          panel.background = element_blank())
  print (p)
  i=i+1
}
dev.off()


#区分DN，PY#
pdf("E:/R/Morpho2/20210608_variation_DNPY_M.pdf",width = 2,height = 4) 
for (i in 16:26){
  p <- ggplot(MFCD,aes(x=patient.id,y=MFCD[,i],color=MFCD$cell.type))+
    geom_point()+
    scale_color_manual(values = c('#ff0000','#0000ff'))+
    labs(y=MVal[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90,hjust = 1),
          axis.line = element_line(),
          panel.background = element_blank(),
          legend.position = "none")
  print (p)
  i=i+1
}
dev.off()

#区分UL_DN,UL_PY#
pdf("E:/R/Morpho2/20210608_variation_UL_DN_M.pdf",width = 2,height = 4) 
for (i in 16:26){
  p <- ggplot(MFCD,aes(x=patient.id,y=MFCD[,i],color=MFCD$cell.type2))+
    geom_point()+
    scale_color_manual(values = c('#c81e50','#001e96','#f08caa','#6482ff'))+
    labs(y=MVal[i])+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90,hjust = 1),
          axis.line = element_line(),
          panel.background = element_blank(),
          legend.position = "none")
  print (p)
  i=i+1
}
dev.off()



