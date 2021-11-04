Total <- read.table("E:/R/PCA/20201210_89cells.txt",row.names=1,head=TRUE,sep="\t")
Annotation<-Total[,1:11]
morpho <- Total[,12:21]
Ephys <- Total[,22:34]
nmorpho <- apply(morpho,2,scale) #对数据按列进行scale#
rownames(nmorpho) <- rownames(morpho)

morphodata <- prcomp(nmorpho) #计算主成分#
PCimportance <- morphodata$rotation #显示各个PC成分中最主要的参数#
#PC1:dendrite total length, dendritic ends, dendritic nodes; PC2: soma surface area, cell body volume, average contraction#
summ <- summary(morphodata) #多了importance, 计算多个PC的方差，计算累计概率#
d <- round(summ$importance[2,1]*100,2) #取出PC1,PC2占的百分比，取2位小数#

library(ggbiplot)
#改变点的大小#
p<-ggbiplot(morphodata, obs.scale = 1, var.scale = 1,
            groups = Annotation$cell.type3, 
            ellipse = TRUE, epplipse.prob=0.8,circle = FALSE,
            labels.size = 10,varname.size = 5,varname.adjust = 2)+
  geom_point(aes(colour=Annotation$cell.type3),size=2)+scale_color_manual(values=c('#c81e50','#001e96','#f08caa','#6482ff'))+
  theme(panel.grid=element_blank(),legend.direction = 'horizontal',legend.position = 'no')
p
ggsave("E:/R/Morpho2/20210607_PCA_89_celltype.pdf",p,width=5,height=4)

p<-ggbiplot(morphodata, obs.scale = 1, var.scale = 1,
            groups = Annotation$MorphoClust2, 
            ellipse = TRUE, epplipse.prob=0.68,circle = FALSE,
            labels.size = 10,varname.size = 5,varname.adjust = 2)+
  geom_point(aes(colour=Annotation$MorphoClust2),size=2)+scale_color_manual(values=c('#f8766d','#e76bf3','#00bf7d','#00b0f6','#a3a500'))+
  theme(panel.grid=element_blank(),legend.direction = 'horizontal',legend.position = 'no')
p
ggsave("E:/R/Morpho2/20210607_PCA_89_Mcluster.pdf",p,width=5,height=4)


#简易版#
p <- ggbiplot(morphodata,obs.scale = 1, var.scale = 1,groups = Annotation$cell.type3,ellipse=TRUE,ellipse.prob=0.8)
p
#X,Y, PC1,PC2 对应百分比#
xlab <- paste0("PC1 ",round(summ$importance[2,1]*100,2),"%") #PC1 方差的百分比，paste0 字符串合并函数#
ylab <- paste0("PC2 ",round(summ$importance[2,2]*100,2),"%")
#调节x,y比例#
p2 <- p+coord_fixed(ratio=0.8) #改变ratio改变xy比例#
#改变x轴范围#
p2 <- p+scale_x_continuous(limits=c(-5,5))
#改变色调#
p2<-p+scale_color_discrete(h=c(150,300)) #色调范围（h）、饱和度（c）和亮度（l）#




