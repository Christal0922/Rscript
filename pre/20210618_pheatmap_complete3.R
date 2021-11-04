Morpho <- read.csv("E:/R/PCA/20210618_morpho_89cells.csv",header = TRUE,row.names = 1)
Val <- as.matrix(colnames(Morpho))
Morpho2 <- Morpho[,c(12:23)]
colnames(Morpho2) <- c("Tree Termination Distance","Soma Surface Area",
                          "S_Soma Surface Area","Contraction",
                          "Maximum Branch Order","Cell Body Volume",
                          "Soma Volume","Local Angle",
                          "Dendrite Quantity","Dendrite Nodes",
                          "Dendrite Ends","Dendrite Total Length")
nMorpho2 <- apply(Morpho2,2,scale) #按列进行标准化
rownames(nMorpho2) <- rownames(Morpho2)
Morphoanno <- Morpho[,1:11]

#含s.volume
nMorpho3 <- nMorpho2[,c(-2,-3,-6)]
#Complete聚类
out.dist <- dist(nMorpho3,method = "euclidean")
complete3 <- cutree(hclust(out.dist,method = "complete"),k=3)
Morphoanno$complete3 <- as.factor(complete3)

#输出分类表格
Morpho4 <- data.frame(rownames(Morphoanno),Morphoanno[,c(1:8,11:13)],Morpho2)
write.csv(Morpho4,"E:/R/PCA/20210618_cluster_s.volume_Mcluster.csv",row.names = FALSE)

#pheatmap 
library(pheatmap)
mycol<-colorRampPalette(c("blue","white","red"))(100)
bk=unique(c(seq(-2.5,2.5,length=100)))
ano_row <- data.frame(Morphoanno[,12])
colnames(ano_row) <- "Cluster"
rownames(ano_row) <- rownames(Morphoanno)
p1<-pheatmap(nMorpho3,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row,breaks = bk)
p1
pdf("E:/R/Morpho2/20210617_89mor_s.volume.pdf",width = 8.27,height = 11.69)
print(p1)
dev.off()

#热图旋转，ano_col
nMorpho4 <- t(nMorpho3)
p2<-pheatmap(nMorpho4,color=mycol,border_color=NA,show_rownames=T,show_colnames=F,annotation_col = ano_row,breaks = bk)
p2
pdf("E:/R/Morpho2/20210618_89mor_s.volume2.5.pdf",width = 8,height = 14)
print(p2)
dev.off()
#调整cluster命名和颜色
Morphoanno$MClust_new[Morphoanno$complete3 ==1]<- "M2"
Morphoanno$MClust_new[Morphoanno$complete3 ==2]<- "M3"
Morphoanno$MClust_new[Morphoanno$complete3 ==3]<- "M1"
ano_row <- data.frame(Morphoanno[,13])
colnames(ano_row) <- "Cluster"
rownames(ano_row) <- rownames(Morphoanno)
p2<-pheatmap(nMorpho4,color=mycol,border_color=NA,show_rownames=T,show_colnames=F,annotation_col = ano_row,breaks = bk)
p2
ann_colors<-list(Cluster=c(M1="#ff9289",M2="#82B7FF",M3="#00D65C"))
p3<-pheatmap(nMorpho4,color=mycol,border_color=NA,show_rownames=T,show_colnames=F,annotation_col = ano_row,breaks = bk,annotation_colors = ann_colors)
p3
pdf("E:/R/Morpho2/20210618_89mor_s.volume2.5c.pdf",width = 8,height = 14)
print(p3)
dev.off()

#PCA
#计算PCA
morphodata <- prcomp(nMorpho3) #计算主成分#
PCimportance <- morphodata$rotation #显示各个PC成分中最主要的参数#
#PC1:dendritic ends,dendrite total length,  dendritic nodes; PC2: average local angle, average contraction#
summ <- summary(morphodata) #多了importance, 计算多个PC的方差，计算累计概率#

#作图
#简易版
library(ggbiplot)
p <- ggbiplot(morphodata,obs.scale = 1, var.scale = 1,groups = Morphoanno$complete3,ellipse=TRUE,ellipse.prob=0.8)
p
#高级版 cluster#
p<-ggbiplot(morphodata, obs.scale = 1, var.scale = 1,
            groups = Morphoanno$MClust_new, 
            ellipse = TRUE, epplipse.prob=0.8,circle = FALSE,
            labels.size = 8,varname.size = 3,varname.adjust = 1)+
  geom_point(aes(colour=Morphoanno$MClust_new),size=2)+scale_color_manual(values=c('#FF9289','#82B7FF','#00D65C'))+
  theme(panel.grid=element_blank(),legend.direction = 'horizontal',legend.position = 'no',
        axis.title = element_text(size = 12),axis.text = element_text(size=10))
p
ggsave("E:/R/Morpho2/20210621_PCA_89_Cluster2.pdf",p,width=5,height=4)

#高级版 cell type#
p2<-ggbiplot(morphodata, obs.scale = 1, var.scale = 1,
            groups = Morphoanno$cell.type3, 
            ellipse = TRUE, epplipse.prob=0.8,circle = FALSE,
            labels.size = 8,varname.size = 3,varname.adjust = 1)+
  geom_point(aes(colour=Morphoanno$cell.type3),size=2)+scale_color_manual(values=c('#c81e50','#001e96','#f08caa','#6482ff'))+
  theme(panel.grid=element_blank(),legend.direction = 'horizontal',legend.position = 'no',
        axis.title = element_text(size = 12),axis.text = element_text(size=10))
p2
ggsave("E:/R/Morpho2/20210621_PCA_89_celltype.pdf",p2,width=5,height=4)

