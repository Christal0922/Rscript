Morpho <- read.csv("E:/R/PCA/20210617_morpho_89cells.csv",header = TRUE,row.names = 1)
Val2 <- as.matrix(colnames(Morpho2))
Morpho2 <- Morpho[,c(12:23)]
nMorpho2 <- apply(Morpho2,2,scale) #按列进行标准化
rownames(nMorpho2) <- rownames(Morpho2)
Morphoanno <- Morpho[,1:11]

#含soma surface area
nMorpho3 <- nMorpho2[,c(-2,-3,-6)]

#complete 聚类
out.dist <- dist(nMorpho3,method = "euclidean")
complete2 <- cutree(hclust(out.dist,method = "complete"),k=2)
complete3 <- cutree(hclust(out.dist,method = "complete"),k=3)
complete5 <- cutree(hclust(out.dist,method = "complete"),k=5)
Morphoanno$complete2 <- as.factor(complete2)
Morphoanno$complete3 <- as.factor(complete3)
Morphoanno$complete5 <- as.factor(complete5)

#ward 聚类
ward2 <- cutree(hclust(out.dist,method = "ward"),k=2)
ward3 <- cutree(hclust(out.dist,method = "ward"),k=3)
ward5 <- cutree(hclust(out.dist,method = "ward"),k=5)
Morphoanno$ward2 <- as.factor(ward2)
Morphoanno$ward3 <- as.factor(ward3)
Morphoanno$ward5 <- as.factor(ward5)

#k-means聚类
set.seed(1234)
fit.km2 <- kmeans(nMorpho3,2,nstart = 25)
fit.km3 <- kmeans(nMorpho3,3,nstart = 25)
fit.km5 <- kmeans(nMorpho3,5,nstart = 25)
Morphoanno$kmeans2 <- as.factor(fit.km2$cluster)
Morphoanno$kmeans3 <- as.factor(fit.km3$cluster)
Morphoanno$kmeans5 <- as.factor(fit.km5$cluster)

#PAM聚类
library(cluster)
set.seed(1234)
fit.pam2 <- pam(nMorpho3,k=2,stand=FALSE)
fit.pam3 <- pam(nMorpho3,k=3,stand=FALSE)
fit.pam5 <- pam(nMorpho3,k=5,stand=FALSE)
Morphoanno$pam2 <- as.factor(fit.pam2$clustering)
Morphoanno$pam3 <- as.factor(fit.pam3$clustering)
Morphoanno$pam5 <- as.factor(fit.pam5$clustering)

#umap可视化
library(umap)
library(ggpubr)
library(ggthemes)
library(ggplot2)
Morphoumap.info<-umap(nMorpho3)
colnames(Morphoumap.info$layout)<-c("umap_1","umap_2")
Morphoumap<-data.frame(Morphoanno$cell.type,Morphoanno$cell.type3,Morphoanno$MorphoClust2,Morphoanno[,12:23],Morphoumap.info$layout) #生成用于umap作图的数据集#
Val2 <- colnames(Morphoumap)
pam_2<-ggscatter(Morphoumap,x="umap_1",y="umap_2",color="pam2",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)
pam_2<-ggpar(pam_2,legend = "right")
pam_2
#批量输出umap图
pdf("E:/R/Morpho2/20210617_Morphoumap_s.volume.pdf",width = 5,height = 4)
for (i in 1:15){
  p<-ggscatter(Morphoumap,x="umap_1",y="umap_2",color=Val2[i],repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)
  p<-ggpar(p,legend = "right")
  p
  print (p)
  i=i+1
}
dev.off()


#输出分类表格
Morpho4 <- data.frame(rownames(Morphoanno),Morphoanno,Morpho2,Morphoumap.info$layout)
write.csv(Morpho4,"E:/R/PCA/20210617_cluster_s.volume.csv",row.names = FALSE)

#pheatmap可视化
library(pheatmap)
mycol<-colorRampPalette(c("blue","white","red"))(100)
bk=unique(c(seq(-4,4,length=100)))
ano_row <- data.frame(Morphoumap[,c(2,3,5,8,11,14)])
rownames(ano_row) <- rownames(Morphoumap)
p1<-pheatmap(nMorpho3,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row,breaks = bk)
p1
pdf("E:/R/Morpho2/20210617_89mor_s.volume.pdf",width = 12,height = 16)
print(p1)
dev.off()



