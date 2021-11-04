Morpho <- read.csv("E:/R/PCA/20210608_cell89M2.csv",header = TRUE,row.names = 1)
Val <- as.matrix(colnames(Morpho))
Morpho2 <- Morpho[,c(18:22,24:28)]
nMorpho2 <- apply(Morpho2,2,scale) #按列进行标准化
rownames(nMorpho2) <- rownames(Morpho2)

#选择聚类的个数
library(NbClust)
devAskNewPage(ask = TRUE)
nc <- NbClust(nMorpho2, distance="euclidean",
              min.nc=2, max.nc=15, method="complete")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
#Hierarchical complete 
out.dist <- dist(nMorpho2,method = "euclidean")
out.hclust <- hclust(out.dist,method = "complete")
out.id2 <- cutree(out.hclust,k=2)
out.id3 <- cutree(out.hclust,k=3)
out.id5 <- cutree(out.hclust,k=5)
out.hclust <- hclust(out.dist,method = "complete")

#annotation
Morphoanno <- Morpho[,1:17]
Morphoanno$Complete2 <- as.factor(out.id2)
Morphoanno$Complete3 <- as.factor(out.id3)
Morphoanno$Complete5 <- as.factor(out.id5)

#other methods average#
average2 <- cutree(hclust(out.dist,method = "average"),k=2)
average3 <- cutree(hclust(out.dist,method = "average"),k=3)
average5 <- cutree(hclust(out.dist,method = "average"),k=5)
Morphoanno$average2 <- as.factor(average2)
Morphoanno$average3 <- as.factor(average3)
Morphoanno$average5 <- as.factor(average5)
#ward法
ward2 <- cutree(hclust(out.dist,method = "ward"),k=2)
ward3 <- cutree(hclust(out.dist,method = "ward"),k=3)
ward5 <- cutree(hclust(out.dist,method = "ward"),k=5)
Morphoanno$ward2 <- as.factor(ward2)
Morphoanno$ward3 <- as.factor(ward3)
Morphoanno$ward5 <- as.factor(ward5)
#heatmap可视化
library(pheatmap)
mycol<-colorRampPalette(c("blue","white","red"))(100)
bk=unique(c(seq(-4,4,length=100)))
ano_row <- data.frame(Morphoanno$Complete2,Morphoanno$Complete3,Morphoanno$Complete5,Morphoanno$cell.type,Morphoanno$cell.type2)
rownames(ano_row) <- rownames(Morphoanno)
p1<-pheatmap(nMorpho2,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row,breaks = bk)
p1
pdf("E:/R/Morpho2/20210616_89mor_complete235.pdf",width = 8,height = 12)
print(p1)
dev.off()

#k-means
set.seed(1234)
fit.km2 <- kmeans(nMorpho2,2,nstart = 25)
fit.km3 <- kmeans(nMorpho2,3,nstart = 25)
fit.km5 <- kmeans(nMorpho2,5,nstart = 25)
Morphoanno$kmeans2 <- as.factor(fit.km2$cluster)
Morphoanno$kmeans3 <- as.factor(fit.km3$cluster)
Morphoanno$kmeans5 <- as.factor(fit.km5$cluster)

#PAM
library(cluster)
set.seed(1234)
fit.pam2 <- pam(nMorpho2,k=2,stand=FALSE)
fit.pam3 <- pam(nMorpho2,k=3,stand=FALSE)
fit.pam5 <- pam(nMorpho2,k=5,stand=FALSE)
Morphoanno$pam2 <- as.factor(fit.pam2$clustering)
Morphoanno$pam3 <- as.factor(fit.pam3$clustering)
Morphoanno$pam5 <- as.factor(fit.pam5$clustering)

#输出分类表格
Morpho3 <- data.frame(rownames(Morphoanno),Morphoanno,Morpho2,Morphoumap.info$layout)
write.csv(Morpho3,"E:/R/PCA/20210616_cluster.csv",row.names = FALSE)

#umap可视化
library(umap)
library(ggpubr)
library(ggthemes)
library(ggplot2)
Morphoumap.info<-umap(nMorpho2)
colnames(Morphoumap.info$layout)<-c("umap_1","umap_2")
Morphoumap<-data.frame(Morphoanno,Morphoumap.info$layout) #生成用于umap作图的数据集#
pam_2<-ggscatter(Morphoumap,x="umap_1",y="umap_2",color="pam2",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)
pam_2<-ggpar(pam_2,legend = "right")
pam_2

pdf("E:/R/Morpho2/20210616_Morphoumap_Anno.pdf",width = 5,height = 4)
for (i in 13:32){
  p<-ggscatter(Morphoumap,x="umap_1",y="umap_2",color=Val2[i],repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)
  p<-ggpar(p,legend = "right")
  print (p)
  i=i+1
}
dev.off()


