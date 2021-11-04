dt2<-read.delim('E:/R/PCA/Morpho_89cells_10p.txt',row.names=1,sep = '\t', header = T,quote = '')
df2<-dt2[,5:14]
da<-dt2[,1:4]
data<-apply(df2,2,scale)
rownames(data)<-rownames(df2)
out.dist=dist(data,method = "euclidean")
out.hclust=hclust(out.dist,method="complete")
out.id=cutree(out.hclust,k=5)
da$MorphoClust<-as.factor(out.id)
ord<-prcomp(data)
de<-ord$x
summ<-summary(ord)
dd <- summ$rotation
