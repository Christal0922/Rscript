Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total2 <- Total[,2:236]
rownames(Total2) <- Total$Genename
library(pheatmap)
tmp <- data.frame(group=Total_anno$group)
rownames(tmp) <- rownames(Total_anno)
ann_colors<-list(group=c(IN="#00BE67",NRPS6="#FF61CC",PY="#00A9FF"))
p1 <- pheatmap(cor(Total2), border_color=NA,show_rownames=F,show_colnames=F,annotation_row = tmp,annotation_col = tmp,annotation_colors = ann_colors)
p1 # 5in, 6in#

Total_anno2 <- na.omit(Total_anno)
Val <- as.matrix(names(Total_anno2))
Total_anno3 <- Total_anno2[,31:43]
Total_anno4 <- apply(Total_anno3,1,scale)
rownames(Total_anno4) <- colnames(Total_anno3)
Total3 <- t(Total2)
cell <- rownames(Total_anno3)
Total4 <- Total3[match(cell,rownames(Total3)),]



Total5 <- Total4[1:4,1:4]
Total_anno5 <- t(Total_ann4)[1:4,1:2]
cor(Total5,Total_anno4)


#morphodata#
morphodata <- read.table("E:/R/PCA/20201218_morpho_89cells.txt",row.names = 1,sep = '\t',header = T)
data1 <- morphodata[which(morphodata$EClust=='E1'|morphodata$EClust=='E2'),]
x <- data1[,12:21]
y <- data1[,22:34]
cor(x,y)
ano_row <- data.frame(data1$MorphoClust2)
rownames(ano_row) <- rownames(data1)
ano_col <- data.frame(data1$EClust)
rownames(ano_col) <- rownames(data1)
Z <- cbind(x,y)
d <- as.data.frame(t(Z))
p2 <- pheatmap(cor(x,y))
p2

ano <- data.frame(data1$MorphoClust2,data1$EClust)
colnames(ano) <- c("MClust","EClust")
rownames(ano) <- rownames(data1)
ann_colors<-list(MClust=c(M1="#f8766d",M2="#e76bf3",M3="#00bf7d",M4="#00b0f6",M5="#a3a500"),
                 EClust=c(E1="#00D766",E2="#FFA148"))
p3 <- pheatmap(cor(d),order_color=NA,show_rownames=F,show_colnames=F,annotation_row = ano,annotation_col = ano,annotation_colors = ann_colors)
p3
