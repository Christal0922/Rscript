morphodata <- read.table("E:/R/PCA/20201218_morpho_89cells.txt",row.names = 1,sep = '\t',header = T)
morphopara <- morphodata[,12:21] #提取仅有形态学参数数值的数据#
morphoanno <- morphodata[,1:11]
data <- apply(morphopara,2,scale) #对数据按列scale#
rownames(data) <- rownames(morphopara)
out.dist <- dist(data,method = "euclidean")
out.hclust <- hclust(out.dist,method = "complete")
out.id <- cutree(out.hclust,k=5)
morphoanno$MClust <- as.factor(out.id) #按照hclust方法聚类#

mycol<-colorRampPalette(c("blue","white","red"))(100)
library(pheatmap)
p1<-pheatmap(data,color=mycol,border_color="white",show_rownames=F)
order <- p1$tree_row
range(data)
bk=unique(c(seq(-4,4,length=100)))
ano_row <- data.frame(morphoanno$MorphoClust2,morphoanno$cell.annotation,morphoanno$pathology,morphoanno$Brain.area)
rownames(ano_row) <- rownames(morphoanno)
colnames(ano_row) <- c("MClust","Cell.annotation","Pathology","Brain.area")
ann_colors<-list(MClust=c(M1="#f8766d",M2="#e76bf3",M3="#00bf7d",M4="#00b0f6",M5="#a3a500"),Cell.annotation=c(upper_DN="#F08CAA",deeper_DN="#C81E50",upper_PY="#6482FF",deeper_PY="#001E96"),Pathology=c(FCD2="#F8766D",TLE="#00BA3B",Tumor="#619CFF"),Brain.area=c(FL="#F8766D",TL="#00BA3B"))
p2<-pheatmap(data,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row,annotation_colors = ann_colors,breaks = bk)
p2

#转换pheatmap的xy#
library(pheatmap)
tdata <- t(data)
mycol<-colorRampPalette(c("blue","white","red"))(100)
p3<-pheatmap(tdata,color=mycol,border_color=NA,show_rownames=TRUE,show_colnames = FALSE)
ano_row <- data.frame(morphoanno$MorphoClust2,morphoanno$cell.annotation,morphoanno$pathology,morphoanno$Brain.area)
rownames(ano_row) <- rownames(morphoanno)
colnames(ano_row) <- c("MClust","Cell.annotation","Pathology","Brain.area")
bk=unique(c(seq(-2.5,2.5,length=100)))
ann_colors<-list(MClust=c(M1="#f8766d",M2="#e76bf3",M3="#00bf7d",M4="#00b0f6",M5="#a3a500"),Cell.annotation=c(upper_DN="#F08CAA",deeper_DN="#C81E50",upper_PY="#6482FF",deeper_PY="#001E96"),Pathology=c(FCD2="#F8766D",TLE="#00BA3B",Tumor="#619CFF"),Brain.area=c(FL="#F8766D",TL="#00BA3B"))
p4<-pheatmap(tdata,color=mycol,border_color=NA,show_rownames=TRUE,show_colnames = FALSE,annotation_col = ano_row,annotation_colors = ann_colors,breaks = bk)
p4