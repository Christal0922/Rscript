Ephysdata <- read.table("E:/R/PCA/20201218_340cells.txt",row.names = 1,sep = '\t',header = T)
Ephyspara <- Ephysdata[,13:25] #提取仅有电生理参数数值的数据#
Ephysanno <- Ephysdata[,1:12]
data <- apply(Ephyspara,2,scale) #对数据按列scale#
rownames(data) <- rownames(Ephyspara)
out.dist <- dist(data,method = "euclidean")
out.hclust <- hclust(out.dist,method = "complete")
out.id <- cutree(out.hclust,k=3)
Ephysanno$EClust2 <- as.factor(out.id) #按照hclust方法聚类#

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
ano_row <- data.frame(Ephysanno$MClust,Ephysanno$cell.annotation,Ephysanno$Pathology,Ephysanno$Brain.area,Ephysanno$EClust)
rownames(ano_row) <- rownames(Ephysanno)
colnames(ano_row) <- c("MClust","Cell.annotation","Pathology","Brain.area","EClust")
range(data)
bk=unique(c(seq(-3,3,length=100)))
ann_colors<-list(MClust=c(M1="#f8766d",M2="#e76bf3",M3="#00bf7d",M4="#00b0f6",M5="#a3a500"),Cell.annotation=c(upper_DN="#F08CAA",deeper_DN="#C81E50",upper_PY="#6482FF",deeper_PY="#001E96"),Pathology=c(FCD2="#F8766D",TLE="#00BA3B",Tumor="#619CFF"),Brain.area=c(FL="#F8766D",TL="#00BA3B",PL="#619CFF"),EClust=c(E1="#00d766",E2="#ffa148",E3="#A0B0FF"))
p4<-pheatmap(tdata,color=mycol,border_color=NA,show_rownames=TRUE,show_colnames = FALSE,annotation_col = ano_row,annotation_colors = ann_colors,breaks = bk)
p4