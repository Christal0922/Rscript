set.seed(24321)
Total <- read.table("E:/R/patch/20210107_235cells.txt",row.names = NULL,header = TRUE,sep = '\t')
rownames(Total) <- Total[,1]
Ephys <- na.omit(Total[,35:47]) #省略缺失值#
Annotation <- na.omit(Total)
nEphys<-apply(Ephys,2,scale) #对数据进行scale#
library(umap)
library(ggpubr)
library(ggthemes)
library(ggplot2)
Ephysumap.info<-umap(nEphys) #进行umap计算#
colnames(Ephysumap.info$layout)<-c("umap_1","umap_2")
Ephysumap<-data.frame(Annotation,Ephysumap.info$layout) #生成用于umap作图的数据集#

#Umap by cell annotation with legend#
Cell_Annotation<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Cell_Annotation",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#A3A500","#E76BF3","#00BF7D","#00B0F6"))
Cell_Annotation<-ggpar(Cell_Annotation,legend = "right")
Ephys_Cluster<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Ephys_Cluster",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#E76BF3","#00BF7D","#00B0F6","#A3A500"))
Ephys_Cluster<-ggpar(Ephys_Cluster,legend = "right")
Ephys_Cluster
Pathology<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Pathology",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#619CFF","#00BA38"))
Pathology<-ggpar(Pathology,legend = "right")
Layer<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Layer",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#9FA0A0","#619CFF"))
Layer<-ggpar(Layer,legend = "right")
Brain_area<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Brain_area",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#A3A500","#E76BF3","#00BF7D"))
Brain_area<-ggpar(Brain_area,legend = "right")
Predicted_cellType<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Predicted_cellType",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#CD9600","#7CAE00","#00BE67","#00BFC4","#00A9FF","#C77CFF","#FF61CC"))
Predicted_cellType<-ggpar(Predicted_cellType,legend = "right")
Group<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Group",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#00BE67","#00A9FF","#FF61CC"))
Group<-ggpar(Group,legend = "right")
Sex<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Sex",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,palette = c("#F8766D","#619CFF"))
Sex<-ggpar(Sex,legend = "right")
Anno <- list(Cell_Annotation,Ephys_Cluster,Pathology,Layer,Brain_area,Predicted_cellType,Group,Sex)
pdf("E:/R/Morpho2/20210111_Ephysumap_Anno.pdf",width = 5,height = 4)
print (Anno)
dev.off()

#Umap by cell annotation without legend#
Cell_Annotation<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Cell_Annotation",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                           palette = c("#F8766D","#A3A500","#E76BF3","#00BF7D","#00B0F6"))
Cell_Annotation<-ggpar(Cell_Annotation,legend = "none")
Ephys_Cluster<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Ephys_Cluster",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                         palette = c("#F8766D","#E76BF3","#00BF7D","#00B0F6","#A3A500"))
Ephys_Cluster<-ggpar(Ephys_Cluster,legend = "none")
Pathology<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Pathology",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                     palette = c("#F8766D","#619CFF","#00BA38"))
Pathology<-ggpar(Pathology,legend = "none")
Layer<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Layer",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                 palette = c("#F8766D","#9FA0A0","#619CFF"))
Layer<-ggpar(Layer,legend = "none")
Brain_area<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Brain_area",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                      palette = c("#F8766D","#A3A500","#E76BF3","#00BF7D"))
Brain_area<-ggpar(Brain_area,legend = "none")
Predicted_cellType<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Predicted_cellType",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                              palette = c("#F8766D","#CD9600","#7CAE00","#00BE67","#00BFC4","#00A9FF","#C77CFF","#FF61CC"))
Predicted_cellType<-ggpar(Predicted_cellType,legend = "none")
Group<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Group",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
                 palette = c("#00BE67","#00A9FF","#FF61CC"))
Group<-ggpar(Group,legend = "none")
Sex<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="Sex",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE,
               palette = c("#F8766D","#619CFF"))
Sex<-ggpar(Sex,legend = "none")
Anno <- list(Cell_Annotation,Ephys_Cluster,Pathology,Layer,Brain_area,Predicted_cellType,Group,Sex)
pdf("E:/R/Morpho2/20210111_Ephysumap_Anno_nolegend.pdf",width = 4,height = 4)
print (Anno)
dev.off()

#电生理参数feature plot umap with legend#
Val <- names(Ephysumap)
i=1
mycol<-colorRampPalette(c("#D3D3D3","#FF2712"))(100)
pdf("E:/R/Morpho2/20210111_Ephysumap_nolegend.pdf",width = 4,height = 4)
for (i in 1:13) {
  Ephysumap[,(i+34)] <- as.numeric(Ephys[,i])
  p <-ggscatter(Ephysumap,x="umap_1",y="umap_2",color=Val[i+34],repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
  p <-ggpar(p,legend = "right")
  print (p)
  i=i+1
}
dev.off()

#nfeature, nCount #
mycol<-colorRampPalette(c("#D3D3D3","#FF2712"))(100)
nFeature<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="nFeature_RNA",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
nFeature<-ggpar(nFeature,legend = "right")
nFeature2<-ggpar(nFeature,legend = "none")
nCount<-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="nCount_RNA",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
nCount<-ggpar(nCount,legend = "right")
nCount2<-ggpar(nCount,legend = "none")
genenum <- list(nFeature,nFeature2,nCount,nCount2)
pdf("E:/R/Morpho2/20210111_Ephysumap_genenum.pdf",width = 4,height = 4)
print (genenum)
dev.off()

#电生理参数feature plot umap without legend#
Val <- names(Ephysumap)
i=1
mycol<-colorRampPalette(c("#D3D3D3","#FF2712"))(100)
pdf("E:/R/Morpho2/20210111_Ephysumap_nolegend.pdf",width = 4,height = 4)
for (i in 1:13) {
  Ephysumap[,(i+34)] <- as.numeric(Ephys[,i]) #使用原始数据，而非scale后的数据#
  p <-ggscatter(Ephysumap,x="umap_1",y="umap_2",color=Val[i+34],repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
  p <-ggpar(p,legend = "none")
  print (p)
  i=i+1
}
dev.off()

#基因umap#
Total_gene <-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG_find <- c("CUX2","SLC17A7","LINC00507","SATB2",
              "GAD1","GAD2","ERBB4","LHX6")
DEG_matrix<-Total_gene[match(DEG_find,Total_gene$Genename),] #大表里提取找到的gene的矩阵#
logexp<-data.frame(log2(1+DEG_matrix[,2:236])) #对数据进行取log#
rownames(logexp) <- DEG_matrix$Genename
logexp2 <- t(data.frame(logexp))
logexp2 <- data.frame(rownames(logexp2),logexp2) #生成cellID 和 GENE的数据#
colnames(logexp2)[1] <- "SeqID2"
T_merge<-merge(Ephysumap,logexp2,by.x="SeqID",by.y="SeqID2") #将基因表达情况与umap作图数据集合并#
Val2 <- names(T_merge) #提取T_merge的列名#

#gene umap with legend#
mycol<-colorRampPalette(c("#D3D3D3","#FF2712"))(100)
pdf("E:/R/Morpho2/20210203_Ephysumap_byMole.pdf",width = 5,height = 4)

i=1
for (i in 1:length(DEG_find)) {
  p <-ggscatter(T_merge,x="umap_1",y="umap_2",color=Val2[i+49],repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
  p <-ggpar(p,legend = "right")
  print (p)
  i=i+1
}
dev.off()

#age#
mycol<-colorRampPalette(c("#D3D3D3","#FF2712"))(100)
p <-ggscatter(Ephysumap,x="umap_1",y="umap_2",color="age",repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
p1 <-ggpar(p,legend = "none")
p2 <-ggpar(p,legend = "right")
pdf("E:/R/Morpho2/20210203_age.pdf",width = 4,height = 4)
print(p1)
print(p2)
dev.off()

#gene umap without legend#
mycol<-colorRampPalette(c("#D3D3D3","#FF2712"))(100)
pdf("E:/R/Morpho2/20210111_Ephysumap_byMole_nolegend.pdf",width = 4,height = 4)
i=1
for (i in 1:length(DEG_find)) {
  p <-ggscatter(T_merge,x="umap_1",y="umap_2",color=Val2[i+49],repel = TRUE,size=2,xlab = FALSE,ylab = FALSE)+gradient_color(palette = mycol)
  p <-ggpar(p,legend = "none")
  print (p)
  i=i+1
}
dev.off()

#set.seed(321)#