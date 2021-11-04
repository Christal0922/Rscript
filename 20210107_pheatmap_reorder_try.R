Total <- read.table("E:/R/patch/20210107_235cells.txt",row.names = NULL,header = TRUE,sep = '\t')
rownames(Total) <- Total[,1]
Ephys <- na.omit(Total[,35:47]) #省略缺失值#
Annotation <- na.omit(Total)
nEphys<-apply(Ephys,2,scale) #对数据进行scale#
rownames(nEphys) <- rownames(Ephys)
data2 <- data.frame(Annotation$Ephys_Cluster,nEphys)
data3 <- data2[order(data2$Annotation.Ephys_Cluster),]
Anno3 <- Annotation[order(Annotation$Ephys_Cluster),]

library(pheatmap)
mycol<-colorRampPalette(c("blue","white","red"))(100)
bk=unique(c(seq(-3,3,length=100)))


#E1 figure#
E1C<-data3[which(data3$Annotation.Ephys_Cluster == 'E1'), ]
E1A <- Anno3[which(Anno3$Ephys_Cluster == "E1"),]
E1 <- E1C[,2:14]
E1_t <- as.data.frame(t(E1))
col_dist = dist(E1_t)
hclust_1 <- hclust(col_dist)
ano_row <- data.frame(E1A$Cell_Annotation,E1A$Layer,E1A$Brain_area,E1A$Pathology,E1A$Ephys_Cluster)
rownames(ano_row) <- rownames(E1A)
colnames(ano_row) <- c("Cell_Annotation","Layer","Brain_area","Pathology","Ephys_Cluster")
p2 <- pheatmap(E1,cluster_cols = hclust_1,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row)
manual_order=c("RP","Rin","Ins.freq","Freq","adaptation.index",
               "rheo.latency","threshold","peak","up_down" ,"halfwidth","ahp","rhe","Cm")
dend = reorder(as.dendrogram(hclust_1), wts=order(match(manual_order, rownames(E1_t))), agglo.FUN = max)
col_cluster <- as.hclust(dend)
p3 <- pheatmap(E1, cluster_cols = col_cluster,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row)
p3
ann_colors<-list(Ephys_Cluster=c(E1="#f8766d"),
                 Cell_Annotation=c(DN="#F8766D",immature="#E76BF3",PY="#00B0F6"),
                 Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF"),
                 Brain_area=c(FL="#F8766D",IL="#A3A500",TL="#00BF7D"),
                 Pathology=c(FCD="#F8766D",PT="#619CFF",TLE="#00BA38"))
E1P <- pheatmap(E1, cluster_cols = col_cluster,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row,annotation_colors = ann_colors)
pdf("E:/R/Morpho2/20210108_E1A.pdf",width = 6,height = 3) 
print(E1P)
dev.off()

#E2 figure#
E2C<-data3[which(data3$Annotation.Ephys_Cluster == 'E2'), ]
E2A <- Anno3[which(Anno3$Ephys_Cluster == "E2"),]
E2 <- E2C[,2:14]
E2_t <- as.data.frame(t(E2))
ano_row <- data.frame(E2A$Cell_Annotation,E2A$Layer,E2A$Brain_area,E2A$Pathology,E2A$Ephys_Cluster)
rownames(ano_row) <- rownames(E2A)
colnames(ano_row) <- c("Cell_Annotation","Layer","Brain_area","Pathology","Ephys_Cluster")
ann_colors<-list(Ephys_Cluster=c(E2="#e76bf3"),
                 Cell_Annotation=c(DN="#F8766D",PY="#00B0F6"),
                 Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF"),
                 Brain_area=c(FL="#F8766D",IL="#A3A500",TL="#00BF7D"),
                 Pathology=c(FCD="#F8766D",PT="#619CFF",TLE="#00BA38"))
E2P <- pheatmap(E2, cluster_cols = col_cluster,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row,annotation_colors = ann_colors)
pdf("E:/R/Morpho2/20210108_E2A.pdf",width = 6,height = 3) 
print(E2P)
dev.off()

#E3 figure#
E3C<-data3[which(data3$Annotation.Ephys_Cluster == 'E3'), ]
E3A <- Anno3[which(Anno3$Ephys_Cluster == "E3"),]
E3 <- E3C[,2:14]
E3_t <- as.data.frame(t(E3))
ano_row <- data.frame(E3A$Cell_Annotation,E3A$Layer,E3A$Brain_area,E3A$Pathology,E3A$Ephys_Cluster)
rownames(ano_row) <- rownames(E3A)
colnames(ano_row) <- c("Cell_Annotation","Layer","Brain_area","Pathology","Ephys_Cluster")
ann_colors<-list(Ephys_Cluster=c(E3="#00bf7d"),
                 Cell_Annotation=c(DN="#F8766D",immature="#E76BF3",INT="#00BF7D",PY="#00B0F6"),
                 Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF"),
                 Brain_area=c(FL="#F8766D",IL="#A3A500",TL="#00BF7D"),
                 Pathology=c(FCD="#F8766D",PT="#619CFF",TLE="#00BA38"))
E3P <- pheatmap(E3, cluster_cols = col_cluster,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row,annotation_colors = ann_colors)
E3P
pdf("E:/R/Morpho2/20210108_E3A.pdf",width = 6,height = 3) 
print(E3P)
dev.off()

#E4 figure#
E4C<-data3[which(data3$Annotation.Ephys_Cluster == 'E4'), ]
E4A <- Anno3[which(Anno3$Ephys_Cluster == "E4"),]
E4 <- E4C[,2:14]
E4_t <- as.data.frame(t(E4))
ano_row <- data.frame(E4A$Cell_Annotation,E4A$Layer,E4A$Brain_area,E4A$Pathology,E4A$Ephys_Cluster)
rownames(ano_row) <- rownames(E4A)
colnames(ano_row) <- c("Cell_Annotation","Layer","Brain_area","Pathology","Ephys_Cluster")
ann_colors<-list(Ephys_Cluster=c(E4="#00b0f6"),
                 Cell_Annotation=c(immature="#E76BF3",INT="#00BF7D",PY="#00B0F6"),
                 Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF"),
                 Brain_area=c(FL="#F8766D",TL="#00BF7D"),
                 Pathology=c(FCD="#F8766D",PT="#619CFF",TLE="#00BA38"))
E4P <- pheatmap(E4, cluster_cols = col_cluster,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row,annotation_colors = ann_colors)
E4P
pdf("E:/R/Morpho2/20210108_E4A.pdf",width = 6,height = 3) 
print(E4P)
dev.off()

#E5 figure#
E5C<-data3[which(data3$Annotation.Ephys_Cluster == 'E5'), ]
E5A <- Anno3[which(Anno3$Ephys_Cluster == "E5"),]
E5 <- E5C[,2:14]
E5_t <- as.data.frame(t(E5))
ano_row <- data.frame(E5A$Cell_Annotation,E5A$Layer,E5A$Brain_area,E5A$Pathology,E5A$Ephys_Cluster)
rownames(ano_row) <- rownames(E5A)
colnames(ano_row) <- c("Cell_Annotation","Layer","Brain_area","Pathology","Ephys_Cluster")
ann_colors<-list(Ephys_Cluster=c(E5="#A3A500"),
                 Cell_Annotation=c(FS="#A3A500",immature="#E76BF3",INT="#00BF7D"),
                 Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF"),
                 Brain_area=c(FL="#F8766D",IL="#A3A500",PL="#E76BF3",TL="#00BF7D"),
                 Pathology=c(FCD="#F8766D",PT="#619CFF",TLE="#00BA38"))
E5P <- pheatmap(E5, cluster_cols = col_cluster,color=mycol,border_color=NA,show_rownames=F,breaks = bk,annotation_row = ano_row,annotation_colors = ann_colors)
E5P
pdf("E:/R/Morpho2/20210108_E5A.pdf",width = 6,height = 3) 
print(E5P)
dev.off()



