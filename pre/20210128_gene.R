library(clusterProfiler)
library(org.Hs.eg.db) #人源#
genelist <- c("CUX2","SLC17A7","LINC00507","SATB2","GAD1","GAD2","ERBB4","LHX6","RPS6",
              "NRGN","RORB","PVALB","SST","VIP","SV2C","GFAP","PLP1","PDGFRA","PTPRC","CLDN5",
              "CDKN1A","CCND1","CCL2","NFKBIA","RPS6","MT-CO2","MT-CO1")
gene.df2 <- bitr(genelist, fromType = "SYMBOL", #fromType是指你的数据ID类型是属于哪一类的
                 toType = c("GENENAME"), #toType是指你要转换成哪种ID类型，可以写多种，也可以只写一种
                 OrgDb = org.Hs.eg.db)#Orgdb是指对应的注释包是哪个
write.csv(gene.df2,"E:/R/patch/20210302_geneAnnotation.csv",row.names = FALSE)

#读入文件#
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=1,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG <- read.table("patch/20200210 Voltage gated ion channel.txt",header = TRUE,sep = '\t')
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2 <- data.frame(rownames(Total_anno),Total_anno)
rownames(Total_anno2) <- rownames(Total_anno)
DEG_matrix<-Total[match(DEG$Gene,rownames(Total)),]
DEG_matrix2 <- na.omit(DEG_matrix)
DEG_matrix3 <-data.frame(log2(1+DEG_matrix2))
data <- DEG_matrix3

library(pheatmap)
mycol<-colorRampPalette(c("blue","white","red"))(100)
bk=unique(c(seq(-3,3,length=100)))

ano_col <- data.frame(Total_anno2$group)
rownames(ano_col) <- rownames(Total_anno2)
colnames(ano_col) <- "group"

data_t <- data.frame(t(data))
data2 <- cbind(ano_col,data_t)
data2 <- data2[order(data2$group),]
write.csv(data2,"patch/20200210_Voltage gated ion channel_expr.csv",row.names = TRUE)
data2 <- data2[,-1]
data2 <- data.frame(t(data2))

ann_colors<-list(Ephys_Cluster=c(IN="#00BE67",NRPS6="#FF61CC",PY="#00A9FF"))
p1 <- pheatmap(data2,scale="row",cluster_cols = F,color = mycol,breaks = bk,border_color=NA,annotation_col = ano_col,show_colnames = F,annotation_colors = ann_colors)
p1

