#输入表达谱----
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
#输入标签----
Total_anno<-read.csv("E:/R/patch/20210928_235cells.csv",row.names=1,header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$group,Total_anno$Type) #Total_anno2提取group信息#

#手动输入DEG----
DEG <- c("LINC00507","CUX2","SATB2","RORB","FEZF2","CUX1","FOXP2","PCP4","TLE4",
         "PVALB","SST","VIP","GAD1","PAX6","SV2C")
#cortical layer marker----
#"LINC00507","CUX2","SATB2","RORB","FEZF2","CUX1","FOXP2","PCP4","TLE4","PVALB","SST","VIP","GAD1","PAX6","SV2C"
#upper:"LINC00507","CUX2","SATB2","RORB","FEZF2","CUX1",
#total:"SLC17A7",
#deeper:"THEMIS","BCL11B","FOXP2","CTGF","POUF3F2","PCP4","CNTC6","TLE4"
#GABA:"PVALB","SST","VIP","GAD1","PAX6"
#剔除："MEF2C","LAMP5","ADARB2","THEMIS","BCL11B","CTGF","POUF3F2","CNTC6",

#excel输入DEG----
genel <- read.csv("E:/Rtest/Input/20210928_genelist.csv",header = T)
DEG <- genel[,8]
colnames(genel)[8]
#计算----
DEG_matrix<-Total[match(DEG,Total$Genename),]
DEG_matrix <- DEG_matrix[complete.cases(DEG_matrix),]
rownames(DEG_matrix) <- DEG_matrix$Genename
logexp<-t(data.frame(log2(1+DEG_matrix[,2:236])))
logexp <- data.frame(rownames(logexp),logexp)
T_merge<-merge(Total_anno2,logexp,by.x="rownames.Total_anno.",by.y="rownames.logexp.") #合并Tlog以及Total_anno2, 使其具有group信息, 得到带分组类别的表达举证#
colnames(T_merge)[1:3] <- c("SeqID","group","Type")
rownames(T_merge) <- T_merge[,1]

expr <- T_merge[,c(-1,-2,-3)]
anno <- T_merge[,2:3]

#加载包----
library(dplyr)
library(pheatmap)
##基础查看,纵列不聚类----
pheatmap(t(expr),annotation_col = anno,cluster_cols = F)

##调整顺序法二，通过hclust计算距离
col_dist <- dist(expr)
hclust_1 <- hclust(col_dist)
da <- T_merge %>% arrange(Type,hclust_1$order)
expr2 <- da[,c(-1,-2,-3)]
anno2 <- da[,2:3]
pheatmap(t(expr2),annotation_col = anno,cluster_cols = F)


##美化改颜色----
ann_colors <- list(group=c(IN="#00BE67",PY="#00a9ff",NRPS6="#FF61CC"),
                   Type=c(IN_PV="#f8766d",IN_SST="#cd9f00",IN_SV2C="#7cae00",IN_VIP="#00be67",
                          L23="#c77cff",L4="#00bfc4",L56CC="#00a9ff",NRPS6="#ff61cc")
)

pheatmap(t(expr2),annotation_col = anno2,cluster_cols = F,show_colnames = F,color=colorRampPalette(c("navy", "white", "firebrick3"))(100),
         annotation_colors = ann_colors,treeheight_row = 0,breaks=unique(c(seq(0,6,length=100))))
p <- pheatmap(t(expr2),annotation_col = anno2,cluster_cols = F,show_colnames = F,color=colorRampPalette(c("navy", "white", "firebrick3"))(100),
              annotation_colors = ann_colors,treeheight_row = 0,breaks=unique(c(seq(0,6,length=100))))
pdf("E:/R/Morpho2/20210928_likelydiseasecausing.pdf",width = 6,height = 4)
print(p)
dev.off()



##调整顺序法一----
A <- pheatmap(t(expr),annotation_col = anno)
col1 <- A$tree_col$order
row1 <- A$tree_row$order
da <- T_merge %>% arrange(Type,col1)
expr2 <- da[,c(-1,-2,-3)]
expr2 <- expr2[,row1]
anno2 <- da[,2:3]
pheatmap(t(expr2),annotation_col = anno2,cluster_cols = F,cluster_rows = F)
##美化改颜色----
ann_colors <- list(group=c(IN="#00BE67",PY="#00a9ff",NRPS6="#FF61CC"),
                   Type=c(IN_PV="#f8766d",IN_SST="#cd9f00",IN_SV2C="#7cae00",IN_VIP="#00be67",
                          L23="#c77cff",L4="#00bfc4",L56CC="#00a9ff",NRPS6="#ff61cc")
)
pheatmap(t(expr2),annotation_col = anno2,cluster_cols = F,cluster_rows = F, show_colnames = F,color=colorRampPalette(c("navy", "white", "firebrick3"))(100),
         annotation_colors = ann_colors)
p <- pheatmap(t(expr2),annotation_col = anno2,cluster_cols = F,cluster_rows = F, show_colnames = F,color=colorRampPalette(c("navy", "white", "firebrick3"))(100),
              annotation_colors = ann_colors)
pdf("E:/R/Morpho2/20210928_glutatamate.pdf",width = 6,height = 6)
print(p)
dev.off()


#去除分类树----
pheatmap(t(expr2),annotation_col = anno2,cluster_cols = F,show_colnames = F,color=colorRampPalette(c("navy", "white", "firebrick3"))(100),
         annotation_colors = ann_colors,treeheight_row = 0)


