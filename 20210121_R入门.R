Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$cell_type.pre.annotated.,Total_anno$layer_type2) #Total_anno2提取group信息#
#id转换#
options(stringsAsFactors = F)
a <- read.table('E:/R/example/ensembl.txt',header = TRUE,sep = '\t')
a2 <- paste(a$ensembel,'.','1',sep = "")

a2 <- as.data.frame(a2)
strsplit("ENSG0000000003.13",'[.]')
strsplit("ENSG0000000003.13",'[.]')[[1]]
strsplit("ENSG0000000003.13",'[.]')[[1]][1]
library(stringr)
a2$ensembl_id <- str_split(a2$a2,'[.]',simplify=T)[,1]
library(org.Hs.eg.db)
g2s <- toTable(org.Hs.egSYMBOL)
g2e <- toTable(org.Hs.egENSEMBL)
colnames(a) <- "ensembl_id"
b <- merge(a,g2e,by='ensembl_id',all.x=T)
d <- merge(b,g2s,by='gene_id',all.x=T)
table(d$ensembl_id)[table(d$ensembl_id)>1]
d <- d[order(d$ensembl_id),]
d <- d[!duplicated(d$ensembl_id),]#去重#
d <- d[match(a$ensembl_id,d$ensembl_id),] #把d的顺序按照a的顺序排列#

#表达相关性#
a <- rnorm(10)
b <- 10*a+rnorm(10)
cor(a,b)

cor(Total[,2],Total[,3])
pheatmap::pheatmap(cor(Total[,2:236]))
tmp <- data.frame(g=Total_anno$group)
rownames(tmp) <- rownames(Total_anno)
pheatmap::pheatmap(cor(Total[,2:236]),annotation_col = tmp,annotation_row = tmp)
Total2 <- Total[,2:236]
rownames(Total2) <- Total$Genename
dim(Total2)
apply(Total2,1,function(x) sum(x>1)>5) #删去低表达基因，以及变异小的基因#
# x=Total2[1,]
# x>1 #TRUE#
# table(x>1) #TRUE的个数#
# sum(x>1) #TRUE的个数#
dim(Total2)

#差异分析#

#DEG by limma
suppressMessages(library(limma))
#基因芯片#
group_list <- as.character(Total_anno$group)
design <- model.matrix(~0+factor(group_list))
colnames(design) <- levels(factor(Total_anno$group))
rownames(design) <- colnames(Total2)
contrast.matrix <- makeContrasts(paste0(unique(group_list),collapse = "-"),levels = design)

#转录组#
BiocManager::install('airway')
library(airway)
data(airway)
exprSet <- assay(airway)
group_list <- colData(airway)[,3]
class(group_list)
exprSet <- exprSet[apply(exprSet,1,function(x) sum(x>1) > 5),]
table(group_list)
exprSet[1:4,1:4]

#方法一：DESeq2#













