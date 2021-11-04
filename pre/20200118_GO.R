library(clusterProfiler)
library(org.Hs.eg.db) #人源#
df <- read.table("E:/R/patch/20210118_forGO.txt",header = T,sep = '\t')
genelist <- unique(as.vector(df[,1]))
gene.df <- bitr(genelist, fromType = "SYMBOL", #fromType是指你的数据ID类型是属于哪一类的
                toType = c("ENSEMBL", "ENTREZID"), #toType是指你要转换成哪种ID类型，可以写多种，也可以只写一种
                OrgDb = org.Hs.eg.db)#Orgdb是指对应的注释包是哪个
go <- enrichGO(genelist,OrgDb = org.Hs.eg.db,ont='ALL',
               pAdjustMethod = 'BH',pvalueCutoff = 0.05,
               qvalueCutoff = 0.2,keyType = 'SYMBOL',pool = T)
#富集分析#
go_result <- go@result
go_result$GeneRatio2 <- paste("0",go_result$GeneRatio,sep = " ")
go_result$log10padj <- log10(go_result$p.adjust)
write.table(go_result,"E:/R/Morpho2/20210118_patch-seq_up_go.txt",row.names = T,sep = '\t')
#可视化#
p1 <- barplot(go,showCategory = 20)
p1
BP <- go_result[grep('BP',go_result$ONTOLOGY),]
BP <- as.matrix(BP)

#KEGG#
trid <- bitr(genelist,fromType = "SYMBOL",toType = c("ENTREZID"),OrgDb = "org.Hs.eg.db")
genelist2 <- as.vector(trid$ENTREZID)
kegg <- enrichKEGG(genelist2,organism = 'hsa',keyType = 'kegg',
                   pvalueCutoff = 0.05,pAdjustMethod = 'BH',qvalueCutoff = 0.2)
kegg_result <- kegg@result
write.table(kegg_result,"E:/R/patch/20210120_kegg.txt",row.names = F,sep = '\t')
emapplot(kegg) #网络图展示各富集功能之间共有基因关系
heatplot(kegg) #热图展示富集功能和基因的包含关系
cnetplot(kegg) #网络图展示富集功能和基因的包含关系

y <- setReadable(kegg,OrgDb = org.Hs.eg.db,keyType = "ENTREZID") #将ENTREZID 转换成SYMBOL#
y_result <- y@result
write.table(y_result,"E:/R/patch/20210120_y_result.txt",row.names = F,sep = '\t')


#数据可视化#
install.packages('GOplot')
install_github('wencke/wencke.github.io')

#david#
david2 <- data.frame(go_result$ONTOLOGY,go_result$ID,go_result$Description,
                    go_result$geneID,go_result$p.adjust)
colnames(david2) <- c("Category","ID","Term","Genes","adj_pval")
write.table(david2,"E:/R/Morpho2/20210118_david2.txt",row.names = F,sep = '\t')

#calculate#
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG_matrix<-Total[match(genelist,Total$Genename),] #大表里提取找到的gene的矩阵#
rownames(DEG_matrix)<-DEG_matrix$Genename  #将基因名字变成matrix的行名#
a<-DEG_matrix[,2:236] #提取数据#
rownames(a)<-rownames(DEG_matrix) #行名赋值#
log2c<-data.frame(log2(1+a)) #对数据加1取log2#
d<-data.frame(rownames(log2c),log2c) #将log2c变成data.frame#
Tlog<-t(d[,2:236]) #对无均值的进行log处理的矩阵进行转置#
Tlog<-data.frame(rownames(Tlog),Tlog) #将Tlog#变成data.frame
colnames(Tlog)[1]<-"SeqID2" #将Tlog第一列的列名改成SeqID2#
Total_anno<-read.table("E:/R/patch/20201028_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$group) #Total_anno2提取group信息#
colnames(Total_anno2)[1]<-"SeqID"
T_merge<-merge(Total_anno2,Tlog,by.x="SeqID",by.y="SeqID2")
T_merge2 <- aggregate(T_merge[,3:241],list(T_merge$Total_anno.group),mean) #按组计算均值#
rownames(T_merge2) <- c("IN","RPS6","PY")
T_merge2 <- T_merge2[,-1]
T_merge2[4,] <- colMeans(T_merge[,3:241])
rownames(T_merge2)[4] <- "expr"
T_merge2 <- t(T_merge2)
T_merge2 <- data.frame(rownames(T_merge2),T_merge2)

#genelist#
genelist2 <- data.frame(df$gene,df$avg_logFC,T_merge2$expr,df$p_val,df$p_val_adj)
colnames(genelist2) <- c("ID","logFC","AveExpr","P.Value","adj.P.Val")
write.table(genelist2,"E:/R/Morpho2/20210118_genelist2.txt",row.names = F,sep = '\t')

#eset#
eset2 <- data.frame(T_merge2[,1:4])
colnames(eset2)[1] <- "Gene_Symbol"
write.table(eset2,"E:/R/Morpho2/20210118_eset2.txt",row.names = F,sep = '\t')

#genes#
genes2 <- data.frame(df$gene,df$avg_logFC)
colnames(genes2) <- c("ID","logFC")
write.table(genes2,"E:/R/Morpho2/20210118_genes2.txt",row.names = F,sep = '\t')

#process#
process2 <- c("cell chemotaxis","antigen processing and presentation",
              "inflammatory responses")

#GOplot#
david2 <- read.table("E:/R/Morpho2/20210118_david3.txt",header = TRUE,sep = '\t')
genelist2 <- read.table("E:/R/Morpho2/20210118_genelist2.txt",header = TRUE,sep = '\t')
genes2 <- read.table("E:/R/Morpho2/20210118_genes2.txt",header = TRUE,sep = '\t')
eset2 <- read.table("E:/R/Morpho2/20210118_eset2.txt",header = TRUE,sep = '\t')
process2 <- c("interferon-gamma-mediated signaling pathway",
              "antigen processing and presentation","positive regulation of immune effector process",
              "chemokine-mediated signaling pathway",
              "response to lipopolysaccharide","response to interleukin-1")

EC2 <- list(david2,genelist2,eset2,genes2,process2)
names(EC2) <- c("david2","genelist2","eset2","genes2","process2")

library(GOplot)

circ <- circle_dat(EC2$david2,EC2$genelist2)
reduced_circ <- reduce_overlap(circ, overlap = 0.75)

circ <- circ[order(circ$adj_pval),]
d <- which(circ$adj_pval<1e-20)
circ3 <- circ[1:872,]

#作图#
p1 <- GOBar(reduced_circ, display = 'multiple', title = 'Z-score coloured barplot')
p1
d <- p1$data
reduced_circ <- reduce_overlap(circ, overlap = 0.9)
p2 <- GOBubble(reduced_circ, labels = 10,display = 'multiple', bg.col = T)
p2
p3 <- GOCircle(reduced_circ, nsub = 10)
p3
# Generate a circular visualization of selected terms
IDs <- c('GO:0007507', 'GO:0001568', 'GO:0001944', 'GO:0048729', 'GO:0048514', 'GO:0005886', 'GO:0008092', 'GO:0008047')
IDs <- b$id
p3.2 <- GOCircle(circ3, nsub = IDs)
p3.2

process3 <- reduced_circ$term
chord <- chord_dat(circ, EC2$genes2, EC2$process2)
p4 <- GOChord(chord, space = 0.02, gene.order = 'logFC', gene.space = 0.25, gene.size = 3)
p4
write.table(d,"E:/R/patch/20210120_GOp12.txt",sep = '\t',row.names = F)
pdf("E:/R/Morpho2/20210120_GO.pdf",width = 8,height = 6)
print(p1)
print(p2)
print(p4)
dev.off()
pdf("E:/R/Morpho2/20210120_GOchord.pdf",width = 8,height = 9)
print(p4)
dev.off()

#基因功能注释#
library(org.Hs.eg.db)
genelist1 <- gene.df$ENTREZID
gene.df2 <- bitr(genelist, fromType = "SYMBOL", #fromType是指你的数据ID类型是属于哪一类的
                toType = c("GENENAME"), #toType是指你要转换成哪种ID类型，可以写多种，也可以只写一种
                OrgDb = org.Hs.eg.db)#Orgdb是指对应的注释包是哪个
head(gene.df2)

