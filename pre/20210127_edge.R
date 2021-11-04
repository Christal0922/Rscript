Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=1,sep = '\t',header=T) #读入全部细胞基因表达list#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
group <- Total_anno$cell_type.pre.annotated.
Total <- as.matrix(Total)
group2 <- group[which(group=='DN'|group=='PY')]
Total2 <- Total[,which(group=='DN'|group=='PY')]
library(edgeR)
dgelist <- DGEList(counts = Total2,group = group2)
dgelist_norm <- calcNormFactors(dgelist,method = 'TMM')
design <- model.matrix(~group2)
dge <- estimateDisp(dgelist_norm,design,robust = TRUE)
plotBCV(dge)
fit <- glmFit(dge,design,robust=TRUE)
lrt <- glmLRT(fit)
topTags(lrt)
write.csv(topTags(lrt,n=nrow(dgelist$counts)),'E:/R/PCA/20210127_glmLRT.csv',quote = FALSE)
dge_de <- decideTestsDGE(lrt,adjust.method = 'fdr',p.value = 0.05)
summary(dge_de)
dge_et <- exactTest(dge)
topTags(dge_et)
write.csv(topTags(dge_et,n=nrow(dgelist$counts)),'E:/R/PCA/20210127_exactTest.csv',quote = FALSE)
dge_de <- decideTestsDGE(dge_et,adjust.method = 'fdr',p.value = 0.05)
summary(dge_de)

#DESeq2#
gene<-read.delim("E:/R/patch/20201124_norm.235cells.txt",row.names=1,sep = '\t',header=T,stringsAsFactors = FALSE,check.names = FALSE) #读入全部细胞基因表达list#
all <- apply(gene,1,function(x) all(x==0))
newdata <- gene[!all,]
colData <- data.frame(Total_anno$cell_type.pre.annotated.)
colData <- data.frame(row.names = colnames(gene),colData)

#DEGlist#
DEG <- topTags(lrt,n=nrow(dgelist$counts))
DEGlist <- DEG$table
DEGlist2 <- DEGlist[which(DEGlist$logFC<=-1|DEGlist$logFC>=1),]
DEGlist2 <- DEGlist2[which(DEGlist2$FDR<0.05),]
#vocalno plot#

logFC <- DEGlist$logFC
adj <- DEGlist$FDR
data <- data.frame(logFC=logFC,padj=adj)
rownames(data) <- rownames(DEGlist)
data$sig[(data$padj>0.05|data$padj=="NA")|(data$logFC<0.5)&(data$logFC>-0.5)] <- "no"
data$sig[data$padj<=0.05 & data$logFC>=0.5] <- "up"
data$sig[data$padj<=0.05 & data$logFC<=-0.5] <- "down"
x_lim <-max(logFC,-logFC)
library(ggplot2)
library(RColorBrewer)
theme_set(theme_bw())
p <- ggplot(data,aes(logFC,-1*log10(padj),color=sig))+
  geom_point()+
  xlim(-5,5)+labs(x="log2(FoldChange",y="-log10(FDR)")
p <- p+scale_color_manual(values = c("#0072b5","grey","#bc3c28"))+
  geom_hline(yintercept = -log10(0.05),linetype=4)+
  geom_vline(xintercept = c(-0.5,0.5),linetype=4)
p <- p+theme(panel.grid = element_blank())+
  theme(axis.line = element_line(size = 0))+ylim(0,8)
p <- p+guides(colour=FALSE)
p <- p+theme(axis.text = element_text(size=20),axis.title = element_text(size = 20))
p

pdf(file = "E:/R/Morpho2/vocalno.pdf",width = 8,height = 8)
print(p)
dev.off()

#pheatmap#
data2 <- data[order(data$logFC),]
data3 <- data2[c(1:20,which(data2$sig=="up")),]
DEGlist3 <- rownames(data3)
matrix1 <- Total2[match(DEGlist3,rownames(Total2)),]
group2 <- as.data.frame(group2)
rownames(group2) <- colnames(Total2)
range(matrix1)
median(matrix1)
matrix2 <- apply(matrix1,1,scale)
rownames(matrix2) <- colnames(matrix1)
colnames(matrix2) <- rownames(matrix1)
library(pheatmap)
mycol<-colorRampPalette(c("blue","white","red"))(100)
bk=unique(c(seq(-3,3,length=100)))
p1 <- pheatmap(matrix2,color=mycol,border_color=NA,annotation_col = group2,show_colnames=F,show_rownames=F)
p1



