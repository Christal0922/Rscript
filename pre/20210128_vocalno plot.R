#volcano plot#
DEGlist <- read.table("E:/R/patch/20210128_DEG.txt",sep = '\t',header = T)
logFC <- DEGlist$avg_logFC
adj <- DEGlist$p_val_adj
data <- data.frame(logFC=logFC,padj=adj)
rownames(data) <- rownames(DEGlist)
data$sig[(data$padj>0.05|data$padj=="NA")|(data$logFC<1)&(data$logFC>-1)] <- "no"
data$sig[data$padj<=0.05 & data$logFC>=1] <- "up"
data$sig[data$padj<=0.05 & data$logFC<=-1] <- "down"
x_lim <-max(logFC,-logFC)
library(ggplot2)
library(RColorBrewer)
theme_set(theme_bw())
p <- ggplot(data,aes(logFC,-1*log10(padj),color=sig))+
  geom_point()+
  xlim(-5,5)+labs(x="log2(FoldChange)",y="-log10(FDR)")
p <- p+scale_color_manual(values = c("#0072b5","grey","#bc3c28"))+
  geom_hline(yintercept = -log10(0.05),linetype=4)+
  geom_vline(xintercept = c(-1,1),linetype=4)
p <- p+theme(panel.grid = element_blank())+
  theme(axis.line = element_line(size = 0))+ylim(0,10)
p <- p+guides(colour=FALSE)
p <- p+theme(axis.text = element_text(size=20),axis.title = element_text(size = 20))
p
p2 <- p+geom_text(label=rownames(d2))
p2

pdf(file = "E:/R/Morpho2/vocalno.pdf",width = 4.5,height = 4)
print(p)
dev.off()


subset(DEGlist,Gene=="CCL2"|Gene=="MT-CO2"|Gene=="NFKBIA"|Gene=="CDKN1A")
which(DEGlist$Gene=="CCL2"|DEGlist$Gene=="MT-CO2"|DEGlist$Gene=="NFKBIA"|DEGlist$Gene=="CDKN1A")
data[which(DEGlist$Gene=="CCL2"|DEGlist$Gene=="MT-CO2"|DEGlist$Gene=="NFKBIA"|DEGlist$Gene=="CDKN1A"),]

Gene <- DEGlist$Gene
data2 <- data.frame(logFC=logFC,padj=adj,Gene=Gene)
data2[duplicated(data2$Gene),]
data2[6202,3] <- "None"
Gene2 <- c("CCL2","MT-CO2","NFKBIA","CDKN1A")

