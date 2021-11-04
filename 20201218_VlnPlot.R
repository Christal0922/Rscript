Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG<-read.table("E:/R/patch/20210106_GSEAgene_down.txt",sep = '\t',header=T) #读入DEG list#
DEG_find<-DEG[DEG$Gene%in%Total$Genename,] #找到的DEG gene list#
DEG_notfound=DEG[!DEG$Gene%in%Total$Genename,] #没找到的gene list#
DEG_matrix<-Total[match(DEG_find,Total$Genename),] #大表里提取找到的gene的矩阵#
logexp<-data.frame(log2(1+DEG_matrix[,2:236])) #对数据进行取log#
rownames(logexp) <- DEG_matrix$Genename
logexp2 <- t(data.frame(logexp))
logexp2 <- data.frame(rownames(logexp2),logexp2)
colnames(logexp2)[1] <- "SeqID2"
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$group) #Total_anno2提取group信息#
colnames(Total_anno2)[1]<-"SeqID"
T_merge<-merge(Total_anno2,logexp2,by.x="SeqID",by.y="SeqID2") #合并Tlog以及Total_anno2, 使其具有group信息, 得到带分组类别的表达举证#
colnames(T_merge)[2] <- "group"


#批量输出VlnPlot#
pdf("E:/R/Morpho2/20210106_GSEAgene_down.pdf",width = 4.5,height = 4) 
attach(T_merge)
library(ggplot2)
for (i in 3:length(T_merge)){
  p <- ggplot(T_merge,aes(x=group,y=T_merge[,i]))+
    geom_violin(aes(fill=group),scale = "width")+
    scale_fill_manual(values=c("#00BE67","#FF61CC","#00a9ff"))+
    geom_jitter(shape=16,size=1.5,fill="black",position=position_jitter(width=0.4))+
    ylab("Expression Level")+labs(title=DEG_find[i-2])+
    theme_bw()+theme(plot.title = element_text(face="bold",size=14,hjust=0.5),
                     axis.title = element_text(size = 14),
                     axis.line.x = element_line(size = 1),
                     axis.line.y = element_line(size = 1),
                     axis.text.x = element_text(size = 12,angle = 45,hjust = 0.8),
                     axis.text.y = element_text(size = 12,vjust=0.8),
                     legend.position="none",panel.grid=element_blank(),panel.border = element_blank(), axis.line = element_line())
  print (p)
  i=i+1
}
detach(T_merge)
dev.off()






#输出少数几个gene#
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG_find <- c("SLC12A2","SLC12A5")
DEG_matrix<-Total[match(DEG_find,Total$Genename),] #大表里提取找到的gene的矩阵#
logexp<-data.frame(log2(1+DEG_matrix[,2:236])) #对数据进行取log#
rownames(logexp) <- DEG_matrix$Genename
logexp2 <- t(data.frame(logexp))
logexp2 <- data.frame(rownames(logexp2),logexp2)
colnames(logexp2)[1] <- "SeqID2"
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$cell_type.pre.annotated.,Total_anno$layer_type2) #Total_anno2提取group信息#
colnames(Total_anno2)<-c("SeqID","Cell_Annotation","Layer")
T_merge<-merge(Total_anno2,logexp2,by.x="SeqID",by.y="SeqID2") #合并Tlog以及Total_anno2, 使其具有group信息, 得到带分组类别的表达举证#

#VlnPlot#
pdf("E:/R/Morpho2/20201221_Vlnplot.pdf",width = 4.5,height = 4) 
library(ggplot2)
attach(T_merge)
p1 <- ggplot(T_merge,aes(x=group,y=CDKN1A))+
  geom_violin(aes(fill=group),scale = "width")+
  scale_fill_manual(values=c("#00BE67","#FF61CC","#00a9ff"))+
  geom_jitter(shape=16,size=1.5,fill="black",position=position_jitter(width=0.4))+
  ylab("Expression Level")+labs(title="CDKN1A")+
  theme_bw()+theme(plot.title = element_text(face="bold",size=14,hjust=0.5),
                   axis.title = element_text(size = 14),
                   axis.line.x = element_line(size = 1),
                   axis.line.y = element_line(size = 1),
                   axis.text.x = element_text(size = 12,angle = 45,hjust = 0.8),
                   axis.text.y = element_text(size = 12,vjust=0.8),
                   legend.position="none",panel.grid=element_blank(),panel.border = element_blank(), axis.line = element_line())
print(p1)
detach(T_merge)
dev.off()
