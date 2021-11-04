Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG<-read.table("E:/R/patch/20210112_glutamatergic.txt",sep = '\t',header=T) #读入DEG list#
DEG_find<-DEG[DEG$Gene%in%Total$Genename,] #找到的DEG gene list#
DEG_notfound=DEG[!DEG$Gene%in%Total$Genename,] #没找到的gene list#
DEG_matrix<-Total[match(DEG_find,Total$Genename),] #大表里提取找到的gene的矩阵#
logexp<-data.frame(log2(1+DEG_matrix[,2:236])) #对数据进行取log#
rownames(logexp) <- DEG_matrix$Genename
logexp2 <- t(data.frame(logexp))
logexp2 <- data.frame(rownames(logexp2),logexp2)
colnames(logexp2)[1] <- "SeqID2"
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$cell_type.pre.annotated.,Total_anno$layer_type2) #Total_anno2提取group信息#
colnames(Total_anno2)[1]<-"SeqID"
T_merge<-merge(Total_anno2,logexp2,by.x="SeqID",by.y="SeqID2") #合并Tlog以及Total_anno2, 使其具有group信息, 得到带分组类别的表达举证#
colnames(T_merge)[2:3] <- c("Cell_Annotation","Layer")

#筛选类别#
T_DN<-T_merge[which(T_merge$Cell_Annotation== 'DN'), ]
T_ULDN <- T_merge[which((T_merge$Cell_Annotation== 'DN')&(T_merge$Layer=="upper")), ]
T_DLDN <- T_merge[which((T_merge$Cell_Annotation== 'DN')&(T_merge$Layer=="deeper")), ]
T_PY<-T_merge[which(T_merge$Cell_Annotation== 'PY'), ]
T_ULPY <- T_merge[which((T_merge$Cell_Annotation== 'PY')&(T_merge$Layer=="upper")), ]
T_DLPY <- T_merge[which((T_merge$Cell_Annotation== 'PY')&(T_merge$Layer=="deeper")), ]

#计算表达量#
mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)/sqrt(n)
  return(c(mean=m,se=s,n=n))
} #设计统计函数#
#Anno1#
DN1 <- sapply(T_DN[,4:99],mystats) #对T_DN按列计算统计值#
DN1 <-data.frame(t(DN1))
colnames(DN1)<-c("DN.mean","DN.se","DN.n")
PY1 <- sapply(T_PY[,4:99],mystats) #对T_DN按列计算统计值#
PY1 <-data.frame(t(PY1))
colnames(PY1)<-c("PY.mean","PY.se","PY.n")
Anno1 <- data.frame(rownames(DN1),DN1,PY1)
write.table(Anno1,file="E:/R/patch/20210112_Anno1.txt",row.names=F,quote=F,sep='\t')

#Anno2#
ULDN1 <- sapply(T_ULDN[,4:99],mystats) #对T_DN按列计算统计值#
ULDN1 <-data.frame(t(ULDN1))
colnames(ULDN1)<-c("ULDN.mean","ULDN.se","ULDN.n")
DLDN1 <- sapply(T_DLDN[,4:99],mystats) #对T_DN按列计算统计值#
DLDN1 <-data.frame(t(DLDN1))
colnames(DLDN1)<-c("DLDN.mean","DLDN.se","DLDN.n")
ULPY1 <- sapply(T_ULPY[,4:99],mystats) #对T_DN按列计算统计值#
ULPY1 <-data.frame(t(ULPY1))
colnames(ULPY1)<-c("ULPY.mean","ULPY.se","ULPY.n")
DLPY1 <- sapply(T_DLPY[,4:99],mystats) #对T_DN按列计算统计值#
DLPY1 <-data.frame(t(DLPY1))
colnames(DLPY1)<-c("DLPY.mean","DLPY.se","DLPY.n")
Anno2 <- data.frame(rownames(ULDN1),ULDN1,DLDN1,ULPY1,DLPY1)
write.table(Anno2,file="E:/R/patch/20210112_Anno2.txt",row.names=F,quote=F,sep='\t')

#DEG_2#
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG_find<-c("CACNA1A","CACNA1E","KCNC1","KCND3","KCNJ3","KCNJ6","KCNK1","SCN3A","TRPM2","TRPM7")
DEG_matrix<-Total[match(DEG_find,Total$Genename),] #大表里提取找到的gene的矩阵#
logexp<-data.frame(log2(1+DEG_matrix[,2:236])) #对数据进行取log#
rownames(logexp) <- DEG_matrix$Genename
logexp2 <- t(data.frame(logexp))
logexp2 <- data.frame(rownames(logexp2),logexp2)
colnames(logexp2)[1] <- "SeqID2"
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$cell_type.pre.annotated.,Total_anno$layer_type2) #Total_anno2提取group信息#
colnames(Total_anno2)[1]<-"SeqID"
T_merge<-merge(Total_anno2,logexp2,by.x="SeqID",by.y="SeqID2") #合并Tlog以及Total_anno2, 使其具有group信息, 得到带分组类别的表达举证#
colnames(T_merge)[2:3] <- c("Cell_Annotation","Layer")

#T_merge#
merge <- T_merge[which((T_merge$Cell_Annotation== 'PY')|(T_merge$Cell_Annotation== 'DN')), ]

#Vlnplot#
#批量输出VlnPlot#
pdf("E:/R/Morpho2/20210127_ca.pdf",width = 4.5,height = 4) 
attach(merge)
library(ggplot2)
for (i in 4:length(merge)){
  p <- ggplot(merge,aes(x=Cell_Annotation,y=merge[,i]))+
    geom_violin(aes(fill=Cell_Annotation),scale = "width")+
    scale_fill_manual(values=c("#F08CAA","#6482FF"))+
    geom_jitter(shape=16,size=1.5,fill="black",position=position_jitter(width=0.4))+
    ylab("Expression Level")+labs(title=DEG_find[i-3])+
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
detach(merge)
dev.off()

#merge2#
merge2 <- merge[which((merge$Layer=="upper")|(merge$Layer=="deeper")), ]
merge2$Cell_Annotation2 <- paste0(merge2$Layer,"_",merge2$Cell_Annotation)
#批量输出VlnPlot#
pdf("E:/R/Morpho2/20210113_CA2.pdf",width = 4.5,height = 4) 
attach(merge2)
library(ggplot2)
for (i in 4:14){
  p <- ggplot(merge2,aes(x=Cell_Annotation2,y=merge2[,i]))+
    geom_violin(aes(fill=Cell_Annotation2),scale = "width")+
    scale_fill_manual(values=c("#C81E50","#001E96","#F08CAA","#6482FF"))+
    geom_jitter(shape=16,size=1.5,fill="black",position=position_jitter(width=0.4))+
    ylab("Expression Level")+labs(title=DEG_find[i-3])+
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
detach(merge2)
dev.off()


