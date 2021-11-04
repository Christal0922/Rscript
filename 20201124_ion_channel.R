Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG<-read.table("E:/R/patch/20201124_ion_channel.txt",sep = '\t',header=T) #读入DEG list#
DEG_find<-DEG[DEG$Gene%in%Total$Genename,] #找到的DEG gene list#
DEG_notfound=DEG[!DEG$Gene%in%Total$Genename,] #没找到的gene list#
DEG_matrix<-Total[match(DEG_find$Gene,Total$Genename),] #大表里提取找到的gene的矩阵#
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
T_merge<-merge(Total_anno2,Tlog,by.x="SeqID",by.y="SeqID2") #合并Tlog以及Total_anno2, 使其具有group信息#
T_IN<-T_merge[which(T_merge$Total_anno.group== 'IN'), ] #筛选出IN的数据#
T_NRGN<-T_merge[which(T_merge$Total_anno.group== 'NRGN'), ] #筛选出NRGN的数据#
T_PY<-T_merge[which(T_merge$Total_anno.group== 'PY'), ] #筛选出PY的数据#
mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)/sqrt(n)
  return(c(mean=m,se=s,n=n))
} #设计统计函数#
PY1 <- sapply(T_PY[,3:162],mystats) #对T_PY按列计算统计值#
IN1 <- sapply(T_IN[,3:162],mystats)
NRGN1 <- sapply(T_NRGN[,3:162],mystats)
PY1 <-data.frame(t(PY1))
IN1 <-data.frame(t(IN1))
NRGN1 <-data.frame(t(NRGN1))
colnames(PY1)<-c("PY.mean","PY.se","PY.n") #标记PY的均值，标准误，个数等#
colnames(IN1)<-c("IN.mean","IN.se","IN.n")
colnames(NRGN1)<-c("NRGN.mean","NRGN.se","NRGN.n")
ION_2<-data.frame(DEG_find,IN1,NRGN1,PY1)
write.table(ION_2,file="E:/R/patch/20201124_IONchannel.txt",row.names=F,quote=F,sep='\t')
