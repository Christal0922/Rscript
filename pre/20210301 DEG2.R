Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
DEG <- read.table("patch/20210302_Kv2.txt",header = T)

DEG_find<-DEG[DEG$Gene%in%Total$Genename,]
DEG_notfound <- DEG[!DEG$Gene%in%Total$Genename,]
DEG_matrix<-Total[match(DEG_find,Total$Genename),]
rownames(DEG_matrix)<-DEG_matrix$Genename

a<-DEG_matrix[,2:236] #提取数据#
rownames(a)<-rownames(DEG_matrix) #行名赋值#
log2c<-data.frame(log2(1+a)) #对数据加1取log2#
d<-data.frame(rownames(log2c),log2c) #将log2c变成data.frame#
Tlog<-t(d[,2:236]) #对无均值的进行log处理的矩阵进行转置#
Tlog<-data.frame(rownames(Tlog),Tlog) #将Tlog#变成data.frame
colnames(Tlog)[1]<-"SeqID2"
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$group) #Total_anno2提取group信息#
colnames(Total_anno2)[1]<-"SeqID"
T_merge<-merge(Total_anno2,Tlog,by.x="SeqID",by.y="SeqID2")
rownames(T_merge) <- T_merge$SeqID
#设置计算公式#
mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)/sqrt(n)
  return(c(mean=m,se=s,n=n))
}

#计算mystats#
T_IN<-T_merge[which(T_merge$Total_anno.group== 'IN'), ] #筛选出IN的数据#
T_NRGN<-T_merge[which(T_merge$Total_anno.group== 'NRPS6'), ] #筛选出NRGN的数据#
T_PY<-T_merge[which(T_merge$Total_anno.group== 'PY'), ] #筛选出PY的数据#
PY1 <- sapply(T_PY[,3:length(T_PY)],mystats) #对T_PY按列计算统计值#
IN1 <- sapply(T_IN[,3:length(T_IN)],mystats)
NRGN1 <- sapply(T_NRGN[,3:length(T_NRGN)],mystats)
PY1 <-data.frame(t(PY1))
IN1 <-data.frame(t(IN1))
NRGN1 <-data.frame(t(NRGN1))
colnames(PY1)<-c("PY.mean","PY.se","PY.n") #标记PY的均值，标准误，个数等#
colnames(IN1)<-c("IN.mean","IN.se","IN.n")
colnames(NRGN1)<-c("RPS6.mean","RPS6.se","RPS6.n")
ION_2<-data.frame(rownames(IN1),IN1,NRGN1,PY1)

write.csv(ION_2,file="E:/R/patch/20210302_k2.csv",row.names=F)




#计算mean#
T_merge2 <- aggregate(T_merge[,3:length(T_merge)],list(T_merge$Total_anno.group),mean) #按组计算均值#
rownames(T_merge2) <- c("IN","RPS6","PY")
T_merge2 <- T_merge2[,-1]