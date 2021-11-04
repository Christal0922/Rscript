Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
df <- read.table("E:/R/patch/20210118_forGO.txt",header = T,sep = '\t')
genelist <- unique(as.vector(df[,1]))
library(clusterProfiler)
library(org.Hs.eg.db)
gene.df2 <- bitr(genelist, fromType = "SYMBOL", #fromType是指你的数据ID类型是属于哪一类的
                 toType = c("GENENAME"), #toType是指你要转换成哪种ID类型，可以写多种，也可以只写一种
                 OrgDb = org.Hs.eg.db)#Orgdb是指对应的注释包是哪个

DEG_find <- gene.df2$SYMBOL
DEG_matrix<-Total[match(DEG_find,Total$Genename),]
rownames(DEG_matrix)<-DEG_matrix$Genename  #将基因名字变成matrix的行名#
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
T_merge4 <- t(T_merge[,3:length(T_merge)])
T_merge4 <- data.frame(rownames(T_merge4),T_merge4)
colnames(T_merge4)[1] <- "SYMBOL"
T_merge5 <- merge(gene.df2,T_merge4,by="SYMBOL")
colnames(T_merge5)[1:2] <- c("Name","Description")
write.table(T_merge5,"E:/R/patch/20210126_expression.txt",row.names=F,sep = '\t')

#total
Total2 <- bitr(Total$Genename, fromType = "SYMBOL", #fromType是指你的数据ID类型是属于哪一类的
                 toType = c("GENENAME"), #toType是指你要转换成哪种ID类型，可以写多种，也可以只写一种
                 OrgDb = org.Hs.eg.db)
Total3 <- Total[match(Total2$SYMBOL,Total$Genename),]
Total4 <- merge(Total2,Total3,by.x="SYMBOL",by.y="Genename")
colnames(Total4)[1:2] <- c("Name","Description")
d <- data.frame(rownames(Total_anno),Total_anno$group)
rownames(d) <- rownames(Total_anno)
d1 <- t(d)
c1 <- matrix(data = NA,nrow = 2,ncol = 2)
d2 <- cbind(c1,d1)
colnames(d2)[1:2]<- c("Name","Description")
Total5 <- rbind(d2,Total4)
Total5[1:2,1] <- c("seqID","group")
Total5[1:2,2] <- c("Description","Description")
d3 <- Total5[,which(Total5[2,]!="IN")]
write.table(d3,"E:/R/patch/20210127_total.exprd2.txt",row.names=F,sep = '\t')

#只有RPS6和PY的细胞#
d <- data.frame(rownames(Total_anno),Total_anno$group)
rownames(d) <- rownames(Total_anno)
c1 <- matrix(data="NA",nrow = 2,ncol = 1)
d2 <- cbind(c1,d1)
colnames(d2)[1] <- "Genename"
Total6 <- rbind(d2,Total)
d3 <- Total6[,which(Total6[2,]!="IN")]
write.table(d3,"E:/R/patch/20210201_178cells.txt",row.names=F,sep = '\t')


#calculate#
T_merge2 <- aggregate(T_merge[,3:length(T_merge)],list(T_merge$Total_anno.group),mean) #按组计算均值#
rownames(T_merge2) <- c("IN","RPS6","PY")
T_merge2 <- T_merge2[,-1]
T_merge2[4,] <- colMeans(T_merge[,3:length(T_merge)])
rownames(T_merge2)[4] <- "expr"
T_merge2 <- t(T_merge2)
T_merge2 <- data.frame(rownames(T_merge2),T_merge2)
colnames(T_merge2)[1] <- "SYMBOL"
T_merge3 <- merge(gene.df2,T_merge2,by="SYMBOL")
T_merge3 <- T_merge3[,1:5]
write.table(T_merge3,"E:/R/patch/20210126_expr.txt",row.names=F,sep = '\t')

