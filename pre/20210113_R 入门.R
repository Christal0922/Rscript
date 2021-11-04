.libPaths() #查看包的安装位置#
tmp='abcde'
substring(temp,1,3) #取tmp 1至3个元素#
class(tmp) #查看数据类型#
str(tmp) #查看数据类型
as.numeric(unlist(lapply(d,length))) #计算list d中每个元素的长度并将其unlist化#
b <- matrix(1:10,nrow = 2)
b[1,]
b[c(T,F),] #以上两种描述类似，T表示TRUE,第一行取，F表示FALSE,第二行不取#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
index1 <- grep('RPS6',Total_anno$group) #取group 里面RPS6 的数据 对应的行名#
index2 <- grepl('RPS6',Total_anno$group) #取group 里面RPS6 的数据 对应 TRUE or FALSE#
b <- Total_anno[index1,]
c <- list(Total_anno,b,"a")
lapply(c,length)
index2 <- as.numeric(unlist(lapply(c,length)))>2
d <- c[index2]  #抓取变量大于2的结果#
class(d)
d[1][1]
d[[1]][1]
d[[1]][[1]] #取list 的元素，一般用双括号#

#外部数据导入#
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
#comment.char = '!', 以感叹号开头的通通不要#
write.csv(b,"E://",row.names = F)
save(b,file="b_input.Rdata")

#中等数据处理#
b <- sort(Total_anno$AST.FB.Score,decreasing = T)

#按照Score排序#
Total_anno2 <- na.omit(Total_anno)
max(Total_anno$AST.FB.Score)
fivenum(Total_anno$AST.FB.Score) #五分位数#
range(Total_anno2$rhe) #数据范围#
table(Total_anno2$rhe>100) #计数rhe>100的有多少个#
b <- Total_anno2[Total_anno2$rhe>100,] #取rhe>100的观察值#
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG_find <- c("SLC12A2","SLC12A5")
RPS6 <- Total_anno[which(Total_anno$group=="NRPS6"),]
RPS6 <- Total_anno[Total_anno$group=="NRPS6",]
Val <- data.frame(names(Total_anno2))
Ephys <- Total_anno2[,31:43]

#取平均值#
mean(Ephys[,1]) #对Ephys 第一列取平均#
mean(as.numeric(Ephys[,1]))

rowMeans(Ephys) #取行平均#

for (i in 1:ncol(Ephys)){
  print(mean(Ephys[,i]))
} #取列平均#

apply(Ephys,2,function(x){
  m <- mean(x)
  ax <- max(x)
  return(c(mean=m,max=ax))
}) #中间1表示行，2表示列

apply(Ephys,2,mean) #直接取平均值#

rowMax=function(x){
  apply(x,1,max)
} #定义取行平均的函数#
rowMax(Ephys)

#复杂计算#
for(i in 1:3){
  x=as.numeric(Ephys[i,])
  y=x[1]+x[2]-x[3]
  print(y)
}

#设计函数#
design <- function(x){
  for(i in 1:3){
    x=as.numeric(Ephys[i,])
    y=x[1]+x[2]-x[3]
    print(y)
  }
}
design(Ephys)


#其他计算#
table(Total_anno$cell_type.pre.annotated.,Total_anno$group) #计数
c <- as.data.frame(table(Total_anno$cell_type.pre.annotated.,Total_anno$group))
apply(Ephys,2,sd) #标准差#
sort(apply(Ephys,2,sd),decreasing = T) #排序#
sort(apply(Ephys,2,sd),decreasing = T)[1:3] # top3#
sample(1:nrow(Ephys),50) #随机取50个细胞#
pheatmap::pheatmap(Ephys[1:50,]) #取前50行细胞#
pheatmap::pheatmap(Ephys[sample(1:nrow(Ephys),50),])

rownames(Total2) <- Total$Genename
cg <- names(sort(apply(Total2,1,sd),decreasing = T)[1:50])
pheatmap::pheatmap(Total2[cg,])

#热图的绘制#
library(pheatmap)
a1 <- rnorm(100)
dim(a1) <- c(5,20) #5行20列#
pheatmap(a1)
a2 <- rnorm(100)+2
dim(a2) <- c(5,20)
pheatmap(a1,cluster_rows=F,cluster_cols=F) #不排序#
pheatmap(cbind(a1,a2))
b <- cbind(a1,a2)
b <- as.data.frame(cbind(a1,a2))
names(b) <- c(paste("a1",1:20,sep = "_"),paste("a2",1:20,sep = "_"))

