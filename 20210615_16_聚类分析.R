Morpho <- read.csv("E:/R/PCA/20210608_cell89M2.csv",header = TRUE,row.names = 1)
Val <- as.matrix(colnames(Morpho))
Morpho2 <- Morpho[,c(18:22,24:28)]

#常用的聚类方法：层次聚类（hierarchical agglomerative clustering)和划分聚类（partitioning clustering)
#层次聚类，每一个观测值自成一类，这些类每次两两合并，直到所有的类被聚成一类为止
#层次聚类常见算法：单联动（single linkage）、全联动（complete linkage）、平均联动（average linkage）、质心（centroid)、和Ward方法
#划分聚类：首先指定类的个数K，然后观测值被随机分成K类，再重新形成聚合的类
#对于划分聚类来说，最常用的算法是K均值（K-means)和围绕中心点的划分（PAM)

#缩放数据
nMorpho2 <- apply(Morpho2,2,scale) #按列进行标准化
rownames(nMorpho2) <- rownames(Morpho2)
#其他数据处理方式
df1 <- apply(mydata,2,function(x){
  (x-mean(x))/sd(x)
}) #类似于scale函数
df2 <- apply(mydata,2,function(x){
  x/max(x)
})
df3 <- apply(mydata,2,function(x){
  (x-mean(x)/mad(x))
})

#聚类分析的一般步骤
#选择合适的变量
#缩放数据
#寻找异常点
#计算距离
#选择聚类算法，层次聚类对于小样本来说很实用（如150个观测值或更少
#获得一种或多种聚类方法
#确定类的数目
#获得最终的聚类解决方案
#结果可视化
#解读类
#验证结果

d <- dist(nMorpho2) #计算欧几里德聚类
as.matrix(d)[1:4,1:4] #观测值之间的距离越大，异质性越大

#层次聚类分析
#hclust(d,method=),其中d是通过dist()函数产生的距离矩阵，方法包括"single","complete","average","centroid","ward"
d <- dist(nMorpho2)
fit.average <- hclust(d,method = "average")
plot(fit.average,hang=-1,cex=.8,main="average linkage clustering")
#hang命令展示观测值的标签，让它们挂在0下面

#选择聚类的个数
library(NbClust)
devAskNewPage(ask = TRUE)
nc <- NbClust(nMorpho2, distance="euclidean",
              min.nc=2, max.nc=15, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#获取最终的聚类方法
clusters <- cutree(fit.average,k=5)
table(clusters)
aggregate(nMorpho2,by=list(cluster=clusters),median)
#尝试全联动层次聚类法#
out.dist <- dist(nMorpho2,method = "euclidean")
out.hclust <- hclust(out.dist,method = "complete")
out.id <- cutree(out.hclust,k=5)
table(out.id)
aggregate(Morpho2,by=list(cluster=out.id),median)
aggregate(as.data.frame(nMorpho2),by=list(cluster=out.id),median)
plot(out.hclust,hang=-1,cex=0.8,
     main = "Average linkage clustering\n5 cluster solution")
rect.hclust(out.hclust,k=5)
#cutree()函数用来把树状图分成五类，
#aggregate()函数用来获取每类的中位数，结果有原始度量和标准度量两种形式
#rect.hclust()函数用来显示五类的分类方法，树状图被重新绘制

#划分聚类分析
#在划分方法中，观测值被分为K组并根据给定的规则改组成最有粘性的类
#K均值聚类的算法如下，这种算法就是把观测值分成k组并使得观测值到指定的聚类中心的平方的总和为最小
#1.选择K个中心点（随机选择K行）
#2.把每个数据点分配到离它最近的中心点
#3.重新计算每类中的点到该类中心点距离的平均值（也就说，得到长度为p的均值向量，这里的p是变量的个数
#4.分配每个数据到它最近的中心点
#5.重复步骤3和4直到所有的观测值不再被分配或是达到最大的迭代次数（R把10次作为默认迭代次数

#判断选择多少类
#卵石弯曲图代码
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(nMorpho2) #确定聚类的个数，看拐点
library(NbClust)
set.seed(1234)
devAskNewPage(ask = TRUE)
nc <- NbClust(nMorpho2,min.nc = 2,max.nc = 15,method = "kmeans")
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]),
        xlab = "Number of clusters",ylab = "Number of Criteria",
        main = "Number of Clusters chosen by 26 Criteria")
#NbClust确定聚类的个数
set.seed(1234) #set.seed()函数可以保证结果是可复制的
fit.km <- kmeans(nMorpho2,5,nstart = 25)
#kmeans()函数有一个nstart选项尝试多种初始配置并输出最好的一个
fit.km$size
fit.km$centers
aggregate(Morpho2,by=list(cluster=fit.km$cluster),mean)
#展示数据结构
ct.km <- table(out.id,fit.km$cluster)
#可用flexclust包中的兰德指数（Rand index)来量化类型变量和类之间的协议
library(flexclust)
randIndex(ct.km)

#围绕中心点的划分，PAM
#因为K均值聚类方法是基于均值的，所以它对异常值是敏感的，一个更稳健的方法是PAM
#PAM算法如下
#1.随机选择K个观测值（每个都称为中心点
#2.计算观测值到各个中心的距离/相异性
#3.把每个观测值分配到最近的中心点
#4.计算每个中心点到每个观测值的距离的综合（总成本
#5.选择一个该类中不是中心的点，并和中心点呼唤
#6.重新把每个点分配到距它最近的中心点
#7.再次计算总成本
#8.如果总成本比步骤4计算的总成本少，把新的点作为中心点
#9.重复步骤5-8直到中心点不再改变

library(cluster)
set.seed(1234)
fit.pam <- pam(nMorpho2,k=2,stand=FALSE)
#pam(x,k,metric="euclidean",stand=FALSE),
# x表示数据矩阵或数据框，k表示聚类的个数，metric表示使用的相似性/相异性的度量
# stand是一个逻辑值，表示是否有变量应该在计算该指标前被标准化
fit.pam$medoids
clusplot(fit.pam,main="Bivariate Cluster Plot")

#检验无效的类
library(NbClust)
nc <- NbClust(nMorpho2,min.nc = 2,max.nc = 15,method = "kmeans")
plot(nc$All.index[,4], type="o", ylab="CCC",
     xlab="Number of clusters", col="blue")
#当CCC的值为负并且对于两类或更多的类递减时，就是典型的单峰分布











#作图
Morphoanno <- Morpho[,1:17]
Morphoanno$Kcluster <- as.factor(fit.km$cluster)
Morphoanno$Ocluster <- as.factor(out.id)
mycol<-colorRampPalette(c("blue","white","red"))(100)
library(pheatmap)
p1<-pheatmap(nMorpho2,color=mycol,border_color="white",show_rownames=F)
order <- p1$tree_row
range(nMorpho2)
bk=unique(c(seq(-4,4,length=100)))
ano_row <- data.frame(Morphoanno$Ocluster,Morphoanno$Kcluster,Morphoanno$cell.type,Morphoanno$cell.type2)
rownames(ano_row) <- rownames(Morphoanno)
p2<-pheatmap(nMorpho2,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row,breaks = bk)
p2
