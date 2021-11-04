#创建矩阵
mymatrix <- matrix(vector,nrow = 1,ncol=1,byrow=TRUE,dimnames = list(char_vector_rownames,char_vector_colnames))
y <- matrix(1:20,nrow=5,ncol=4)
cells <- c(1,26,24,68)
mymatrix <- matrix(cells,nrow=2,ncol = 2,byrow = TRUE,
                   dimnames = list(c("r1","r2"),c("c1","c2")))
set.seed(1234)
mymatrix <- matrix(rnorm(100),ncol=10)
x <- mymatrix
sums <- numeric(ncol(x))



#将常量变成矩阵
x <- c(1,2,3,4,5,6,7,8)
class(x)
print(x)
attr(x,"dim") <- c(2,4) #将x变成2行4列的矩阵 attr()函数允许你创建任意属性并将其与对象相关联
attributes(x)
attr(x,"dimnames") <- list(c("A1","A2"),
                           c("B1","B2","B3","B4"))
unclass(iris) #检查对象的内容
attributes(iris) #得到数据集的属性 $names,5
class(iris)
str(iris) #展示对象的结构
names(iris) #成分的名字
length() #对象包含多少成分
attributes() #检查对象的属性
head(iris)[1:3,1:5]

set.seed(1234)
fit <- kmeans(iris[1:4],3)
sapply(fit,class) #返回该类各个成分的对象
fit[c(2,7)]
fit[2] #得到列表
fit[[2]] #得到矩阵，等价于fit$centers

#画出K均值聚类的中心
set.seed(1234)
fit <- kmeans(iris[1:4],3)
means <- fit$centers
library(reshape2)
dfm <- melt(means)
names(dfm) <- c("Cluster","Measurement","Centermeters")
dfm$Cluster <- factor(dfm$Cluster)
head(dfm)
library(ggplot2)
ggplot(data = dfm,aes(x=Measurement,y=Centermeters,group=Cluster))+
  geom_point(size=3,aes(shape=Cluster,color=Cluster))+
  geom_line(size=1,aes(color=Cluster))+
  ggtitle("Profiles for Iris Clusters")

#ifelse() ifelse(test,yes,no)
pvalues <- c(0.0867,.0018,.0054,.1572,.0183,.5386)
results <- ifelse(pvalues<.05,"Significant","Not Significant")
#或者常规if语句
results <- vector(mode = "character",length=length(pvalues))
for(i in 1:length(pvalues)){
  if (pvalues[i]<.05) results[i] <- "Significant"
  else results[i] <- "Not Significant"
}

#创建函数
functionname <- function(parameters){
  statements
  return(value)
}
f <- function(x,y,z=1){
  result <- x+(2*y)+(3*z)
  return(result)
}
f(2,3,4)
f(2,3)
f(x=2,y=3)
f(z=4,y=2,3)
args(f) #观测参数的名字和默认值


