install.packages(c("VIM","mice"))
Morpho <- read.csv("E:/R/PCA/20210608_cell89M2.csv",header = TRUE,row.names = 1)
#is.na(x),识别缺失值
#is.nan(x),识别不可能值
#is.infinite,识别无穷值

#列出没有缺失值的行
a <- Morpho[complete.cases(Morpho),]
#列出有一个或多个缺失值的行
b <- Morpho[!complete.cases(Morpho),]
#由于逻辑值TRUE和FALSE分别等价于1和0，可以用sum()和mean()函数来获取缺失数据的有用信息
sum(is.na(Morpho$rheobase.pA.)) #变量rheobase有16个缺失值
mean(is.na(Morpho$rheobase.pA.)) #18%的实例在此变量上有缺失值
mean(!complete.cases(Morpho)) #数据集中18%的实例包含一个或多个缺失值

Morpho2 <- Morpho[,29:41]
#列表显示缺失值
library(mice)
c <- md.pattern(Morpho2)
#图形探究缺失数据
library("VIM")
aggr(Morpho2,prop=FALSE,numbers=TRUE)
matrixplot(Morpho2)
marginplot(Morpho2[,1:2],pch = c(20))

#用相关性探索缺失值，影子矩阵，指示变量替代数据集中的数据（1表示缺失，0表示存在
x <- as.data.frame(abs(is.na(Morpho2)))


