Morpho <- read.csv("E:/R/PCA/20210608_cell89M2.csv",header = TRUE,row.names = 1)
Val <- as.matrix(colnames(Morpho))
Morpho2 <- Morpho[,c(18:22,24:28)]
#PCA，是一种数据降维技巧，它能将大量相关变量转化为一组很少的不相关变量，这些无关变量成为主成分
#PCA的目标是用一组较少的不相关变量代替相关变量，同时尽可能保持初始变量的信息，这些推到所得的变量称为主成分，它们是观测变量的线性组合

#判断主成分的个数
library(psych)

fa.parallel(Morpho2, fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")
#基于观测特征值的碎石检验（由线段和x符号组成）、根据100
# 个随机数据矩阵推导出来的特征值均值（虚线），以及大于1的特征值准则（y=1的水平线）
#Kaiser-Harris准则，建议保留特征值大于1的主成分，特征值小于1的成分所解释的方差比包含在单个变量中的方差更少
#Cattell碎石检验，绘制了特征值与主成分数的图形。这类图形可以清晰地展示图形弯曲状况，在图形变化最大处之上的主成分都可保留
#平行分析，依据与初始矩阵相同大小的随机数据矩阵来判断要提取的特征值。若基于真实数据的某个特征值大于一组随机数据矩阵相应的平均特征值，那么该主成分可以保留


#提取主成分
principal(Morpho2,nfactors = 1)
#PC1栏包含了成分载荷，指观测变量与主成分的相关系数
#h2栏指成分公因子方差，即主成分对每个变量的方差解释度
#u2栏指成分唯一性，即方差无法被主成分解释的比例（1-h2)
pc <- principal(Morpho2,nfactors = 2,scores = TRUE)
cor(Morpho2$Soma.surface.area,pc$scores) #探索相关系数
round(unclass(pc$weights),2) #获取主成分得分的系数

#EFA分析
#探索性因子分析EFA 是一系列用来发现一组变量的潜在结构的方法。它通过寻找一组更小的、潜在的或潜藏的结构来解释已观测到的、显式的变量间的关系
#EFA分析的目标是通过发掘隐藏在数据下的一组较少的、更为基本的无法观测的变量，来解释一组可观测变量的相关性
options(digits = 2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
#数据集ability.cov提供了变量的协方差矩阵，你可用cov2cor()函数将其转化为相关系数矩阵
library(psych)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
fa.parallel(correlations, n.obs=112, fa="both", n.iter=100,
              main="Scree plots with parallel analysis")




