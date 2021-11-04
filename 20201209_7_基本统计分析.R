#7.基本统计分析#
myvars <- c("mpg", "hp", "wt")
summary(mtcars[myvars])
# summary()函数提供了最小值、最大值、四分位数和数值型变量的均值

sapply(x,FUN,options)
#x指数据框或矩阵，FUN函数，如果指定了options,它们将被传递给FUN#
#FUN:eg, mean(),sd(),var(),min(),max(),median(),length(),range(),quantile()#
mydata <- matrix(rnorm(30),nrow=6)
apply(array,Margin,FUN)
apply(mydata,1,mean)#计算每行的均值#
apply(mydata,2,mean)#计算每列的均值#
#lapply(),sapply()则可将函数应用到列表list上#

mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)] #若x为缺失值，则返回空值#
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n-3
  return(c(n=n,mean=m,stdev=s,skew=skew,kurtosis=kurt))
  
}
#skew,偏度，kurt 峰度#
myvars <- c("mpg","hp","wt")
sapply(mtcars[myvars],mystats)
#若希望忽略缺失值#
sapply(mtcars[myvars],mystats,na.omit=TRUE)

#其他方法#
library(Hmisc)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])

#pastecs包的stat.desc()函数#
stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
# 其中的x是一个数据框或时间序列。若basic=TRUE（默认值），则计算其中所有值、空值、缺失
# 值的数量，以及最小值、最大值、值域，还有总和。若desc=TRUE（同样也是默认值），则计算
# 中位数、平均数、平均数的标准误、平均数置信度为95%的置信区间、方差、标准差以及变异系
# 数。最后，若norm=TRUE（不是默认的），则返回正态分布统计量，包括偏度和峰度（以及它们
# 的统计显著程度）和Shapiro-Wilk正态检验结果。这里使用了p值来计算平均数的置信区间（默认
# 置信度为0.95）。

#psych包的describe()函数#
# 计算非缺失值的数量、平均数、标准差、中位数、截尾均值、绝对中位差、最小值、最大值、值域、偏度、峰度和平均
# 值的标准误

#使用aggregate()分组获取描述性统计量#
myvars <- c("mpg","hp","wt")
aggregate(mtcars[myvars],by=list(am=mtcars$am),mean)
# 如果有多个分组变量，可以使用by=list(name1=groupvar1, name2=groupvar2, ... , nameN=groupvarN)这样的语句

mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)/sqrt(n)
  return(c(n=n,mean=m,se=s))
} #设计统计函数#

by(data,INDICES,FUN)
# data是一个数据框或矩阵，INDICES是一个因子或因子组成的列表，定义了分组，FUN是任意函数
dstats <- function(x)sapply(x, mystats)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], mtcars$am, dstats)
# dstats()调用了mystats()函数，将其应用于数据框的每一栏中。
# 再通过by()函数则可得到am中每一水平的概括统计量

library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars,FUN=mystats)

library(psych)
myvars <- c("mpg", "hp", "wt")
describeBy(mtcars[myvars], list(am=mtcars$am))

#频数表和列联表#
library(vcd)
head(Arthritis)
mytable <- with(Arthritis, table(Improved))
prop.table(mytable)
prop.table(mytable)*100

#二维列联表#
mytable <- table(A, B)
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
prop.table(mytable, 1)
addmargins(mytable)
addmargins(prop.table(mytable, 1), 2)

addmargins(prop.table(mytable, 2), 1)
# table()函数默认忽略缺失值（NA）。要在频数统计中将NA视为一个有效的类别，请设定
# 参数useNA="ifany"

#三维列联表#
library(vcd)
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis) #treatment需要用下标1来引用，Sex通过下标2来引用，Improve通过下标3来引用#
ftable(mytable)
margin.table(mytable, 1) #边际频数#
margin.table(mytable, c(1,3)) #边际频数#
ftable(prop.table(mytable,c(1,2))) #治疗情况 改善情况的边际频数#
ftable(addmargins(prop.table(mytable,c(1,2)),3)) #治疗情况，性别的各类改善比例#
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100

#卡方独立性检验#
chisq.test()
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)
#X-squared = 13.055, df = 2, p-value = 0.001463 治疗情况和改善情况不独立#

mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)
#X-squared = 4.8407, df = 2, p-value = 0.08889， 性别和改善情况独立#
# 这里的p值表示从总体中抽取的样本行变量与列变
# 量是相互独立的概率。由于1的概率值很小，所以你拒绝了治疗类型和治疗结果相互独立的原假
# 设。由于2的概率不够小，故没有足够的理由说明治疗结果和性别之间是不独立的。代码清单7-12
# 中产生警告信息的原因是，表中的6个单元格之一（男性－一定程度上的改善）有一个小于5的值，
# 这可能会使卡方近似无效。

#创建一个table#
b <- c(0,2,8,21,4,4,10,22,15,3)
dim(b) <- c(5, 2) #将b转换成矩阵，5行2列#
rownames1 <- c("M1","M2","M3","M4","M5") #rownames1 <- c(paste("M",seq(1:5),sep = ""))#
colnames1 <- c("deeper","upper")
dimnames(b) <- list(rownames1, colnames1)

ML <- matrix(c(0,2,8,21,4,4,10,22,15,3), nrow = 5, ncol = 2,
             dimnames = list(c("M1","M2","M3","M4","M5"),
                             c("deeper","upper"))) #法2#

b <- data.frame(rownames(b),b)
colnames(b)[1] <- "Mtype"
Mtype <- factor(b[,1],levels = c("M1","M2","M3","M4","M5"))
library(reshape2)
d <- melt(b,"Mtype")
colnames(d)[2] <- "layer_type"
layer_type <- factor(b[,2],levels = c("deeper","upper"))
mytable <- xtabs(value~Mtype+layer_type,data=d)
#卡方独立性检验#
chisq.test(mytable)
#p-value = 0.008795<0.05,说明layer_type和M_type相互独立,因为有n<5，所以有warning#
#Fisher 精确检验，原假设是：边界固定的列联表中行和列是相互独立的#
fisher.test(mytable)
#fisher.test()函数可以在任意行列数大于等于2的二维列联表上使用，但不能用于2×2的列联表#
#Cochran-Mantel-Haenszel检验，mantelhaen.test()函数可用来进行Cochran-Mantel-Haenszel卡方检验，其原假设是，两个
# 名义变量在第三个变量的每一层中都是条件独立的,x必须为3维阵列#
mantelhaen.test(mytable)

#相关性度量#
#衡量相关性强弱的相关性度量。vcd包中的assocstats()函数可以用来计算二维列联表的phi系数、列联系数和Cramer’s V系数#
library(vcd)
assocstats(mytable)
#总体来说，较大的值意味着较强的相关性#

#相关#
# 1. Pearson、Spearman和Kendall相关
# Pearson积差相关系数衡量了两个定量变量之间的线性相关程度。Spearman等级相关系数则衡量分级定序变量之间的相关程度。Kendall’s Tau相关系数也是一种非参数的等级相关度量。
cor(mytable,use="everything",method = "pearson")

layer_type <- factor(d[,2],levels=c(1,2),labels = c("deeper","upper"))
Mtype <- as.factor(Mtype)
Mtype <- factor(Mtype,levels=c(1,2,3,4,5),labels = c("M1","M2","M3","M4","M5"))

#协方差和相关系数#
states<- state.x77[,1:6]
cov(states)
cor(states)
cor(states, method="spearman")
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

#相关性的显著性检验#
# 在计算好相关系数以后，如何对它们进行统计显著性检验呢？常用的原假设为变量间不相关
# （即总体的相关系数为0）。你可以使用cor.test()函数对单个的Pearson、Spearman和Kendall相关系数进行检验。简化后的使用格式为：
cor.test(x, y, alternative = , method = )
# 当研究的假设为总体的相关系数小于0 时， 请使用
# alternative="less" 。在研究的假设为总体的相关系数大于0 时， 应使用
# alternative="greater"。在默认情况下，假设为alternative="two.side"（总体相关系
# 数不等于0）。
cor.test(states[,3], states[,5])
# 这段代码检验了预期寿命和谋杀率的Pearson相关系数为0的原假设。假设总体的相关度为0，
# 则预计在一千万次中只会有少于一次的机会见到0.703这样大的样本相关度（即p=1.258e–08）。由
# 于这种情况几乎不可能发生，所以你可以拒绝原假设，从而支持了要研究的猜想，即预期寿命和
# 谋杀率之间的总体相关度不为0

# 遗憾的是，cor.test()每次只能检验一种相关关系。但幸运的是，psych包中提供的
# corr.test()函数可以一次做更多事情。corr.test()函数可以为Pearson、Spearman或Kendall
# 相关计算相关矩阵和显著性水平。

#独立样本的t检验#
t.test(y ~ x, data)
t.test(y1, y2, paired=TRUE) #配对t检验#

#非参数检验#
#若两组数据独立，可以使用Wilcoxon秩和检验（更广为人知的名字是Mann-Whitney U检验）
# 来评估观测是否是从相同的概率分布中抽得的（即，在一个总体中获得更高得分的概率是否比另
# 一个总体要大）。调用格式为：
wilcox.test(y ~ x, data) #其中的y是数值型变量，而x是一个二分变量。调用格式或为：wilcox.test(y1, y2)
# Wilcoxon符号秩检验是非独立样本t检验的一种非参数替代方法。它适用于两组成对数据和
# 无法保证正态性假设的情境。调用格式与Mann-Whitney U检验完全相同，不过还可以添加参数
# paired=TRUE。

#Kruskal-Wallis检验#
kruskal.test(y ~ A, data)
friedman.test(y ~ A | B, data)
#多组多重比较#
source("http://www.statmethods.net/RiA/wmc.txt")
states <- data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data=states, method="holm")


