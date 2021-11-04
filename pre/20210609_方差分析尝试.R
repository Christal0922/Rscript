Ephys <- read.table("E:/R/PCA/20210608_cell340E2.txt",row.names=1,head=TRUE,sep="\t")
#基本计算
Val <- as.matrix(names(Ephys))
attach(Ephys) #为方便描述变量
#将pathology.type转换成因子
pathology.type <- as.factor(pathology.type)

aggregate(rheo,by=list(pathology.type),FUN=mean) #rheo参数按照pathology.type取均值
aggregate(Ephys[,10:22],by=list(pathology.type),FUN=mean) #多个电生理参数取均值
aggregate(Ephys[,10:22],by=list(pathology.type),FUN=sd) #多个电生理参数取sd


#方差分析
fit <- aov(rheo~pathology.type) #对rheo按照pathology.type进行方差分析
summary(fit) #显示方差分析结果
#可忽略#
library(gplots)
plotmeans(rheo~pathology.type,xlab="pathology.type",ylab="Rheobase",
          main="Rheobase")

#多重比较
#Turkey法
TukeyHSD(aov(rheo~pathology.type))
par(las=2) #旋转轴标签#
par(mar=c(5,8,4,2)) #增大左边界的面积#
plot(TukeyHSD(aov(rheo~pathology.type)))
#图形中置信区间包含0的疗法说明差异不显著#

#multcomp glht()函数进行多重比较并作图
library(multcomp)
tuck <- glht(fit,linfct=mcp(pathology.type="Tukey"))
plot(cld(tuck,level=0.05),col="lightgrey")

#评估检验的假设条件
#正态性检验
#Shapiro-Wilk正态性检验，每组检验一遍#
group_data <- Ephys[,c(6,10)]
group_data$seqid <- rownames(group_data)
library(reshape2)
md <- melt(group_data,id=c("pathology.type","seqid"))
group_new <- dcast(md,seqid~pathology.type)
rownames(group_new) <- group_new$seqid
group_new <- group_new[,-1]
lapply(group_new, function(x){
  shapiro.test(x)$p.value
})

#有几组P值小于0.05，不满足正态分布#


#用Q-Q图来检验正态性假设
library(car)
qqPlot(lm(rheo ~ pathology.type, data=Ephys),
       simulate=TRUE, main="Q-Q Plot", labels=FALSE)

#数据点不分布在置信区间内，不满足正态性假设#

#方差齐性检验
#car包的leveneTest()函数
library(car)
leveneTest(rheo~pathology.type,data=Ephys)
#p=0.003726说明方差不齐

#Bartlett检验检测方差齐性
bartlett.test(rheo~pathology.type,data = Ephys)
#p-value = 0.0002281表示数据不满足方差齐性

#单因素协方差分析
fit2 <- aov(rheo~pathology.type+Cm)
summary(fit2)
#Cm Pr<0.05，控制pathology.time，Cm与rheo相关
#由于使用了协变量，可能需要获取调整的组均值，即去除协变量效应后的组均值，可使用effects包的effects(函数
library(effects)
effect("Cm",fit2)

#离群点检测#
library(car)
outlierTest(fit)
outlierTest(lm(rheo~pathology.type,data = Ephys))
#38,178
Ephys[c(38,178),]

#单因素方差分析，结果显示P值小于0.05
summary(aov(rheo~pathology.type,data = Ephys))
#p<0.05说明不同类型的pathology type对测量结果有影响

#Welch's anova，如果方差不齐性（并且每组样本数也不平衡），那么可以使用oneway.test(),Welch's anova相当于根据每组方差加了个不同的权重调整了F统计量
oneway.test(rheo~pathology.type,data = Ephys,var.equal = F)

#多重比较其他方法
#LSD法，最小显著差法，该法一般用于计划好的多重比较。它其实只是t检验的一个简单变形
library(agricolae)
#LSD.test(y, trt, DFerror, MSerror, alpha = 0.05, p.adj=c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr"), …)
model <- aov(rheo~pathology.type,data = Ephys)
out <- LSD.test(model,"pathology.type",alpha = 0.05,p.adj="none")
out$group
plot(out)

#Bonferroni法
library(agricolae)
model <- aov(rheo~pathology.type)
out <- LSD.test(model,"pathology.type",p.adj = "bonferroni")
plot(out)

#Dunnett检验，用于多个实验组与一个对照组间的比较
#glht(model, linfct, alternative = c("two.sided", "less", "greater"), ...)
library(multcomp)
fit <- aov(rheo~pathology.type)
rht <- glht(fit,linfct=mcp(pathology.type="Dunnett"),alternative="two.side")
summary(rht)
plot(rht)

#Turkey检验，使用学生会的范围统计量进行组间所有成对比较，特点是所有各组的样本数相等，各组样本均数之间的全面比较，可能产生角度的假阴性结论
fit <- aov(rheo~pathology.type)
tuk <- TukeyHSD(fit)
plot(tuk)

#Duncan法（新复极差法
library(agricolae)
fit <- aov(rheo~pathology.type)
out <- duncan.test(fit,"pathology.type")
plot(out)

#Scheffe检验
# 为均值的所有可能的成对组合执行并发的联合成对比较。使用F取样分布。可用来检查组均值的所有可能的线性组合，而非仅限于成对组合。Scheffe检验特点：
# 各组样本数相等或不等均可以，但是以各组样本数不相等使用较多；
# 如果比较的次数明显地大于均数的个数时，Scheffe法的检验功效可能优于Bonferroni法
library(agricolae)
fit <- aov(rheo~pathology.type)
out <- scheffe.test(fit,"pathology.type")
plot(out)

