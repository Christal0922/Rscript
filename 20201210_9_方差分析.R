#9 方差分析#
#单因素方差分析#
Total <- read.table("E:/R/patch/20201218_235cells.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
attach(Total)
table(group)
aggregate(rhe,by=list(group),FUN=mean,na.rm=TRUE) #na.rm，忽略缺失值#
aggregate(rhe,by=list(group),FUN=sd,na.rm=TRUE)
fit <- aov(rhe ~ group)
summary(fit)
library(gplots)
plotmeans(rhe ~ group, xlab="group", ylab="Rhe",
          main="Mean Plot\nwith 95% CI")
detach(Total)

#多重比较#
TukeyHSD(fit)
opar <- par(no.readonly=TRUE)#no.readonly=TRUE可以生成一个可以修改的当前图形参数列表#
par(las=2) #旋转轴标签#
par(mar=c(5,8,4,2)) #增大左边界的面积#
plot(TukeyHSD(fit))
par(opar)

library(multcomp)
#参考Bretz、Hothorn和Westfall的Multiple Comparisons Using R（2010
fit <- aov(rhe~group,data = Total)
summary(fit)
TukeyHSD(fit,which = "group")
library(multcomp)
tuk <- glht(fit,linfct = mcp(group="Tukey")) #multcomp包中的glht()函数提供了多重均值比较更为全面的方法，既适用于线性模型（如本章各例），也适用于广义线性模型#
plot(cld(tuk,level = 0.05),col="lightgrey")

#评估检验的假设条件#
attach(Total)
library(car)
qqPlot(lm(rhe ~ group,data = Total),simulate=TRUE,main="Q-Q Plot",labels=FALSE) #Q-Q图检验正态性假设#
#Bartlett检验方差齐性#
bartlett.test(rhe~group,data = Total)
#类似的有Fligner-Killeen检验fligner.test()函数，Brown-Forsythe检验（HH包里面的hov()函数）#
library(car)
outlierTest(fit) #检测离群点#
#当p>1时产生NA表示含有离群点#

#单因素协方差分析#
data(Total,package = "multcomp")
attach(Total)
table(group)
aggregate(rhe,by=list(group),FUN=mean,na.rm=TRUE)
fit <- aov(rhe ~ Cm + group) #Cm为协方差#
summary(fit)
#ANCOVA条件，正态性，同方差性，回归斜率相同#
library(multcomp)
fit2 <- aov(rhe ~ group*Brain_area, data=Total)
summary(fit2)
#可以看到交互效应不显著，支持了斜率相等的假设#

#结果可视化#
library(HH)
ancova(rhe ~ group+Cm,data=Total)
ancova(rhe ~ group*Cm,data=Total)#考虑交互作用#

#双因素方差分析#
attach(Total)
table(group,pathology2)
aggregate(rhe,by=list(group,pathology2),FUN=mean,na.rm=TRUE)
aggregate(rhe,by=list(group,pathology2),FUN=sd,na.rm=TRUE)
fit <- aov(rhe ~ group*pathology2)
summary(fit)
threshold <- factor(threshold) #变量被转换为因子变量，这样aov()函数就会将它当做一个分组变量，而不是一个数值型协变量
#交互作用的可视化#
#方法1#
interaction.plot(Cm,group,rhe,type = "b",
                 col = c("red","blue","yellow"),
                 main="Interaction between group and pathology Type")
#方法2#
plotmeans(rhe ~ interaction(Cm,group,sep = " "),
          col = c("red","blue","yellow"),
          main="Interaction between group and pathology Type")
#方法3#
library(HH)
interaction2wt(rhe~group*Brain_area) #推荐#


