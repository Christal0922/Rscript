#方差分析
rm(list = ls()) #清除所有变量
library(multcomp)
attach(cholesterol) #trt表示不同疗法，response表示不同疗法对应的降胆固醇的效应
table(trt)
aggregate(response,by=list(trt),FUN=mean)
aggregate(response,by=list(trt),FUN=sd)
fit <- aov(response~trt) #单因素方差分析
summary(fit) #显示分析结果，
library(gplots)
plotmeans(response~trt,xlab="Treatment",ylab="Response",
          main="Mean Plot\nwith 95%CI")
detach(cholesterol)

#多重比较# 推荐Bretz\Hothorn\Westfall的multiple Comparisons Using R 2010
TukeyHSD(fit)

par(las=2) #旋转轴标签#
par(mar=c(5,8,4,2)) #增大左边界的面积#
plot(TukeyHSD(fit))
#图形中置信区间包含0的疗法说明差异不显著#

#multcomp包的glht()函数提供了多重比较更为全面的方法
library(multcomp)
par(mar=c(5,4,6,2))
tuck <- glht(fit,linfct=mcp(trt="Tukey"))
plot(cld(tuck,level=0.05),col="lightgrey")
#cld()函数的level设置显著水平
#有相同字母的组（用箱线图表示）说明均值差异不显著

#评估检验的假设条件
#正态性检验
#Shapiro-Vilk法检测每组的正态性#
group


#用Q-Q图来检验正态性假设
library(car)
qqPlot(lm(response ~ trt, data=cholesterol),
       simulate=TRUE, main="Q-Q Plot", labels=FALSE)
#数据落在95%置信缺件范围内，说明满足正态性假设#

#Bartlett检验检测方差齐性
bartlett.test(response~trt,data = cholesterol)
#p-value = 1表示五组的方差没有显著不同
#其他检验包括，Fligner-Killeen检验fligner.test()函数，Brown-Forsythe检验（HH包中的hov()函数

#car包的outlierTest()函数检测离群点
library(car)
outlierTest(fit)

#方差不齐，用welch's anova, welch's anova相当于根据每组方差加了个不同的权重调整了F统计量
oneway.test(rheo~pathology.type,data = Ephys,var.equal = F)
oneway.test(rheo~pathology.type,data = Ephys,var.equal = F)$p.value
#p-value=9.667e-06,

#可忽略，设置因子
data$dose <- factor(data$dose, levels = c(0.5,1,2), labels = c("D0.5", "D1", "D2"))




