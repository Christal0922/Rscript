Morpho <- read.csv("E:/R/PCA/20210608_cell89M2.csv",header = TRUE,row.names = 1)
Morpho2 <- Morpho[which(Morpho$epileptic.frequency.before.treatment.in.3.months.!="unknown"),]

#泊松回归研究癫痫过程
install.packages("robust")
data(breslow.dat,package = "robust")
summary(breslow.dat[c(6,7,8,10),])

#观察数据分布
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY,breaks = 20,xlab = "Seizure Count",main = "Distrubtion of Seizures")
boxplot(sumY~Trt,xlab="Treatment",main="Group comparison")
par(opar)

#拟合泊松回归
fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
summary(fit)
coef(fit) #获取模型系数 癫痫发病数的对数均值
exp(coef(fit))  #癫痫发病数，而非发病数的对数

#评价过度离势，残差偏差与残差自由度的比例远远大于1
deviance(fit)/df.residual(fit)









