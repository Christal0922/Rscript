#简单线性回归#
fit <- lm(weight~height, data=women)
summary(fit)
women$weight #真实值#
fitted(fit) #预测值#
residuals(fit) #残差值#
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)")
abline(fit)
#Weight=-87.52+3.45 x Height#

#多项式回归#
fit2 <- lm(weight~height+I(height^2),data=women)
summary(fit2)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))
#Weight=261.88-7.35 x Height + 0.083 x Height #

library(car)
scatterplot(weight ~ height, data=women,
            lty=2, pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")
#这个功能加强的图形，既提供了身高与体重的散点图、线性拟合曲线和平滑拟合（loess）曲
# 线，还在相应边界展示了每个变量的箱线图。spread=FALSE选项删除了残差正负均方根在平滑
# 曲线上的展开和非对称信息。smoother.args=list(lty=2)选项设置loess拟合曲线为虚线。
# pch=19选项设置点为实心圆（默认为空心圆）

#多元线性回归#
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                    main="Scatter Plot Matrix")
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
summary(fit)

fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)

#回归诊断#
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
confint(fit)

fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)

fit2 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)
#观测点13不满足残差正态性，观测点15位强影响点#
newfit <- lm(weight~ height + I(height^2), data=women[-c(13,15),])#删去观测点13和15#

library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
#绘制学生化残差图的函数#
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)

#多重共线性#
# 多重共线性可用统计量VIF（Variance Inflation Factor，方差膨胀因子）进行检测。VIF的平
# 方根表示变量回归参数的置信区间能膨胀为与模型无关的预测变量的程度（因此而得名）。car
# 包中的vif()函数提供VIF值。一般原则下， vif >2就表明存在多重共线性问题。
library(car)
vif(fit)

#异常观测值#
#离群点#
# 离群点是指那些模型预测效果不佳的观测点。它们通常有很大的、或正或负的残差（Yi–??i）。
# 正的残差说明模型低估了响应值，负的残差则说明高估了响应值。
# 你已经学习过一种鉴别离群点的方法：图8-9的Q-Q图，落在置信区间带外的点即可被认为是
# 离群点。另外一个粗糙的判断准则：标准化残差值大于2或者小于–2的点可能是离群点，需要特
# 别关注。
library(car)
outlierTest(fit)

#高杠杆值点#
# 高杠杆值观测点，即与其他预测变量有关的离群点。换句话说，它们是由许多异常的预测变
# 量值组合起来的，与响应变量值没有关系。
# 高杠杆值的观测点可通过帽子统计量（hat statistic）判断。对于一个给定的数据集，帽子均
# 值为p/n，其中p是模型估计的参数数目（包含截距项），n是样本量。一般来说，若观测点的帽子
# 值大于帽子均值的2或3倍，就可以认定为高杠杆值点。

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

locator(图形交互)

#强影响点#
# 强影响点，即对模型参数估计值影响有些比例失衡的点。例如，若移除模型的一个观测点时
# 模型会发生巨大的改变，那么你就需要检测一下数据中是否存在强影响点了。
# 有两种方法可以检测强影响点：Cook距离，或称D统计量，以及变量添加图（added variable
# plot）。一般来说，Cook’s D值大于4/(n–k–1)，则表明它是强影响点，其中n为样本量大小，k是预
# 测变量数目。可通过如下代码绘制Cook’s D图形
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
# 但逐渐发现以1为分割点比4/(n–k–1)更具一般性。若设定D=1为判别标准，则数据集中没有点看起来像是强影响点。
library(car)
avPlots(fit, ask=FALSE, id.method="identify")

library(car)
influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
#将离群点、杠杆值和强影响点的信息整合到一幅图形中#
# 纵坐标超过+2或小于–2的州可被认为是离群点，水平轴超过0.2或0.3
# 的州有高杠杆值（通常为预测值的组合）。圆圈大小与影响成比例，圆圈很大
# 的点可能是对模型参数的估计造成的不成比例影响的强影响点

#删除观测点#
# 我对删除观测点持谨慎态度。若是因为数据记录错误，或是没有遵守规程，或是受试对象误
# 解了指导说明，这种情况下的点可以判断为离群点，删除它们是十分合理的。
# 不过在其他情况下，所收集数据中的异常点可能是最有趣的东西。发掘为何该观测点不同于
# 其他点，有助于你更深刻地理解研究的主题，或者发现其他你可能没有想过的问题。

#变量变换#
library(car)
summary(powerTransform(states$Murder))
# 结果表明，你可以用Murder0.6来正态化变量Murder。由于0.6很接近0.5，你可以尝试用平方
# 根变换来提高模型正态性的符合程度。但在本例中，λ=1的假设也无法拒绝（p=0.145），因此没
# 有强有力的证据表明本例需要变量变换

library(car)
boxTidwell(Murder~Population+Illiteracy,data=states)
# 结果显示，使用变换Population0.87和Illiteracy1.36能够大大改善线性关系。但是对
# Population（p=0.75）和Illiteracy（p=0.54）的计分检验又表明变量并不需要变换

#选择最佳的回归模型#
#模型比较#
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
             data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit2, fit1)
# 此处，模型1嵌套在模型2中。anova()函数同时还对是否应该添加Income和Frost到线性模型
# 中进行了检验。由于检验不显著（p=0.994），我们可以得出结论：不需要将这两个变量添加到线
# 性模型中，可以将它们从模型中删除

fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
AIC(fit1,fit2)
# 此处AIC值表明没有Income和Frost的模型更佳。注意，ANOVA需要嵌套模型，而AIC方法不需要

#变量选择#
#逐步回归 stepwise method#
# 逐步回归中，模型会一次添加或者删除一个变量，直到达到某个判停准则为止。例如，向前
# 逐步回归（forward stepwise regression）每次添加一个预测变量到模型中，直到添加变量不会使
# 模型有所改进为止。向后逐步回归（backward stepwise regression）从模型包含所有预测变量开始，
# 一次删除一个变量直到会降低模型质量为止。而向前向后逐步回归（stepwise stepwise regression，
# 通常称作逐步回归，以避免听起来太冗长），结合了向前逐步回归和向后逐步回归的方法，变量
# 每次进入一个，但是每一步中，变量都会被重新评价，对模型没有贡献的变量将会被删除，预测
# 变量可能会被添加、删除好几次，直到获得最优模型为止。

library(MASS)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                       "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
            data=states)
stepAIC(fit, direction="backward")

#相对重要性#
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)
# 此处可以看到，当其他因素不变时，文盲率一个标准差的变化将增加0.68个标准差的谋杀率。
# 根据标准化的回归系数，我们可认为Illiteracy是最重要的预测变量，而Frost是最不重要的。

# 相对权重（relative weight）是一种比较有前景的新方法，它是对所有可能子模型添加一个预
# 测变量引起的R平方平均增加量的一个近似值
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}
#相对权重的函数#
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="blue")
