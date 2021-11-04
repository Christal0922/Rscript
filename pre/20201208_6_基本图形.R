library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
#堆叠图，besides=FALSE(默认）#
barplot(counts,
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow","green"),
        legend=rownames(counts))
#条形图，besides=TRUE#
barplot(counts,
        main="Grouped Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)
#均值条形图#
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means <- means[order(means$x),]#将均值从小到大排序#
barplot(means$x, names.arg=means$Group.1)
title("Mean Illiteracy Rate")
#棘状图，百分比堆叠图#
library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main="Spinogram Example")
detach(Arthritis)
#饼图#
pie(x,labels)#x是一个非负数值向量，表示每个扇形的面积，而labels则是表示各扇形标签的字符型向量#
par(mfrow=c(2, 2))
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls,
    main="Simple Pie Chart")
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", sep="")
pie(slices, labels=lbls2, col=rainbow(length(lbls2)),
    main="Pie Chart with Percentages")
library(plotrix)
pie3D(slices, labels=lbls,explode=0.1,
      main="3D Pie Chart ")
mytable <- table(state.region)
lbls3 <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls3,
    main="Pie Chart from a Table\n (with sample sizes)")
par(no.readonly=TRUE)
#扇形图，Fan Plot#
library(plotrix)
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
fan.plot(slices, labels = lbls, main="Fan Plot")
#par()其调用格式为par(optionname=value,
# optionname=name,...)。不加参数地执行par()将生成一个含有当前图形参数设置的列表。
# 添加参数no.readonly=TRUE可以生成一个可以修改的当前图形参数列表
opar <- par(no.readonly=TRUE)
par(lty=2, pch=17)
plot(dose, drugA, type="b")
par(opar)
# library(Hmisc)
# minor.tick(nx=n, ny=n, tick.ratio=n)
# 来添加次要刻度线。其中nx和ny分别指定了X轴和Y轴每两条主刻度线之间通过次要刻度线划
# 分得到的区间个数。tick.ratio表示次要刻度线相对于主刻度线的大小比例。当前的主刻度
# 线长度可以使用par("tck")获取。
par(mfrow=c(1, 1))
#直方图#
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=12,col="red",xlab="Miles Per Gallon",main = "Colored histogram with 12 bins")
#添加轴须图（rug plot), 核密度曲线，若数据有相同的值时，可以用rug(jitter(mtcars$mpag,amount=0.01)),这样将向每个数据点添加一个小的随机值（一个±amount之间的均匀分布随机数），以避免重叠
的点产生影响#
hist(mtcars$mpg,freq=FALSE,breaks=12,col="red",xlab="Miles Per Gallon",main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg,col="blue",lwd=2))

#添加正态分布曲线和外框#
x <- mtcars$mpg
h<-hist(x,
        breaks=12,
        col="red",
        xlab="Miles Per Gallon",
        main="Histogram with normal curve and box")
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()

#箱线图#
boxplot(mtcars$mpg,main="Box plot",ylab="Miles per Gallon")
boxplot.stats(mtcars$mpg)#输出构建图形的统计量#
#并列箱线图#
boxplot(formula, data=dataframe)
boxplot(mpg ~ cyl, data=mtcars,
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")
#varwidth=TRUE, 使箱线图的宽度与其样本大小的平方根成正比#
#horizontal=TRUE可以反转坐标轴的方向#
#notch=TRUE，可以得到含凹槽的箱线图#
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels=c(4,6,8),
                       labels=c("4","6","8"))
#创建气缸数量的因子#
mtcars$am.f <- factor(mtcars$am,
                      levels=c(0,1),
                      labels=c("auto", "standard"))
#创建变速箱的因子#
boxplot(mpg ~ am.f *cyl.f,
        data=mtcars,
        varwidth=TRUE,
        col=c("gold","darkgreen"),
        main="MPG Distribution by Auto Type",
        xlab="Auto Type", ylab="Miles Per Gallon")
