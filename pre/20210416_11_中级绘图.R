#散点图#
attach(mtcars)
plot(wt,mpg,
     main="Basic Scatter plot of MPG vs. Weight",
     xlab = "Car Weight (lbs/1000)",
     ylab = "Miles per Gallon",pch=19)
abline(lm(mpg~wt),col="red",lwd=2,lty=1) #添加最佳拟合的线性直线
lines(lowess(wt,mpg),col="blue",lwd=2,lty=2) #lowess()函数添加一条平滑曲线

#散点图矩阵#
pairs(~mpg+disp+drat+wt,data = mtcars,
      main="Basic Scatter Plot Matrix")
pairs(~mpg+disp+drat+wt,data = mtcars,
      main="Basic Scatter Plot Matrix",
      upper.panel=NULL) #只显示下三角#
library(car)
scatterplotMatrix(~mpg+disp+drat+wt,data = mtcars,
      spread=FALSE,smoother.args=list(lty=2),
      main="Scatter Plot Matrix via car Package")
#线性和平滑（loess)拟合曲线默认添加，主对角线添加了核密度曲线和轴须图，
#spread=FALSE表示不添加展示分散度和对称信息的直线
#smoother.args=list(lty=2)设定平滑（loess)拟合曲线使用虚线而不是实线

#高密度散点图
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n,mean = 0,sd = 0.5),ncol = 2)
c2 <- matrix(rnorm(n,mean = 3,sd = 2),ncol = 2)
mydata <- rbind(c1,c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x","y")

with(mydata,
     plot(x,y,pch=19,main = "Scatter Plot with 10,000 observations"))
#使用封箱、颜色和透明度来指明图中任意点上重叠点的数目
#smoothScatter()函数可利用核密度估计生成用颜色密度来表示点分布的散点图
with(mydata,
     smoothScatter(x,y,main="Scatter Plot Colored by Smoothed Densities"))
#hexbin包中的hexbin()函数将二元变量的封箱放到六边形单元格中
library(hexbin)
with(mydata,{
  bin <- hexbin(x,y,xbins=50)
  plot(bin,main="Hexagonal Binning with 10,000 Observations")
})

#三维散点图
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg,
              main="Basic 3D Scatter plot")

#旋转三维散点图,rgl包
library(rgl)
attach(mtcars)
plot3d(wt,disp,mpg,col="red",size=5)

library(car)
with(mtcars,
     scatter3d(wt, disp, mpg))

#气泡图
#symbols(x,y,circle=radius)
#symbols(x,y,circle=sqrt(z/pi)),用面积而不是半径来表示第3个变量
attcah(mtcars)
r <- sqrt(disp/pi)
symbols(wt,mpg,circles = r,inches = 0.30,
        fg="white",bg="lightblue",
        main = "Bubble Plot with point size proportional to displacement",
        ylab = "Miles Per Gallon",
        xlab = "Weight of car (lbs/1000)")
text(wt,mpg,rownames(mtcars),cex = 0.6)
detach(mtcars)
#inches是比例因子，控制着圆圈大小（默认最大圆圈为1英寸）

#折线图
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
t1 <- subset(Orange,Tree==1)
plot(t1$age,t1$circumference,
     xlab="Age(days)",
     ylab="Circumference(mm)",
     main="Orange Tree 1 Growth")
plot(t1$age,t1$circumference,
     xlab="Age(days)",
     ylab="Circumference(mm)",
     main="Orange Tree 1 Growth",
     type="b")
par(opar)

#展示五种橘树随时间推移的生长状况的折线图
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange,yrange,type="n",
     xlab="Age (days)",
     ylab="Circumference(mm)")

colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18,18+ntrees,1) #产生18至18+ntrees的等差数列，差值为1

for (i in 1:ntrees){
  tree <- subset(Orange,Tree==i)
  lines(tree$age,tree$circumference,
        type="b",
        lwd=2,
        lty=linetype[i],
        col=colors[i],
        pch=plotchar[i])
}

title("Tree Growth","example of line plot")
legend(xrange[1],yrange[2],
       1:ntrees,
       cex=0.8,
       col = colors,
       pch = plotchar,
       lty=linetype,
       title = "Tree")

#相关图
options(digits = 2)
cor(mtcars)

library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of mtcars intercorrelations")
#阴影图，默认地，蓝色和从左下指向右上的斜杠表示单元格中的两个变量呈正相关。反过来，红色和从左上指向右下的斜杠表示变量呈负相关。色彩越深，饱和度越高，说明变量相关性越大。相关性接近于0的单元格基本无色。
#饼状图，颜色的功能同上，但相关性大小由被填充的饼图块的大小来展示。正相关性将从12点钟处开始顺时针填充饼图，而负相关性则逆时针方向填充饼图。

#马赛克图
ftable(Titanic)
library(vcd)
mosaic(Titanic,shade=TRUE,legend=TRUE)
mosaic(~Class+Sex+Age+Survived,data = Titanic,shade=TRUE,legend=TRUE)





