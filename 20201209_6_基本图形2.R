#小提琴图，vioplot包的vioplot()#
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3,
        names=c("4 cyl", "6 cyl", "8 cyl"),
        col="gold")
title("Violin Plots of Miles Per Gallon", ylab="Miles Per Gallon",
      xlab="Number of Cylinders")
#白点是中位数，黑色盒型的范围是下四分位点到上四分位点，细黑线表示须。外部形状即为核密度估计#

#点图#
dotchart(x,labels=)
dotchart(mtcars$mpg, labels=row.names(mtcars), cex=.7,
         main="Gas Mileage for Car Models",
         xlab="Miles Per Gallon")

x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl) #将数值向量cyl转换成一个因子#
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
#添加一个字符型向量（color)到数据库x中，根据cyl的值，它所含的值为#
dotchart(x$mpg,
         labels = row.names(x),#各数据点的标签取自数据框的行名（车辆型号）#
         cex=.7,
         groups = x$cyl,
         gcolor = "black", #数字4、6和8以黑色显示#
         color = x$color, #点和标签的颜色#
         pch=19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")


