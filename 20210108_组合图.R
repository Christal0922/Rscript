#par() mfrow=c(nrows,ncols)按行填充，mfcol=c(nrows,ncols)按列填充#
attach(mtcars)
dev.new()
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
plot(wt,mpg,main = "wt vs mpg")
hist(wt,main = "wt")
par(opar)
detach(mtcars)

#layout#
dev.new()
attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow = TRUE)) #c()位置和因素，2,2 2行2列#
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#layout(mat) mat是一个矩阵，指定所要组合的多个图形的所在位置#
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE),
       widths=c(3, 1), heights=c(1, 2))
#widths=各列宽度值组成的一个向量#
#heights= 各列高度值组成的一个向量#

#gridExtra包#
library(gridExtra)
library(ggplot2)
gs <- list(NULL)
gs[[1]] <- ggplot(E1,aes(x=rhe,y=RP))+geom_point()
gs[[2]] <- ggplot(E1,aes(x=RP,y=Rin))+geom_point()
grid.arrange(grobs=gs,ncol = 2)

p1 <- ggplot(E1,aes(x=rhe,y=RP))+geom_point()
p2 <- ggplot(E1,aes(x=RP,y=Rin))+geom_point()
gs <- list(NULL)
gs[[1]] <- p1
gs[[2]] <- p2
grid.arrange(grobs=gs,nrow = 2)

#grid 包#
library(grid)
library(ggplot2)
# prepare ggplot charts
p.hist.len <- ggplot(iris) + geom_histogram(aes(x=Sepal.Length))
p.hist.wid <- ggplot(iris) + geom_histogram(aes(x=Sepal.Width)) + coord_flip()
p.scatter <- ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width))

# create viewports
grid.newpage()
vp.len <- viewport(x=0, y=0.66, width=0.66, height=0.34, just=c("left", "bottom"))
vp.wid <- viewport(x=0.66, y=0, width=0.34, height=0.66, just=c("left", "bottom"))
vp.scatter <- viewport(x=0, y=0, width=0.66, height=0.66, just=c("left", "bottom"))

# direct the charts into the specified viewport
print(p.hist.len, vp=vp.len)
print(p.hist.wid, vp=vp.wid)
print(p.scatter, vp=vp.scatter)
