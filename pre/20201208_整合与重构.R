aggregate(x, by, FUN)#x,待折叠的数据对象，by,一个变量名组成的列表，这些变量将被去掉以形成新的观测，FUN,用来描述统计量的标量函数#
options(digits = 3)
attach(mtcars)
aggdata <- aggregate(mtcars, by=list(cyl,gear),FUN=mean, na.rm=TRUE)
# by=list(Group.cyl=cyl, Group.gears=gear)
#reshape2包#
library(reshape2)
md <- melt(mydata,id=c("ID","Time"))
newdata <- dcast(md,formula,fun.aggregate)
# md为已融合的数据，formula表示最后想要的结果，fun.aggregate是数据整合函数



