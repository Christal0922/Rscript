#创建leadership数据框#
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE)
#创建新变量的3种方式
mydata<-data.frame(x1 = c(2, 2, 6, 4),
                   x2 = c(3, 4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2

attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)

mydata <- transform(mydata,
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2)
#变量的重编码#
leadership$age[leadership$age == 99] <- NA #将99岁的年龄值重编码成缺失值#
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                    leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young" 
#variable[condition] <- expression 将仅在condition的值为TRUE时执行赋值
leadership <- within(leadership,{
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young" })
#若要冲编码成有序型因子，则可以使用doBy包的recodevar()#
#重命名#
fix(leadership) #交互式编辑#
names(leadership)[2] <- "testDate"
names(leadership)
names(leadership)[2] <- "testDate"
names(leadership)[6:10] <- c("item1", "item2", "item3", "item4", "item5")
library(plyr)
leadership <- rename(leadership,
                     c(manager="managerID", date="testDate"))
#缺失值#
y <- c(1,2,3,NA)
is.na(y)#返回一个相同大小的对象，如果某个元素是缺失值，相应的位置将被改写成TRUE,不是缺失值的位置则为FALSE#
is.na(leadership[,6:10])
leadership$age[leadership$age == 99] <- NA
#在分析中排除缺失值#
na.rm=TRUE #移除缺失值#
na.omit()#移除所有含有缺失值的观测#
newdata <- na.omit(leadership)
#日期值，as.Date(x,"input_format"),将字符串的形式改成数值形式存储#
myformat <- "%m/%d/%y"
leadership$date <- as.Date(leadership$date, myformat)
Sys.Date()#返回当天的日期#
date()#返回当前的日期和时间#
today <- Sys.Date()
format(today,format="%B %d %Y")
startdate <- as.Date("2004-02-13")
enddate <- as.Date("2011-01-22")
days <- enddate - startdate
days #计算日期#
#将日期转换成字符型变量#
strDates <- as.character(dates)
#数据排序#
newdata <- leadership[order(leadership$age),] #按age升序排列#

attach(leadership)
newdata <- leadershp[order(gender,age),]
detach(leadership) #将各行依女性到男性、同样性别中按年龄升序排序#
attach(leadership)
newdata <- leadershp[order(gender,-age),]
detach(leadership) #将各行依经理人的性别和年龄降序排序,变量名前加-表示降序排列#

#数据集的合并#
total <- merge(dataframeA,dataframeB,by="ID") #将dataframeA和dataframeB按照ID进行了合并#
total <- merge(dataframeA, dataframeB, by=c("ID","Country")) #将两个数据框按照ID和Country进行了合并#
total <- cbind(A,B)#如果要直接横向合并两个矩阵或数据框，并且不需要指定一个公共索引，那么可以直接使用cbind()函数#
total <- rbind(dataframeA, dataframeB) #向数据框添加行#

#选入变量#
newdata <- leadership[, c(6:10)]

myvars <- c("q1", "q2", "q3", "q4", "q5")
newdata <-leadership[myvars]

myvars <- paste("q", 1:5, sep="")
newdata <- leadership[myvars] #用paste()函数创建了与上例中相同的字符型向量#

#剔除（丢弃）变量#
myvars <- names(leadership) %in% c("q3", "q4")
newdata <- leadership[!myvars]

newdata <- leadership[c(-8,-9)] #在知道q3和q4是第8个和第9个变量的情况下,在某一列的下标之前加一个减号（–）就会剔除那一列#

leadership$q3 <- leadership$q4 <- NULL #将q3和q4两列设为了未定义（NULL)#

newdata <- leadership[1:3,]
newdata <- leadership[leadership$gender=="M" &
                        leadership$age > 30,]

attach(leadership)
newdata <- leadership[gender=='M' & age > 30,]
detach(leadership)

leadership$date <- as.Date(leadership$date, "%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$date >= startdate &
                              leadership$date <= enddate),]

#subset()函数#
newdata <- subset(leadership,age >= 35|age < 24, 
                  select=c(q1,q2,q3,q4))#选择所有age值大于等于35或age值小于24的行，保留了变量q1到q4#
newdata <- subset(leadership,gender=="M" & age >25,
                  select=gender:q4) #选择25岁以上所有男性，并保留了变量gender到q4和其间的所有列#
#随机抽样#
mysample <- leadership[sample(1:nrow(leadership),3,replace=FALSE)] #随机取出3个元素，无放回取样，可参考sampling包，survey包#



