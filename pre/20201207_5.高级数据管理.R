#数据的标准化#
newdata <- scale(mydata)#对矩阵或数据框的指定列进行均值为0，标准差为1的标准化；
newdata <- scale(mydata)*SD+M #对每一列进行任意均值和标准差的标准化，M为想要的均值，SD为想要的标准差#
newdata <- transform(mydata,myvar = scale(myvar)*10+50)#将变量var标准化为均值50，标准差为10的变量#

#标准正态分布（均值为0，标准差为1）密度函数 dnorm; 分布函数 pnorm; 分位数函数 qnorm;随机生成函数 rnorm#
#设置随机数种子，让结果重现#
set.sead(1234)
runif(5)#生成5个0到1区间上服从均匀分布的伪随机数#
library(MASS)
myrnorm(n,mean,sigma) #获取给定均值向量和协方差阵的多元正态分布的数据#

library(MASS)
options(digits=3)
set.seed(1234) #设置随机数种子#
mean <- c(230.7, 146.7, 3.6)
sigma <- matrix(c(15360.8, 6721.2, -47.1,
                    6721.2, 4700.9, -16.5,
                    -47.1, -16.5, 0.3), nrow=3, ncol=3) #指定均值向量，协方差阵#
mydata <- mvrnorm(500, mean, sigma)
mydata <- as.data.frame(mydata)
names(mydata) <- c("y","x1","x2") #生成数据#
dim(mydata)
head(mydata,n=10)

#字符处理函数#
nchar(x)#计算x中的字符数量#
substr(x,start, stop)#提取或替换字符向量中的子串#
grep(pattern,x,ignore.case=FALSE,fixed=FALSE)#在x 中搜索某种模式。若fixed=FALSE，则pattern 为一个正则表达式（^[hc]?at，可参考维基百科regular expression）。若fixed=TRUE，则pattern 为一个文本字符串。返回值为匹配的下标#
grep("A",c("b","A","c"),fixed=TRUE)
# paste(...,sep="") 连接字符串，分隔符为sep,
# paste("x", 1:3,sep="")返回值为c("x1", "x2", "x3")
# paste("x",1:3,sep="M")返回值为c("xM1","xM2" "xM3")
# paste("Today is", date())返回值为Today is Thu Jun 25 14:17:32 2011
toupper(x)#大写转换#
tolower(x)#小写转换#
round(x,digits = n) #将x 舍如为指定位的小数#
seq(from, to, by)#生成一个序列#
rep(x,n)#将x重复n次#
pretty(x,n)#创建美观的分割点，通过选取n+1 个等间距的取整值，将一个连续型变量x分割为n 个区间。绘图中常用#
cat(...,file="myfile",append = FALSE)#连接...中的对象，并将其输出到屏幕上或文件中（如果声明了一个的话）,\n表示新行，\t为制表符，\'
# 为单引号，\b为退格，等等。（键入?Quotes以了解更多。）

#将函数应用于矩阵和数据框#
c <- matrix(runif(12),nrow=3)
apply(x,margin,FUN,...)#apply()函数，可将一个任意函数“应用”到矩阵、数组、数据框的任何维度上，#
# x为数据对象，margin为维度的下标，FUN是指定的函数，而...则包括了任何想传递给FUN的参数

mydata <- matrix(rnorm(30),nrow=6)
apply(mydata,1,mean)#计算每行的均值#
apply(mydata,2,mean)#计算每列的均值#
#lapply(),sapply()则可将函数应用到列表list上#

#example#
options(digits =2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                       stringsAsFactors=FALSE)
z <- scale(roster[,2:4])
score <- apply(z,1,mean) #计算行平均#
roster <- cbind(roster,score)#计算综合得分，合并#
y <- quantile(score,c(.8,.6,.4,.2))#给出了学生综合得分的百分位数#
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F" #基于相对名次（前20%，下20%）给出从A到F的评分#
name <- strsplit((roster$Student)," ") #以空格为界把学生姓名拆分为姓氏和名字#
Lastname <- sapply(name,"[",2) #使用sapply()提取列表中每个成分的第一个元素，放入一个储存名字的向量Firstname,并提取每个成分的第二个元素，放入一个储存姓氏的向量Lastname。"["
# 是一个可以提取某个对象的一部分的函数——在这里它是用来提取列表name各成分中的第一
# 个或第二个元素的。
Firstname <- sapply(name,"[",1)
roster <- roster[order(Lastname,Firstname),]#根据姓氏和名字排序#

#控制流#
for (var in seq) statement
for (i in 1:10) print("Hello")

while(cond) statement
i <- 10
while(i>0){
  print("Hello");
  i <- i-1
}

if (cond) statement
if (cond) statement1 else statement2
if (is.character(grade)) grade <- as.factor(grade)
if (!is.factor(grade)) grade <- as.factor(grade) else print("Grade already
is a factor")

ifelse(cond,statement1,statement2)
ifelse(score > 0.5, print("Passed"), print("Failed"))
outcome <- ifelse (score > 0.5, "Passed", "Failed")

switch(expr,...)
feelings <- c("sad", "afraid")
for (i in feelings)
  print(
    switch(i,
           happy = "I am glad you are happy",
           afraid = "There is nothing to fear",
           sad = "Cheer up",
           angry = "Calm down now"
    )
  )

#用户自编函数#
myfunction <- function(arg1,arg2,..){
  statements
  return(object)
}

mydata <- function(type="long"){
  switch(type,
         long= format(Sys.time(),"%A %B %d %Y"),
         short = format(Sys.time(),"%m-%d-%y"),
         cat(type,"is not a recognized type\n")
         )
}

#整合与重构#
