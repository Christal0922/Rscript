#键盘输入#
mydata <- data.frame(age=numeric(0),gender=character(0),weight=numeric(0))
mydata <- edit(mydata) #等价于fix(mydata)#

#用带分隔符的文本文件导入数据#
mydataframe <- read.table(file,options)

#导入Excel数据#
read.xlsx(file,n)
#xlsx包，XLConnect包，openxlsx包#

#导入XML数据#
#参考www.omegahat.org/RSXML#

#从网页抓取数据#
#readLines(),grep(),gsub(),RCurl包，XML包#

#导入SPSS数据# #或者foreign包的spss.get()#
install.packages("Hmisc")
library(Hmisc)
mydataframe <- spss.get("mydata.sav",use.value,labels=TRUE)

