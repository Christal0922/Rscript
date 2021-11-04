raw <- read.csv("E:/R/PCA/20210629_dv2.csv")
raw$id <- as.numeric(rownames(raw))

#寻找threshold
raw$row2 <- raw$d1>=10 #设置dv/dt的阈值，目前定为10
raw2 <- raw[raw$row2,]
library(data.table)
DT <- data.table(raw2)
DT[, yoy := c(NA, diff(id))] #计算两行之间的差值>1，用以判断动作电位间隔
DT$bj <- DT$yoy>1
DT2 <- DT[DT$bj,] 
id2 <- DT2$id #得到动作电位阈值左右的间隔
DT2[,yoy2 :=c(NA,diff(id))]
DT2$bj2 <- DT2$yoy2>100
DT3 <- DT2[DT2$bj2,]
id3 <- DT3$id #初步得到动作电位阈值
ifelse(sum(raw[id2[1]:(id2[1]+100),2]>0),TRUE,FALSE) #判断第一个动作电位的阈值检测是否准确
for (i in 1:length(id3)){
  ifelse(sum(raw[id3[i]:(id3[i]+100),2]>0),DT3$T[i] <- "TRUE",DT3$T[i] <- "FALSE")
  i=i+1
} #检测1ms内是否v>0，若v>0，则存在动作电位，若没有，则threshold检测失误，剔除检测失误值
DT4 <- DT3[DT3$T=="TRUE",]
thresholdid <- c(id2[1],DT4$id)

#找到动作电位，按照threshold+10ms的间隔
e <- paste0(rep("E"),1:length(thresholdid)) 
APnum <- length(thresholdid) #动作电位个数
raw$type <- NA
for (i in 1:length(thresholdid)){
  raw$type[(thresholdid[i]-10):(thresholdid[i]+1000)] <- e[i]
  i=i+1
} #区分动作电位，用E1，E2等标记动作电位
try <- raw[complete.cases(raw),] #筛选出标记动作电位的有效数据


#筛选某个动作电位作图分析
library(ggplot2)
try2 <- try[which(try$type=="E1"),] #筛选出第一个动作电位
ggplot(try2,aes(x=v1,y=d1))+geom_point(aes(color=type))
#动作电位相位图
ggplot(try2,aes(x=t1,y=v1))+geom_point(aes(color=type))+geom_line()+
  geom_text(data = try2[which(try2$id==387),],aes(x=t1,y=v1,label=v1))
#动作电位图，标明threshold的值

#改变数据结构
library(reshape2)
try3 <- try[,c(1,2,3,6)]
meltdata <- reshape2::melt(try3,id=c("t1","v1","type"))
fg <- reshape2::dcast(meltdata,t1+v1~type)
write.csv(fg,"E:/R/PCA/20210629_cell1phase.csv",na="",row.names = FALSE)

#计算动作的其他数值
try2 <- subset(try,type=="E1")
Peak <- max(try2$v1)
trough <- min(try2$d1)




