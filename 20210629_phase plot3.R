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

#找到动作电位，按照threshold+6ms的间隔
e <- paste0(rep("E"),1:length(thresholdid)) 
APnum <- length(thresholdid) #动作电位个数
raw$type <- NA
for (i in 1:length(thresholdid)){
  raw$type[(thresholdid[i]):(thresholdid[i]+600)] <- e[i]
  i=i+1
} #区分动作电位，用E1，E2等标记动作电位
try <- raw[complete.cases(raw),] #筛选出标记动作电位的有效数据


#筛选某个动作电位作图分析
library(ggplot2)
ggplot(try,aes(x=v1,y=d1))+geom_point(aes(color=type))
#所有动作电位相位图
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
write.csv(fg,"E:/R/PCA/20210629_try.csv",na="",row.names = FALSE)

#给出每个动作电位的peak，threshold等值
options(digits = 2)
i=1
trydata <- subset(try,type==e[i])
threshold <- trydata[trydata$id==thresholdid[i],2]
Peak <- max(trydata$v1)
Peakpos <- trydata[which(trydata$v1==max(trydata$v1))[1],1]
trough <- min(trydata$v1)
Amp <- Peak-threshold
hwtest <- (0.5*Amp+threshold)
Peakposid <- trydata[which(trydata$v1==max(trydata$v1))[1],4]
trydata2 <- subset(trydata,id>=Peakposid)
end <- trydata2[which(trydata2$v1==min(trydata2$v1))[1],4]
id5 <- trydata[trydata$v1<=hwtest,4]
hwid1 <- last(id5[(id5<Peakposid)&(id5>thresholdid[i])])
id6 <- trydata[trydata$v1>=hwtest,4]
hwid2 <- last(id6[(id6>Peakposid)&(id6<end)])
halfwidth <- (trydata[trydata$id==hwid2,1]-trydata[trydata$id==hwid1,1])
parameter <- data.frame(threshold,Peak,Peakpos,trough,Amp,halfwidth,e[i])
for (i in 1:length(thresholdid)){
  trydata <- subset(try,type==e[i])
  threshold <- trydata[trydata$id==thresholdid[i],2]
  Peak <- max(trydata$v1)
  Peakpos <- trydata[which(trydata$v1==max(trydata$v1))[1],1]
  trough <- min(trydata$v1)
  Amp <- Peak-threshold
  hwtest <- (0.5*Amp+threshold)
  Peakposid <- trydata[which(trydata$v1==max(trydata$v1))[1],4]
  trydata2 <- subset(trydata,id>=Peakposid)
  end <- trydata2[which(trydata2$v1==min(trydata2$v1))[1],4]
  id5 <- trydata[trydata$v1<=hwtest,4]
  hwid1 <- last(id5[(id5<Peakposid)&(id5>thresholdid[i])])
  id6 <- trydata[trydata$v1>=hwtest,4]
  hwid2 <- last(id6[(id6>Peakposid)&(id6<end)])
  halfwidth <- (trydata[trydata$id==hwid2,1]-trydata[trydata$id==hwid1,1])
  parameter[i,] <- data.frame(threshold,Peak,Peakpos,trough,Amp,halfwidth,e[i])
  i=i+1
}
write.csv(parameter,"E:/R/PCA/20210629_APparameter.csv",row.names = FALSE)



#简化数据，确定end
options(digits = 2)
AP <- matrix(data = NA,ncol = 6,dimnames = list(NULL,c("t1", "v1","d1","id","row2","type")))
for (i in 1:length(thresholdid)){
  trydata <- subset(try,type==e[i])
  Peakposid <- trydata[which(trydata$v1==max(trydata$v1))[1],4]
  trydata2 <- subset(trydata,id>=Peakposid)
  end <- trydata2[which(trydata2$v1==min(trydata2$v1))[1],4]
  APtrace <- trydata[trydata$id<=end,]
  AP <- rbind(AP,APtrace)
  i=i+1
}
library(reshape2)
APt <- AP[-1,c(1,2,3,6)]
meltdata <- reshape2::melt(APt,id=c("t1","v1","type"))
fg <- reshape2::dcast(meltdata,t1+v1~type)
write.csv(fg,"E:/R/PCA/20210629_phase.csv",na="",row.names = FALSE)

#测试phase plot
library(ggplot2)
ggplot(AP[-1,],aes(x=v1,y=d1))+geom_point(aes(color=type))
#动作电位相位图
AP2 <- AP[which(AP$type=="E2"),] #筛选出第一个动作电位
ggplot(AP2,aes(x=v1,y=d1))+geom_point(aes(color=type))
#动作电位相位图
ggplot(AP2,aes(x=t1,y=v1))+geom_point(aes(color=type))+geom_line()+
  geom_text(data = AP2[which(AP2$id==387),],aes(x=t1,y=v1,label=v1))
#动作电位图，标明threshold的值



#计算动作的其他数值
APnum <- length(thresholdid)
try2 <- subset(try,type=="E1")
threshold <- try2[thresholdid[1],2]
Peak <- max(try2$v1)
trough <- min(try2$v1)
troughpos <- try2[which(try2$v1==min(try2$v1))[1],1]
end <- try2[which(try2$v1==min(try2$v1))[1],4]
Amp <- Peak-threshold
Peakposid <- try2[which(try2$v1==max(try2$v1))[1],4]
try3 <- subset(try2,id>=Peakposid)
end <- try3[which(try3$v1==min(try3$v1))[1],4]
#计算halfwidth
hwtest <- (0.5*Amp+threshold)
try2$hw1 <- (try2$v1<=hwtest)
try5 <- try2[try2$hw1,]
id5 <- try5$id
hwid1 <- id5[(id5<Peakposid)&(id5>thresholdid[1])]
hwid1 <- last(hwid1)
id6 <- try2[try2$v1>=hwtest,4]
hwid2 <- last(id6[(id6>Peakposid)&(id6<end)])
halfwidth <- (try2[try2$id==hwid2,1]-try2[try2$id==hwid1,1])
parameter <- data.frame(threshold,Peak,Peakpos,trough,Amp,halfwidth,e[1])

try4 <- try2[try2$id<=end,]
ggplot(try4,aes(x=t1,y=v1))+geom_point(aes(color=type))+geom_line()+
  geom_text(data = try2[which(try2$id==387),],aes(x=t1,y=v1,label=v1))
#动作电位图，标明threshold的值



#循环测试
i=1
trydata <- subset(try,type==e[i])
threshold <- trydata[trydata$id==thresholdid[i],2]
Peak <- max(trydata$v1)
Peakpos <- trydata[which(trydata$v1==max(trydata$v1))[1],1]
trough <- min(trydata$v1)
Amp <- Peak-threshold
parameter[i,] <- data.frame(threshold,Peak,Peakpos,trough,Amp,e[i])
DT5 <- trydata[trydata$d1<0,] #筛选出dv/dt<0的值，查找动作电位结束位置
DT6 <- DT5[which(diff(DT5$id)>1),]
end <- DT6$id[2]
APtrace <- trydata[trydata$id<=end,]
#动作电位图
ggplot(APtrace,aes(x=t1,y=v1))+geom_point(aes(color=type))+geom_line()
#相位图
ggplot(APtrace,aes(x=v1,y=d1))+geom_point(aes(color=type))

#改变数据结构
library(reshape2)
try3 <- try[,c(1,2,3,6)]
meltdata <- reshape2::melt(try3,id=c("t1","v1","type"))
fg <- reshape2::dcast(meltdata,t1+v1~type)
write.csv(fg,"E:/R/PCA/20210629_cell1phase.csv",na="",row.names = FALSE)
