raw <- read.csv("E:/R/PCA/20210629_dv2.csv")
raw$id <- as.numeric(rownames(raw))
raw2 <- raw[raw$d1>=10,]
id2 <- raw2$id
raw3 <- raw2[diff(raw2$id)>1,]
id3 <- raw3[raw3$v1>0,4]
id1 <- raw2$id[1]
for (i in 1:length(id3)){
  id1 <- c(id1,id2[which(id2==id3[i])+1])
  i=i+1
}

#检测1ms内是否v>0，若v>0，则存在动作电位，若没有，则threshold检测失误，剔除检测失误值
thresholdid <- id1
endid <- NA
e <- paste0(rep("E"),1:length(thresholdid)) 
APnum <- length(thresholdid) #动作电位个数
id4 <- c(thresholdid,length(raw$id))

raw$type <- NA
for (i in 1:length(thresholdid)){
  ifelse((id4[i]+1000)<=id4[i+1],endid <- id4[i]+600,endid <- id4[i+1])
  T1 <- subset(raw,(id>=(id4[i]-20))&(id<=endid))
  Peak <- max(T1$v1)
  Peakid <- T1[T1$v1==max(T1$v1),4][1]
  Peakpos <- T1[T1$v1==max(T1$v1),1][1]
  trough <- T1[T1$v1==min(T1$v1)&(T1$id>Peakid),2][1]
  troughid <- T1[T1$v1==min(T1$v1)&(T1$id>Peakid),4][1]
  troughpos <- T1[T1$v1==min(T1$v1)&(T1$id>Peakid),1][1]
  end <- T1[(T1$d1>-5)&(T1$id>=troughid),4][1]
  raw$type[(id4[i]):(end)] <- e[i]
  i=i+1
}
#最后一个AP 不完整时
raw$type[(id4[i-1]):(endid)] <- e[i]

library(ggplot2)
try <- raw[complete.cases(raw),]
ggplot(try,aes(x=v1,y=d1))+geom_point(aes(color=type))
#所有动作电位相位图
library(reshape2)
try3 <- try[,c(1,2,3,5)]
meltdata <- reshape2::melt(try3,id=c("t1","v1","type"))
fg <- reshape2::dcast(meltdata,t1+v1~type)
write.csv(fg,"E:/R/PCA/20210705_try.csv",na="",row.names = FALSE)

for (i in 2:length(id1)){
  ifelse(sum(raw[id1[i]:(id1[i]+50),2]>0),thresholdid <- c(thresholdid,id1[i]),id2 <- c(id2))
  i=i+1
}



