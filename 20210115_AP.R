dt <- read.table("E:/R/PCA/20210115_AP.txt",header = T,sep = '\t')
dt2 <- dt[(dt$T>220)&(dt$T<715.61),]

row <- dt2[dt2$V>0,]
rowindex <- as.numeric(rownames(row))
rowend <- rowindex[length(rowindex)]

bj <-row[diff(rowindex)>1,]
bjindex <- as.numeric(rownames(bj))

lbj <- dt[rowindex[1]:(bjindex[1]+1),]
len <- length(lbj$T)
D1 <- dt[(rowindex[1]-len):(bjindex[1]+len),]
D1$type <- "D1"
num <- length(bjindex)
AP <- paste0("D",1:(num+1))

for (i in 1:(num-1)){
  D <- dt[(bjindex[i]):(rowindex[which(rowindex==bjindex[i+1])-1]),]
  D$type <-AP[i+1]
  D1 <- rbind(D1,D)
  i=i+1
}

D <- dt[(bjindex[num]):rowend,]
D$type <-AP[i+1]
D1 <- rbind(D1,D)

library(reshape2)
md <- melt(D1,id=c("T","V","type"))
md$variable <- md$type
newdata <- dcast(md,T+V~variable,mean)
write.table(newdata,file="E:/R/PCA/20210115_newdata.txt",col.names = TRUE,row.names=FALSE,sep = "\t")

library(ggplot2)
p1 <- ggplot(D1,aes(x=V,y=D))+geom_point()
p1

