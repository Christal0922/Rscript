dt <- read.table("E:/R/PCA/20201229_phase.txt",header = TRUE,sep = "\t")
val <- names(dt)
num <- length(val)
d1 <- dt[,1:3]
type <- paste("d",1:(num/3),sep="")
d1$type <- type[1]
i=2
for (i in 2:(num/3)) {
  d <- dt[,(3*i-2):(3*i)]
  d$type <- type[i]
  colnames(d)[1:3] <- c("t1","v1","d1")
  d1 <- rbind(d1,d)
  i=i+1
}
library(reshape2)
newdata <- na.omit(d1)
md <- melt(newdata,id=c("t1","v1","type"))
newdata2 <- dcast(md,t1+v1~type)
write.table(newdata2,file="E:/R/PCA/20201229_newdata.txt",col.names = TRUE,row.names=FALSE,sep = "\t")