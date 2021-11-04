dt <- read.csv("E:/R/PCA/20210629_phase_raw.csv")
num <- length(dt)
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
md <- reshape2::melt(newdata,id=c("t1","v1","type"))
newdata2 <- reshape2::dcast(md,t1+v1~type)

write.csv(newdata2,file="E:/R/PCA/2020629_processed_phase.csv",na="",row.names=FALSE)


