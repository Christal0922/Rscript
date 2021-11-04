#读入系列文件#
library(readxl)
b <- list.files("TRY3/")
dir <- paste("./TRY3/",b,sep = "")
n=length(dir)
merge.data <- read_xls(dir[1])
merge.data$Type <- b[1]
for (i in 2:n){
  new.data <- read_xls(dir[i])
  new.data$Type <- b[i]
  merge.data <- rbind(merge.data,new.data)
}
write.csv(merge.data,"E:/R/patch/20210201_mergedata.csv",row.names=F)

#读入mergedata#
merge.data <- read.csv("patch/20210201_mergedata.csv")

#读入logfc#
DEGlist <- read.table("E:/R/patch/20210128_DEG.txt",sep = '\t',header = T)

#读入expression level#
expr <- read.table("patch/20210201_178cells.txt",sep = '\t',header = TRUE)
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
anno2 <- Total_anno[which(Total_anno$group!="IN"),]

#计算#
d <- merge(merge.data,DEGlist,by.x = "PROBE",by.y = "Gene",all.x = TRUE)
d <- d[order(d$Type),]
c <- data.frame(d$PROBE,d$Type,d$avg_logFC,d$CORE.ENRICHMENT,d$NAME)

library(stringr)
c$line <- as.numeric(str_split(c$d.NAME,'_',simplify=T)[,2])
c3 <- c[order(c$d.Type,c$line),]
c4 <- subset(c3,select=c("d.PROBE","d.Type","d.NAME", "d.avg_logFC","d.CORE.ENRICHMENT"))
library(reshape2)
c5 <- melt(c4,id=c("d.PROBE","d.NAME","d.CORE.ENRICHMENT","d.Type"))
C6 <- dcast(c5,d.PROBE+d.NAME+d.CORE.ENRICHMENT~d.Type)
write.csv(c5,"patch/20210202_c5.csv",row.names = F)


library(reshape2)
b <- melt(c,id=c("d.PROBE","d.Type"))
b2 <- dcast(b,d.Type~d.PROBE)
rownames(b2) <- b2$d.Type
b2 <- b2[,-1]
c2 <- t(b2)
data <- data.frame(row.names(c2),c2[,1])
data <- data[complete.cases(data),]
write.csv(c2,"patch/20210202_c2.csv")
write.csv(d,"patch/20210202_d.csv",row.names = F)
library(viridis)
d <- viridis(7)

c2 <- read.csv("patch/20210202_c2.csv",row.names = 1)
range(c2,na.rm = TRUE)

library(ggplot2)
mycol <- colorRampPalette(c("white","tomato"))(100)
attach(d)
p <- ggplot(d,aes(x=FRIDMAN_SENESCENCE_UP,y=Gene))+
  geom_point(aes(color=frid2))+scale_color_gradient(low = "blue", high = "red",limit=c(0,6))
p

library(reshape2)
up_tri_melt <- melt(c,id=c("d.PROBE","d.Type"),na.rm=TRUE)
p <- ggplot(data = up_tri_melt,aes(x=d.Type,y=d.PROBE,fill=value))+
  geom_tile(color="white")
p

up2 <- subset(up_tri_melt,d.Type=="1.FRIDMAN_SENESCENCE_UP.xls")
up2 <- up2[order(up2$value),]
p <- ggplot(data = up2,aes(x=d.Type,y=d.PROBE,fill=value))+
  geom_tile(color="white")
p



