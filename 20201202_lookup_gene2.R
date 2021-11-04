DEG<-read.table("E:/R/patch/20210106_GSEAterm.txt",sep = '\t',header=T) #¶ÁÈëDEG list#
DEGlist <- read.table("E:/R/patch/20201126_DEGlist.txt",sep = '\t',header=T) #¶ÁÈëDEGlist#
A<-DEG[DEG[,1]%in%DEGlist$Gene,1] #²éÕÒgene#
A <- data.frame(A)
A<-DEGlist[match(A[,1],DEGlist$Gene),]
A$TYPE <- colnames(DEG)[1]
b <- A
Val <- names(DEG)
num <- length(Val)
n=2
while(n<=num)
{
  t<-DEG[DEG[,n]%in%DEGlist$Gene,n]
  t <- data.frame(t)
  t <-DEGlist[match(t[,1],DEGlist$Gene),]
  t$TYPE <- colnames(DEG)[n]
  b <- rbind(b,t)
  n=n+1
}
write.table(b,file="E:/R/patch/20210106_GSEAgene.txt",row.names=F,quote=F,sep='\t')

