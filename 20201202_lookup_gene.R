DEG<-read.table("E:/R/patch/20201202_GSEAterm.txt",sep = '\t',header=T) #读入DEG list#
DEGlist <- read.table("E:/R/patch/20201126_DEGlist.txt",sep = '\t',header=T) #读入DEGlist#
A<-DEG[DEG$A%in%DEGlist$Gene,1] #查找gene#
A <- data.frame(A) 
A_matrix<-DEGlist[match(A$A,DEGlist$Gene),] #大表里提取找到的gene的矩阵#
A_matrix$TYPE <- c("FRIDMAN_SENESCENCE_UP") #标记term#
B<-DEG[DEG$B%in%DEGlist$Gene,2] #查找gene#
B <- data.frame(B) 
B_matrix<-DEGlist[match(B$B,DEGlist$Gene),] #大表里提取找到的gene的矩阵#
B_matrix$TYPE <- c("GO_CELL_CHEMOTAXIS")


matrix1 <- DEG[,1]
matrix1 <- data.frame(matrix1)
colnames(matrix1) <- "gene"
matrix1$Type <- colnames(DEG)[1]
b <- matrix1
n=2
while(n<=3)
t <- DEG[,n]
t <- data.frame(t)
colnames(t) <- "gene"
t$Type <- colnames(DEG)[n]
b <- data.frame(b,t)
n=n+1