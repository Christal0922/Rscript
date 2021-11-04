DEG<-read.table("E:/R/patch/20201126_GSEAgeneset.txt",sep = '\t',header=T) #读入DEG list#
DEGlist <- read.table("E:/R/patch/20201126_DEGlist.txt",sep = '\t',header=T) #读入DEGlist#
chemokine<-DEG[DEG$GO_POSITIVE_REGULATION_OF_CHEMOKINE_PRODUCTION%in%DEGlist$Gene,] #查找gene#
chemokine_notfound<-DEG[!DEG$GO_POSITIVE_REGULATION_OF_CHEMOKINE_PRODUCTION%in%DEGlist$Gene,] #查找gene#
chemokine_matrix<-DEGlist[match(chemokine$GO_POSITIVE_REGULATION_OF_CHEMOKINE_PRODUCTION,DEGlist$Gene),] #大表里提取找到的gene的矩阵#
MTOR<-DEG[DEG$HALLMARK_MTORC1_SIGNALING%in%DEGlist$Gene,] #查找gene#
MTOR_notfound<-DEG[!DEG$HALLMARK_MTORC1_SIGNALING%in%DEGlist$Gene,] #查找gene#
MTOR_matrix<-DEGlist[match(MTOR$HALLMARK_MTORC1_SIGNALING,DEGlist$Gene),] #大表里提取找到的gene的矩阵#
Inflammatory<-DEG[DEG$HALLMARK_INFLAMMATORY_RESPONSE%in%DEGlist$Gene,] #查找gene#
Inflammatory_notfound<-DEG[!DEG$HALLMARK_INFLAMMATORY_RESPONSE%in%DEGlist$Gene,] #查找gene#
Inflammatory_matrix<-DEGlist[match(Inflammatory$HALLMARK_INFLAMMATORY_RESPONSE,DEGlist$Gene),] #大表里提取找到的gene的矩阵#
Senescence<-DEG[DEG$FRIDMAN_SENESCENCE_UP%in%DEGlist$Gene,] #查找gene#
Senescence_notfound<-DEG[!DEG$FRIDMAN_SENESCENCE_UP%in%DEGlist$Gene,] #查找gene#
Senescence_matrix<-DEGlist[match(Senescence$FRIDMAN_SENESCENCE_UP,DEGlist$Gene),] #大表里提取找到的gene的矩阵#
all <- rbind(chemokine_matrix,MTOR_matrix,Inflammatory_matrix,Senescence_matrix) #整合数据框#
write.table(all,file="E:/R/patch/20201126_GSEAgene.txt",row.names=F,quote=F,sep='\t')
