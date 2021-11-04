Total <- read.table("E:/R/patch/20210107_235cells.txt",row.names = NULL,header = TRUE,sep = '\t')
rownames(Total) <- Total[,1]
Ephys <- na.omit(Total[,35:47]) #省略缺失值#
Annotation <- na.omit(Total)
nEphys<-apply(Ephys,2,scale) #对数据进行scale#
rownames(nEphys) <- rownames(Ephys)

Ephysdata <- prcomp(nEphys) #计算主成分#
PCimportance <- Ephysdata$rotation #显示各个PC成分中最主要的参数#
#PC1:Freq,UDR,Cm; PC2: Rin,halfwidth,threshold#
summ <- summary(Ephysdata) #多了importance, 计算多个PC的方差，计算累计概率#
d <- round(summ$importance[2,1]*100,2)

library(ggbiplot)
#改变点的大小#
p<-ggbiplot(Ephysdata, obs.scale = 1, var.scale = 1,
            groups = Annotation$Group, 
            ellipse = TRUE, epplipse.prob=0.8,circle = FALSE,
            labels.size = 10,varname.size = 5,varname.adjust = 2)+
  geom_point(aes(colour=Annotation$Group),size=2)+scale_color_manual(values=c('#00be67','#00A9FF','#FF61CC'))+
  theme(panel.grid=element_blank(),legend.direction = 'horizontal',legend.position = 'none')
ggsave("E:/R/Morpho2/20210120_PCA_164_Group.pdf",p,width=5,height=4)


#简易版#
p <- ggbiplot(Ephysdata,obs.scale = 1, var.scale = 1,groups = Annotation$Ephys_Cluster)
#X,Y, PC1,PC2 对应百分比#
xlab <- paste0("PC1 ",round(summ$importance[2,1]*100,2),"%") #PC1 方差的百分比，paste0 字符串合并函数#
ylab <- paste0("PC2 ",round(summ$importance[2,2]*100,2),"%")
#调节x,y比例#
p2 <- p+coord_fixed(ratio=0.8) #改变ratio改变xy比例#
#改变x轴范围#
p2 <- p+scale_x_continuous(limits=c(-5,5))
#改变色调#
p2<-p+scale_color_discrete(h=c(150,300)) #色调范围（h）、饱和度（c）和亮度（l）#


#Ephys heatmap#
mycol<-colorRampPalette(c("blue","white","red"))(100)
library(pheatmap)
p1<-pheatmap(nEphys,color=mycol,border_color="white",show_rownames=F)
order <- Annotation$Ephys_Cluster
range(nEphys)
bk=unique(c(seq(-3,3,length=100)))
ano_row <- data.frame(Annotation$Cell_Annotation,Annotation$Layer,Annotation$Brain_area,Annotation$Pathology,Annotation$Ephys_Cluster)
rownames(ano_row) <- rownames(Annotation)
colnames(ano_row) <- c("Cell_Annotation","Layer","Brain_area","Pathology","Ephys_Cluster")
ann_colors<-list(Ephys_Cluster=c(E1="#f8766d",E2="#e76bf3",E3="#00bf7d",E4="#00b0f6",E5="#a3a500"),
                 Cell_Annotation=c(DN="#F8766D",FS="#A3A500",immature="#E76BF3",INT="#00BF7D",PY="#00B0F6"),
                 Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF"),
                 Brain_area=c(FL="#F8766D",IL="#A3A500",PL="#E76BF3",TL="#00BF7D"),
                 Pathology=c(FCD="#F8766D",PT="#9FA0A0",TLE="#619CFF"))
p2<-pheatmap(nEphys,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row,breaks = bk,annotation_colors = ann_colors)
p2

#检查ann_color 哪条出错#
ano_row2 <- data.frame(ano_row$Layer)
rownames(ano_row2) <- rownames(ano_row)
colnames(ano_row2) <- "Layer"
ann_colors<-list(
  Layer=c(deeper="#F8766D",unknown="#9FA0A0",upper="#619CFF")    
                )
p2<-pheatmap(nEphys,color=mycol,border_color=NA,show_rownames=F,annotation_row = ano_row2,breaks = bk,annotation_colors = ann_colors)
p2
pdf("E:/R/Morpho2/20210107_Ephys_pheatmap.pdf",width = 8,height = 12)
print(p2)
dev.off()
#尝试#
nEphys_t <- as.data.frame(t(nEphys))
col_dist = dist (nEphys_t)
hclust_1 <- hclust(col_dist)
p1 <- pheatmap(nEphys,cluster_cols = hclust_1)
manual_order = c("cell1","cell2","cell3","cell4","cell5")
dend = reorder(as.dendrogram(hclust_1),wts = order(match(manual_order,rownames(nEphys_t))))
col_cluster <- as.hclust(dend)
p2 <- pheatmap(nEphys,cluster_cols = col_cluster,annotation_row = ano_row)
p2


