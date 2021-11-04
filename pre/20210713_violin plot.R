# 输入数据------
Total<-read.table("E:/R/patch/20201124_norm.235cells.txt",row.names=NULL,sep = '\t',header=T) #读入全部细胞基因表达list#
DEG <- c("STING1","TMEM173")
DEG_matrix<-Total[match(DEG,Total$Genename),]
DEG_matrix <- DEG_matrix[complete.cases(DEG_matrix),]
rownames(DEG_matrix) <- DEG_matrix$Genename
logexp<-t(data.frame(log2(1+DEG_matrix[,2:236])))
logexp <- data.frame(rownames(logexp),logexp)
Total_anno<-read.table("E:/R/patch/20201218_235cells.txt",row.names=1L,sep = '\t',header=T) #读入加annotation的表格#
Total_anno2<-data.frame(rownames(Total_anno),Total_anno$group,Total_anno$Predicted_cellType) #Total_anno2提取group信息#
T_merge<-merge(Total_anno2,logexp,by.x="rownames.Total_anno.",by.y="rownames.logexp.") #合并Tlog以及Total_anno2, 使其具有group信息, 得到带分组类别的表达举证#
colnames(T_merge)[1:3] <- c("SeqID","group","Predict_celltype")

#作图---------
#~ 以group形式 ####
library(ggplot2)
#group形式
mytheme <- theme(plot.title = element_text(size=24,hjust=0.5),
                 axis.title = element_text(size = 24),
                 axis.line.x = element_line(size = 1),
                 axis.line.y = element_line(size = 1),
                 axis.text.x = element_text(size = 21,hjust = 0.8,color = "black"),
                 axis.text.y = element_text(size = 21,vjust=0.8,color = "black"),
                 legend.position="none",panel.grid=element_blank(),
                 panel.background = element_rect(fill = "white"))
group_anno <- c("IN","RPS6","PY")
p1 <- ggplot(T_merge,aes(x=group,y=T_merge[,4]))+
  geom_violin(aes(fill=group),scale = "width")+
  scale_x_discrete(labels=group_anno)+
  labs(x="",y="Expression Level",title = "TMEM17")+
  scale_fill_manual(values=c("#00BE67","#FF61CC","#00a9ff"))+
  geom_jitter(shape=16,size=1.5,fill="black",position=position_jitter(width=0.4))+mytheme
p1
ggsave("E:/R/Morpho2/20210713_Vlin.pdf",p1,width = 4.5,height = 3.8) #输出后变成原先的29.3%
#~ predict celltype形式 ####
mytheme <- theme(plot.title = element_text(size=24,hjust=0.5),
                 axis.title = element_text(size = 24),
                 axis.line.x = element_line(size = 1),
                 axis.line.y = element_line(size = 1),
                 axis.text.x = element_text(size = 21,angle=45,hjust = 1,color = "black"),
                 axis.text.y = element_text(size = 21,vjust=0.8,color = "black"),
                 legend.position="none",panel.grid=element_blank(),
                 panel.background = element_rect(fill = "white"))
Predict_anno <- c("IN-PV","IN-SST","IN-SV2C","IN-VIP",
                  "L4","L5/6-CC","Layer2/3","RPS6")
p2 <- ggplot(T_merge,aes(x=Predict_celltype,y=T_merge[,4]))+
  geom_violin(aes(fill=Predict_celltype),scale = "width")+
  scale_x_discrete(labels=Predict_anno)+
  scale_fill_manual(values=c("#f8766d","#d39200","#93aa00","#00ba38",
                             "#00c19f","#619cff","#db72f6","#ff61c3"))+
  labs(x="",y="Expression Level",title = colnames(T_merge)[4])+
  geom_jitter(shape=16,size=1.5,fill="black",position=position_jitter(width=0.4))+mytheme
p2
ggsave("E:/R/Morpho2/20210713_Vlin.pdf",p2,width = 4.5,height = 4.6) #输出后变成原先的29.3%
#~ 图片汇总 ####
p <- list(p1,p2)
pdf("E:/R/Morpho2/20210713_Vlin.pdf",width = 4.5,height = 4.6) #输出后变成原先的29.3%
print(p)
dev.off()
