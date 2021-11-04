Total <- read.table("E:/R/patch/20210107_235cells.txt",row.names = NULL,header = TRUE,sep = '\t')
rownames(Total) <- Total[,1]
Ephys <- na.omit(Total[,35:47]) #ʡ��ȱʧֵ#
Annotation <- na.omit(Total)
Type <- data.frame(Annotation$Ephys_Cluster,Annotation$Cell_Annotation)
colnames(Type) <- c("s","t")
data <- as.data.frame(table(Type$s,Type$t))
colnames(data) <- c("s","t","v")
library(networkD3)
data$s = as.character(data$s)  
data$t = as.character(data$t)
data2 <- data[which(data$v!=0),]
Sankeylinks <- data2
Sankeynodes<-data.frame(name=unique(c(Sankeylinks$s,Sankeylinks$t))) 
Sankeynodes$index<-0:(nrow(Sankeynodes) - 1)
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="s",by.y="name") 
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="t",by.y="name")
Sankeydata<-Sankeylinks[,c(4,5,3)]
names(Sankeydata)<-c("Source","Target","Value") 
Sankeyname<-Sankeynodes[,1,drop=FALSE]
p1 <- sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 8, nodeWidth = 20)
p1
color <- 'd3.scaleOrdinal() .
domain(["E1", "E2", "E3", "E4", "E5","DN","FS","immature","INT","PY"]) .
range(["#f8766d", "#e76bf3", "#00bf7d", "#00b0f6", "#a3a500","#F8766D","#A3A500","#E76BF3","#00BF7D","#00B0F6"])'
p2<-sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 10,nodePadding=3, nodeWidth = 20,height=315,width=315,colourScale=color,fontFamily = "Arial")
p2




