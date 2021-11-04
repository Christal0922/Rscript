data<-read.table('E:/R/PCA/20181216_sankey.txt',sep = '\t', header = T,quote = '')
library(networkD3) #桑基图（Sankey diagram）
#导入数据的起点终点是factor类型，需要将其转为chr，否则之后unique的是因子水平，不能进行合并。
#数据要整成3列，s为起点，t为终点，v为数目（以某一起点和终点为两端的）
data$s = as.character(data$s)    
data$t = as.character(data$t)
#取边的数据  
Sankeylinks<-data
#取点的数据，用unique去重，转化为数据框格式，并将列名设置为“name”
Sankeynodes<-data.frame(name=unique(c(Sankeylinks$s,Sankeylinks$t))) 
#增加设置1列index，方便后面合并，取值为0到总行数-1  
Sankeynodes$index<-0:(nrow(Sankeynodes) - 1)
#将边数据与点数据合并，来源点即s为第4列  
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="s",by.y="name") 
#将边数据与点数据合并，目标点即t为第5列
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="t",by.y="name") 
#取第4、5、3列数据，及来源、目标、边的值或权重
Sankeydata<-Sankeylinks[,c(5,6,3)]
#将三列数据分别命名
names(Sankeydata)<-c("Source","Target","Value")  
#取点的名称，即第一列
Sankeyname<-Sankeynodes[,1,drop=FALSE]
#保存成网页格式
p1 <- sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 8, nodeWidth = 20)
p1
color <- 'd3.scaleOrdinal() .
domain(["M1", "M2", "M3", "M4", "M5","E1","E2","E3","upper_DN","deeper_DN","upper_PY","deeper_PY"]) .
range(["#f8766d", "#e76bf3", "#00bf7d", "#00b0f6", "#a3a500","#00d766","#ffa148","#A0B0FF","#F08CAA","#C81E50","#6482FF","#001E96"])'
p2<-sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 10,nodePadding=3, nodeWidth = 20,height=315,width=385,colourScale=color)
p2
p3<-sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 10,nodePadding=3, nodeWidth = 20,height=315,width=385,colourScale=color,fontFamily = "Arial")
p3

rm(list = ls()) #清除变量#
library(openxlsx)
data <- read.xlsx('E:/R/PCA/20181216_sankey.xlsx')
data$Linkcolor <- as.character(data$Linkcolor)
data$V <- as.numeric(data$V)
data$s = as.character(data$s)    
data$t = as.character(data$t)
library(networkD3) #桑基图（Sankey diagram）
Sankeylinks<-data
#取点的数据，用unique去重，转化为数据框格式，并将列名设置为“name”
Sankeynodes<-data.frame(name=unique(c(Sankeylinks$s,Sankeylinks$t))) 
#增加设置1列index，方便后面合并，取值为0到总行数-1  
Sankeynodes$index<-0:(nrow(Sankeynodes) - 1)
#将边数据与点数据合并，来源点即s为第4列  
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="s",by.y="name") 
#将边数据与点数据合并，目标点即t为第5列
Sankeylinks<-merge(Sankeylinks,Sankeynodes,by.x="t",by.y="name") 
#取第4、5、3列数据，及来源、目标、边的值或权重
Sankeydata<-Sankeylinks[,c(6,7,3,4,5)]
#将三列数据分别命名
names(Sankeydata)<-c("Source","Target","Value","group","Linkcolor")  
#取点的名称，即第一列
Sankeyname<-Sankeynodes[,1,drop=FALSE]
#保存成网页格式
Linkcolor <- Sankeydata$Linkcolor
Linkcolor2 <- paste("#","3F",Linkcolor,sep = "")#75%透明度#
Sankeydata$Linkcolor2 <- Linkcolor2
p1 <- sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 8, nodeWidth = 20,LinkGroup = "Linkcolor2")
p1


data$linkcolor2 <- paste("#",data$Linkcolor,sep="")