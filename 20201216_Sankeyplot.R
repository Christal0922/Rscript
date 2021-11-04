data<-read.table('E:/R/PCA/20181216_sankey.txt',sep = '\t', header = T,quote = '')
library(networkD3) #桑基图（Sankey diagram）
#导入数据的起点终点是factor类型，需要将其转为chr，否则之后unique的是因子水平，不能进行合并。
#数据要整成3列，s为起点，t为终点，v为数目（以某一起点和终点为两端的）
library(readxl)
data <- read_xls("PCA/20210203_sankey.xls")
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
Sankeydata<-Sankeylinks[,c(4,5,3)]
#将三列数据分别命名
names(Sankeydata)<-c("Source","Target","Value")  
#取点的名称，即第一列
Sankeyname<-Sankeynodes[,1,drop=FALSE]
#保存成网页格式
p1 <- sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 8, nodeWidth = 20)
p1
color <- 'd3.scaleOrdinal() .
domain(["M0","M1", "M2", "M3", "M4", "M5","E0","E1","E2","E3","upper_DN","deeper_DN","upper_PY","deeper_PY"]) .
range(["#C9CACA","#f8766d", "#e76bf3", "#00bf7d", "#00b0f6", "#a3a500","#C9CACA","#00d766","#ffa148","#A0B0FF","#F08CAA","#C81E50","#6482FF","#001E96"])'
p2<-sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 10,nodePadding=3, nodeWidth = 20,height=315,width=385,colourScale=color)
p2
p3<-sankeyNetwork(Links=Sankeydata,Nodes=Sankeyname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",  fontSize = 10,nodePadding=3, nodeWidth = 20,height=315,width=385,colourScale=color,fontFamily = "Arial")
p3


#change color#
Sankeyname$group1 <- Sankeyname$group1
Sankeyname$color <- c("#00D766","#FFA148","#A0B0FF","#F08CAA","#C81E50","#6482FF","#001E96","#F8766D","#E76BF3","#00BF7D","#00B0F6","#A3A500")
Sankeylinks$links <- c("C81E50","C81E50","C81E50","001E96","001E96","001E96","F08CAA","C81E50","6482FF","001E96","F08CAA","C81E50","001E96","F08CAA","6482FF","001E96","C81E50","6482FF","F08CAA","001E96","6482FF","F08CAA","F08CAA","6482FF","6482FF","6482FF")
Sankeylinks$links2 <- paste("#",Sankeylinks$links,sep="")
Sankeydata<-Sankeylinks[,c(4,5,3,7)]
names(Sankeydata)<-c("Source","Target","Value","links2") 
colors <- paste(sapply(Sankeyname$color, function(x) { paste0("d3.rgb(", paste(c(col2rgb(x), 1), collapse = "," ), ")") }), collapse = ", ")
colorJS <- paste0('d3.scaleOrdinal([', colors, '])')
p5 <- sankeyNetwork(Links = Sankeydata, Nodes = Sankeyname,
              Source = 'Source', Target = 'Target', Value = 'Value',
              NodeID = 'name', NodeGroup = "group", LinkGroup = "links2",
              colourScale = colorJS)
p5



