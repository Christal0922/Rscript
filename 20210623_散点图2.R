library(openxlsx)
Morpho <- read.xlsx(xlsxFile="E:/R/PCA/20210623_morpho89.xlsx",sheet=1,rowNames = TRUE)
Val <- as.matrix(colnames(Morpho))
attach(Morpho)
MClust_new <- as.factor(MClust_new)
EClust.340. <- as.factor(EClust.340.)
Ephys <- Morpho[complete.cases(Morpho),]

#Morphoanno
Morphoanno <- Morpho[,1:22]
Morpho2 <- Morpho[,c(23:34)]
nMorpho2 <- apply(Morpho2,2,scale) #按列进行标准化
rownames(nMorpho2) <- rownames(Morpho2)
nMorpho3 <- nMorpho2[,c(-2,-5,-7)]
#计算PCA
morphodata <- prcomp(nMorpho3) #计算主成分#
PCimportance <- morphodata$rotation #显示各个PC成分中最主要的参数#
#PC1:dendritic ends,dendrite total length,  dendritic nodes; PC2: average local angle, average contraction#
summ <- summary(morphodata) #多了importance, 计算多个PC的方差，计算累计概率#
#PCA作图方式1-ggbiplot
library(ggbiplot)
p<-ggbiplot(morphodata, obs.scale = 1, var.scale = 1,
            groups = Morphoanno$MClust_new, 
            ellipse = TRUE, epplipse.prob=0.8,circle = FALSE,
            labels.size = 8,varname.size = 3,varname.adjust = 1)+
  geom_point(aes(colour=Morphoanno$MClust_new),size=2)+scale_color_manual(values=c('#FF9289','#82B7FF','#00D65C'))+
  theme(panel.grid=element_blank(),legend.direction = 'horizontal',legend.position = 'no',
        axis.title = element_text(size = 12),axis.text = element_text(size=10),
        panel.background = element_rect(fill = "white",color = "black"))
p
#PCA作图方式2-ggplot2
dt <- morphodata$x
df <- data.frame(dt,Morphoanno)
xlab <- paste0("PC1(",round(summ$importance[2,1]*100,2),"%)") #round函数，取两位小数
ylab <- paste0("PC2(",round(summ$importance[2,2]*100,2),"%)") #round函数，取两位小数
p <- ggplot(df,aes(x=PC1,y=PC2,color=MClust_new))+geom_point()
p
p <- ggplot(df,aes(x=PC1,y=PC2,color=MClust_new))+
  geom_point()+labs(x=xlab,y=ylab,title = "PCA")+
  stat_ellipse(aes(fill=MClust_new),type = "norm",geom = "polygon",alpha=0.2,color=NA)
p
#PCA作图方式2-ggord
#ggord安装
install.packages('devtools')
library(devtools)
install_github('fawda123/ggord')
#作图
library(ggord)
p <- ggord(morphodata,MClust_new,coord_fix=F,labcol='purple',repel=TRUE)
p
p <- ggord(morphodata,MClust_new,coord_fix=F,labcol='purple',repel=TRUE,
           arrow=0.25,vec_ext=3,veccol='blue',veclsz=0.5,vectyp='solid',#改变箭头（特征向量）的scaling箭头大小，arrow length箭头长度，line color,size粗细，type，arrow=1,vec_ext=0,txt=NULL可以隐藏箭头
           cols=c('purple','orange','blue'),size=2,alpha=0.7,#自定义点的大小和颜色，透明度
           poly=FALSE,polylntyp="dashed",#改变椭圆透明度，置信椭圆的描边样式
           )
p




#welch's anova分析
oneway.test(Rheobase~MClust_new,data = Morpho,var.equal = F)
#多重比较
library(multcomp)
fit <- aov(Rheobase~MClust_new)
tuck <- glht(fit,linfct=mcp(MClust_new="Tukey"))
plot(cld(tuck,level=0.05),col="lightgrey")

#将数据转换成因子
unique(Morpho$cell.type3)
Morpho$cell.type4[Morpho$cell.type3 =="upper_DN"]<- 0
Morpho$cell.type4[Morpho$cell.type3 =="upper_PY"]<- 1
Morpho$cell.type4[Morpho$cell.type3 =="deeper_DN"]<- 2
Morpho$cell.type4[Morpho$cell.type3 =="deeper_PY"]<- 3
Morpho$cell.type4 <- factor(Morpho$cell.type4,levels = c(0,1,2,3),
                            labels = c("upper_DN","upper_PY","deeper_DN","deeper_PY"))


#散点图
library(ggplot2)
#简易版
p1 <- ggplot(Morpho,aes(x=MClust_new,y=Morpho[,23],color=cell.type4,shape=cell.type4))+
  geom_point(size=2)+labs(x="",y=Val[23])
p1
#设置标签 labs=(x="",y="",title="")

#设置形状和颜色 color=

#直方图
p <- ggplot(Morpho,aes(x=Rheobase))+geom_histogram()
p
#箱线图
p <- ggplot(Morpho,aes(x=MClust_new,y=Rheobase))+geom_boxplot()
p
#组合图
p <- ggplot(Ephys,aes(x=MClust_new,y=Rheobase))+
  geom_boxplot(fill="cornflowerblue",color="black",notch = FALSE)+ #notch, 是否为缺口
  geom_point(position = "jitter",color="blue",alpha=0.5)+
  geom_rug(sides="l",color="black") #地毯图的安置，"b"=底部，"l"=左部，"t"=顶部，"r"=右部，"bl"=左下部
p

#小提琴图箱线图组合图
p <- ggplot(Ephys,aes(x=MClust_new,y=Threshold))+
  geom_violin(fill="lightblue")+
  geom_boxplot(fill="lightgreen",width=0.2)
p
#密度图
p <- ggplot(Morpho,aes(x=Cell.Body.Volume,fill=MClust_new))+
  geom_density(alpha=0.3)
p
#探索不同变量间的关系
p <- ggplot(Morpho,aes(x=Cell.Body.Volume,y=Cm,color=MClust_new,shape=cell.type4))+
  geom_point()
p
#条形图,堆叠图
p <- ggplot(Ephys,aes(x=MClust_new,fill=EClust.340.))+
  geom_bar(position = "stack")
p <- ggplot(Ephys,aes(x=MClust_new,fill=EClust.340.))+
  geom_bar(position = "dodge")
p <- ggplot(Ephys,aes(x=MClust_new,fill=EClust.340.))+
  geom_bar(position = "fill")+labs(y="proportion")
p
#刻面图 facet_wrap()和facet_grid()
p <- ggplot(Ephys,aes(x=Cm))+geom_histogram()+
  facet_wrap(~MClust_new,nrow = 3)
p <- ggplot(Ephys,aes(x=Cm,y=Rheobase,color=MClust_new,shape=MClust_new))+
  geom_point()+facet_grid(.~MClust_new)
p <- ggplot(Ephys,aes(x=Cm,fill=MClust_new))+geom_density()+
  facet_grid(MClust_new~.)
p

#添加光滑曲线
ggplot(Ephys,aes(x=Cm,y=Rheobase))+
  geom_smooth()+geom_point()
#按Clust回归
ggplot(Ephys,aes(x=Cm,y=Rheobase,linetype=EClust.340.,shape=EClust.340.,color=EClust.340.))+
  geom_smooth(method =lm,formula=y~poly(x,2),se=FALSE,size=1)+ #拟合二次多项式回归
  geom_point(size=2)
#修改ggplot2图形的外观
ggplot(Ephys,aes(x=MClust_new,y=Cm,fill=EClust.340.))+
  geom_boxplot()+
  scale_x_discrete(breaks=c("M1","M2","M3"),
                            labels=c("M1cluster","M2cluster","M3cluster"))+ #x轴标签重命名
  scale_y_continuous(breaks=c(100,200,300,400),
                     labels = c("0.1k","0.2k","0.3k","0.4k")) #y轴标签重命名
#图例
ggplot(Ephys,aes(x=MClust_new,y=Cm,fill=EClust.340.))+
  geom_boxplot()+
  scale_x_discrete(breaks=c("M1","M2","M3"),
                   labels=c("M1cluster","M2cluster","M3cluster"))+
  scale_y_continuous(breaks=c(100,200,300,400),
                     labels = c("0.1k","0.2k","0.3k","0.4k"))+
  labs(title="test",x="",y="Cm",fill="Ecluster")+ #fill改变legend的主名称
  theme(legend.position = c(.1,.8)) #设置legend的位置，图例的左上角是分别距离左侧边缘10%和底部边缘80%的部分，删除图例，legend.position="none"
#标尺
ggplot(Ephys,aes(x=Cm,y=Rin,size=Rheobase))+
  geom_point(shape=21,color="black",fill="cornsilk")+
  labs(x="membrane capacitance",y="input resistance",title="Buble Chart",size="Rheobase")
#设置颜色
ggplot(Ephys,aes(x=Cm,y=Rin,color=MClust_new))+
  scale_color_manual(values = c("orange","olivedrab","navy"))+
  geom_point(size=2)
ggplot(Ephys,aes(x=Cm,y=Rin,color=MClust_new))+
  scale_color_brewer(palette = "Set2")+
  geom_point(size=2)
#palette="Set1"可用其他的值，"Set2"、"Set3"、"Pastel1"、"Pastel2"、"Paired"、"Dark2"或"Accent"
library(RColorBrewer)
display.brewer.all()
display.brewer.all(type = "seq") #"seq"连续型，"div"极端型，"qual"离散型，
barplot(rep(1,3),col=brewer.pal(6,"YlOrRd")) #前面的rep(1,3)表示取3个颜色，后面的pal(6)表示将颜色分成6份
barplot(rep(1,6),col=brewer.pal(11,"RdGy")[2:7])# 第2至第7个颜色
a <- brewer.pal(9,"YlOrRd")[c(1,3,5)]
ggplot(Ephys,aes(x=Cm,y=Rin,color=MClust_new))+
  scale_color_manual(values = a)+
  geom_point(size=2)

#主题
mytheme <- theme(plot.title = element_text(
  face = "bold.italic",size = "14",color = "brown"),
  axis.title=element_text(face="bold.italic",size=10,color ="brown"),
  axis.text=element_text(face="bold",size=9,color = "darkblue"),
  panel.background = element_rect(fill = "white",color = "darkblue"),
  panel.grid.major.y = element_line(color = "grey",linetype = 1),
  panel.grid.minor.y = element_line(color = "grey",linetype = 2),
  panel.grid.minor.x = element_blank(),
  legend.position = "top")
 
p <- ggplot(Ephys,aes(x=Cm,y=Rin,color=MClust_new))+
  geom_point()+labs(title = "Ephys")+
  mytheme
p

#多重图
p1 <- ggplot(Ephys,aes(x=Cm))+geom_bar()
p2 <- ggplot(Ephys,aes(x=Rin))+geom_bar()
p3 <- ggplot(Ephys,aes(x=Cm,y=Rin))+geom_point()
library(gridExtra)
grid.arrange(p1,p2,p3)


#保存图片
ggsave(file="mygraph.png", plot=p1, width=5, height=4)

#error bar
#法1
d <- aggregate(Morpho[,23:34],by=list(MClust_new),mean)
t <- aggregate(Morpho[,23:34],by=list(MClust_new),sd)
rownames(d) <- d$Group.1
rownames(t) <- t$Group.1
d <- t(d[2:13])
try <- melt(d)
t2 <- melt(t(t[2:13]))
colnames(try) <- c("parameters","MClust_new","mean")
try$sd <- t2$value
try$tmin <- try$mean-try$sd
try$tmax <- try$mean+try$sd
try2 <- try[try$parameters=="S_Cell.Body.Volume",]
#法二
tmean <- apply(Morpho[,23:34],2,mean)
tsd <- apply(Morpho[,23:34],2,sd)
test <- data.frame(tmean,tmean-tsd,tmean+tsd)


#带误差棒散点图作图
p <-ggplot(Morpho,aes(x=MClust_new,y=S_Cell.Body.Volume,group=MClust_new))+geom_point(aes(color=MClust_new))
p
p <-ggplot(Morpho,aes(x=MClust_new,y=S_Cell.Body.Volume,group=MClust_new))+
  geom_jitter(aes(color=MClust_new),shape=16,size=1.5,fill="black",position=position_jitter(width=0.2))+
  geom_errorbar(data = try2,mapping = aes(x=MClust_new,y=mean,ymin=tmin,ymax=tmax,color=MClust_new),width=0.2)
p
#带误差棒柱状图
p <- ggplot(try2,group=MClust_new)+
  geom_bar(aes(x=MClust_new,y=mean,fill=MClust_new),stat = "identity")+
  geom_errorbar(aes(x=MClust_new,ymin=tmin,ymax=tmax,color=MClust_new),width=0.5)
p

#定义函数
sec <- function(x){
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  sem <- sd(x)/(sqrt(length(x)))
  return(c(n=n,mean=mean,sd=sd,sem=sem))
}

t <- apply(Morpho[,23:34],2,sec)
t <- sapply(Morpho[,23:34],sec)
t <- aggregate(Morpho[,23:34],list(MClust_new),FUN="mean")
library(dplyr)


