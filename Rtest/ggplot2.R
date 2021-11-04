library(tidyverse)
#加图片标题----
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_smooth(se=FALSE)+
  labs(title = paste("Fuel efficiency generally decreases with","engine size"),
       subtitle ="Two seaters (sports cars) are an exception because of their light weight",
       caption = "Data from fueleconomy.gov",
       x="Engine displacement (L)",
       y="Highway fuel economy",
       colour="Car type"
  )
#标记数据点----
best_in_class <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy))==1)
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_text(aes(label=model),data = best_in_class,
            nudge_y = 2,alpha=0.5) 
#防止数据标签重叠----
ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_point(size=3,shape=1,data = best_in_class)+
  ggrepel::geom_label_repel(
    aes(label=model),data=best_in_class
  )
#计算各组中位数并显示----
class_avg <- mpg %>% 
  group_by(class) %>% 
  summarize(
    displ=median(displ),
    hwy=median(hwy)
  )
ggplot(mpg,aes(displ,hwy,color=class))+
  ggrepel::geom_label_repel(
    aes(label=class),data=class_avg,
    size=6,label.size = 0,segment.color=NA
  )+
  geom_point()+
  theme(legend.position = "none")
#右上角做注释----
label <- mpg %>% 
  summarize(
    displ=max(displ),
    hwy=max(hwy),
    label=paste("Increasing engine size is \nrelated to decreasing fuel economy")
  )
#label也可以用Inf,使标签紧贴图形的边界
label <- tibble(
  displ=Inf,
  hwy=Inf,
  label=paste("Increasing engine size is \nrelated to decreasing fuel economy")
)
ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  geom_text(aes(label=label),
            data=label,vjust="top",hjust="right")
#我们使用"\n" 手动为标签文本换行。另一种方法是使用stringr::str_
#wrap() 函数来自动换行，此时需要给出每行的字符数
"Increasing engine size related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()
#坐标轴刻度----
#breaks 和labels。breaks 控制坐标轴刻度的位置，以及与图例项目相关的数值显示。
#labels 控制与每个坐标轴刻度或图例项目相关的文本标签。breaks 的最常见用途是替换默认的刻度
ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  scale_y_continuous(breaks = seq(15,40,by=5))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

presidential2 <- presidential %>% 
  mutate(id=33+row_number())
ggplot(presidential2,aes(start,id))+
  geom_point()+
  geom_segment(aes(xend=end,yend=id))+
  scale_x_date(NULL,breaks = presidential2$start,date_labels = "'%Y")
#图例布局----
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))
base+theme(legend.position = "bottom")+
  guides(color=guide_legend(
    nrow=1,
    override.aes = list(size=4)
  ))
#标度替换，x取log10----
ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()
#颜色替换----
#scale_color_brewer
base <- ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=drv))
base+scale_color_brewer(palette = "Set1")
#palette="Set1"可用其他的值，"Set2"、"Set3"、"Pastel1"、"Pastel2"、"Paired"、"Dark2"或"Accent"
base+scale_color_manual(values = brewer.pal(9,"Set1")[c(2,3,4)])

library(RColorBrewer)
display.brewer.all()
display.brewer.all(type = "seq") #"seq"连续型，"div"极端型，"qual"离散型，
barplot(rep(1,3),col=brewer.pal(6,"YlOrRd")) #前面的rep(1,3)中的3表示取几个颜色，后面的pal(6)表示将颜色分成6份
barplot(rep(1,6),col=brewer.pal(11,"RdGy")[2:7])# 第2至第7个颜色
a <- brewer.pal(9,"YlOrRd")[c(1,3,5)]
barplot(rep(1,9),col=brewer.pal(9,"Set1"))
barplot(rep(1,9),col=brewer.pal(9,"Set1")[c(2,3,9)]) #将颜色分成9份，取第2,3,9个颜色
#scale_color_manual
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(
    values = c(Republican = "red", Democratic = "blue")
  )
#scale_color_viridis
df <- tibble(
  x=rnorm(10000),
  y=rnorm(10000)
)
base <- ggplot(df,aes(x,y))+
  geom_hex()+
  coord_fixed() #y轴和x轴的比例
base+viridis::scale_fill_viridis()




#缩放----
#coord_cartesian() 函数中设置xlim 和ylim 参数值
#显示特定范围的图
base <- ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+geom_smooth()
base+coord_cartesian(xlim=c(5,7),ylim=c(10,30))
#筛选数据
mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>% 
  ggplot(aes(displ,hwy))+geom_point()+
  geom_smooth()
#两组数据统一刻度
suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))
ggplot(suv,aes(displ,hwy,color=drv))+geom_point()+
  x_scale+y_scale+col_scale
ggplot(compact,aes(displ,hwy,color=drv))+geom_point()+
  x_scale+y_scale+col_scale

mpg %>% filter(class == c("suv","compact")) %>% 
  ggplot(aes(displ,hwy))+geom_point(aes(color=class))+
  facet_wrap(~class)
#主题----
base <- ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+geom_smooth(se=FALSE)
#常见8种主题
base+theme_bw() #带网格线的白色背景
base+theme_classic() #经典主题，只有坐标轴，没有网格线
base+theme_dark() #暗色背景，用于对比
base+theme_gray() #灰色背景，默认主题
base+theme_light() #浅色坐标轴和网格线
base+theme_linedraw() #只有黑色网格线
base+theme_minimal() #极简主题，无背景
base+theme_void() #空白主题，只显示几何对象
#保存图形----

#设置保存地址
getwd()
setwd("E:/Rtest/Rtest/output/")
ggsave("my_plot.pdf")
#R markdown
#图形大小，fig.width,fig.height,fig.asp,out.width,out.height
#默认，fig.width=6 (6英寸), fig.asp=0.618
#out.width 控制输出图形的大小，并将其设置为行宽的百分比。默认设置为out.width = "70%" 和fig.align = "center"











