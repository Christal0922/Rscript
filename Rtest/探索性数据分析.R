library(ggplot2)
library(tidyverse)
view(diamonds)
#条形图----
ggplot(diamonds)+geom_bar(aes(x=cut))
diamonds %>% count(cut)
#直方图----
ggplot(diamonds)+geom_histogram(aes(x=carat),binwidth = 0.5)
diamonds %>% count(cut_width(carat,0.5))
smaller <- diamonds %>% filter(carat<3)
ggplot(smaller,aes(x=carat))+geom_histogram(binwidth=0.1)
ggplot(smaller,aes(x=carat,color=cut))+geom_freqpoly(binwidth=0.1) #按照cut叠加多个折线图
ggplot(smaller,aes(x=carat))+geom_histogram(binwidth=0.01)
ggplot(diamonds,aes(x=price))+geom_freqpoly(aes(color=cut),binwidth=500)
#异常值----
ggplot(diamonds)+geom_histogram(aes(x=y),binwidth=0.5)+
  coord_cartesian(ylim=c(0,50))
unusual <- diamonds %>% filter(y<3 | y>20) %>% arrange(y)
#NA----
diamonds2 <- diamonds %>% 
  mutate(y=ifelse(y<3|y>20,NA,y))
diamonds2[is.na(diamonds2$y),] 
ggplot(diamonds,aes(x=x,y=y))+geom_point(na.rm = TRUE)
#直方图，比例，密度----
ggplot(diamonds,aes(x=price,y=..density..))+
  geom_freqpoly(aes(color=cut),binwidth=500)
ggplot(diamonds,aes(x=cut,y=..prop..,group=1))+
  geom_bar()
#箱线图----
ggplot(mpg,aes(x=class,y=hwy))+geom_boxplot()
#基于hwy的值的中位数对class进行重新排序
ggplot(mpg,aes(x=reorder(class,hwy,FUN=median),y=hwy))+geom_boxplot()
ggplot(mpg,aes(x=reorder(class,hwy,FUN=median),y=hwy))+geom_boxplot()+coord_flip()
#两个分类变量----
ggplot(diamonds,aes(x=cut,y=color))+geom_count()
diamonds %>% count(color,cut)
#geom_tile()
diamonds %>% count(color,cut) %>% 
  ggplot(aes(x=color,y=cut))+
  geom_tile(aes(fill=n))
#两个连续变量----
ggplot(diamonds,aes(x=carat,y=price))+
  geom_point(alpha=1/100)
#分箱，geom_histogram(),geom_freqpoly(),geom_bin2d(),geom_hex()
ggplot(smaller,aes(x=carat,y=price))+
  geom_bin2d()
ggplot(smaller,aes(x=carat,y=price))+
  geom_hex()
ggplot(smaller,aes(x=carat,y=price))+
  geom_boxplot(aes(group=cut_width(carat,0.3)))
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()
#模型----
mod <- lm(log(price)~log(carat),diamonds)
library(modelr)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid=exp(resid))
ggplot(diamonds2,aes(x=cut,y=resid))+geom_boxplot()
diamonds %>% count(cut,clarity) %>% 
  ggplot(aes(clarity,cut,fill=n))+geom_tile()





