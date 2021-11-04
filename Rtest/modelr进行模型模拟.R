library(tidyverse)
library(modelr)
options(na.action = na.warn)
#try----
ggplot(sim1,aes(x,y))+geom_point()
models <- tibble(
  a1=runif(250,-20,40),
  a2=runif(250,-5,5)
)
ggplot(sim1,aes(x,y))+
  geom_abline(aes(intercept=a1,slope=a2),
              data=models,alpha=1/4)+
  geom_point()
#工作原理----
#模型族转换为一个R 函数。这个函数将模型参数和数据作为输入，并使用模型预测值作为输出：
model1 <- function(a,data){
  a[1]+data$x*a[2]
}
model1(c(7,1.5),sim1)
#距离用均方根误差
#“均方根误差”。先计算实际值与预测值之间的差，对其取平方，然后求平均数，最后再计算出平方根
measure_distance <- function(mod,data){
  diff <- data$y-model1(mod,data)
  sqrt(mean(diff^2))
}
measure_distance(c(7,1.5),sim1)
sim1_dist <- function(a1,a2){
  measure_distance(c(a1,a2),sim1)
}
models <- models %>% 
  mutate(dist=purrr::map2_dbl(a1,a2,sim1_dist))
ggplot(sim1,aes(x,y))+
  geom_point(size=2,color="grey30")+
  geom_abline(
    aes(intercept=a1,slope=a2,color=-dist),
    data=filter(models,rank(dist)<=10)
  )
#grid
grid <- expand.grid(
  a1=seq(-5,20,length = 25),
  a2=seq(1,3,length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1,a2,sim1_dist))
grid %>% 
  ggplot(aes(a1,a2))+
  geom_point(
    data=filter(grid,rank(dist)<= 10),
    size=4,colour = "red")+
  geom_point(aes(color=-dist))
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )  
#牛顿—拉夫逊搜索----
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])
#lm()----
sim1_mod <- lm(y~x,data = sim1)
coef(sim1_mod)
#所谓的残差，它是数据去除预测值后剩余的部分。残差是非常强大的，
#因为它允许我们使用模型去除数据中显著的模式，以便对剩余的微妙趋势进行研究
#预测
grid <- sim1 %>% 
  data_grid(x)
grid <- grid %>% 
  add_predictions(sim1_mod)
ggplot(sim1,aes(x))+
  geom_point(aes(y=y))+
  geom_line(aes(y=pred),data=grid,color="red",size=1)
#残差
sim1 <- sim1 %>% add_residuals(sim1_mod)
#观测模型的质量
ggplot(sim1,aes(resid))+
  geom_freqpoly(binwidth=0.5)
ggplot(sim1,aes(x,resid))+
  geom_ref_line(h=0)+geom_point()
#公式和模型族----
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df, y ~ x1)
# R 向模型加入截距项的方法是，加入一个值全是1 的列。默认情况下，R 总是加入这一列。
# 如果不想要截距项，那么你必须使用-1 来明确丢弃它：
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)
ggplot(sim2) +
  geom_point(aes(x, y))
mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
#带有分类变量x 的模型会为每个分类预测出均值。（为什么？因为均值会使均方根距离最小化
#交互项----
ggplot(sim3,aes(x1,y))+geom_point(aes(color=x2))
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
grid <- sim3 %>% 
  data_grid(x1,x2) %>% 
  gather_predictions(mod1,mod2)
ggplot(sim3,aes(x1,y,color=x2))+
  geom_point()+
  geom_line(data=grid,aes(y=pred))+
  facet_wrap(~model)
sim3 <- sim3 %>% 
  gather_residuals(mod1,mod2)
ggplot(sim3,aes(x1,resid,color=x2))+
  geom_point()+facet_grid(model~x2)
#交互项（两个连续变量）
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid
#model_matrix()查看lm()到底在拟合哪个方程
df <- tribble(
  ~y,~x,
  1,1,
  2,2,
  3,3
)
model_matrix(df,y~x^2+x)
model_matrix(df,y~I(x^2)+x)
model_matrix(df, y ~ poly(x, 2)) #y = a_1 + a_2 * x + a_3 * x^2
library(splines)
model_matrix(df, y ~ ns(x, 2)) #y = a_1 + a_2 * x + a_3 * x^2
#非线性函数sin(x)----
sim5 <- tibble(
  x=seq(0,3.5*pi,length=50),
  y=4*sin(x)+rnorm(length(x))
)
ggplot(sim5,aes(x,y))+geom_point()
library(splines)
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)
grid <- sim5 %>% 
  data_grid(x=seq_range(x,n=50,expand=0.1)) %>% 
  gather_predictions(mod1,mod2,mod3,mod4,mod5,.pred="y")
ggplot(sim5,aes(x,y))+
  geom_point()+
  geom_line(data=grid,color="red")+
  facet_wrap(~model)
#缺失值----
df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)
mod <- lm(y ~ x, data = df)
mod <- lm(y ~ x, data = df, na.action = na.exclude)
#通过nobs() 函数，你可以知道模型实际使用了多少个观测
nobs(mod)






