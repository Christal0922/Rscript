library(nycflights13)
library(tidyverse)
a <- nycflights13::flights
#使用filter筛选行-----
filter(flights,month==1,day==1)
filter(flights,month==1 | month==12)
#浮点数，计算机进行的是有限精度计算
near(sqrt(2)^2,2)
near(1/49*49,1)
#x %in% y 选取出x是y中的一个值时的所有行
nov <- dec <- filter(flights,month %in% c(11,12))
#可以使用德摩根定律将复杂的筛选条件进行简化：!(x & y) 等价于!x | !y、!(x |y) 等价于!x & !y
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)
#arrange()排列行----
arrange(flights,year,month,day)
arrange(flights,desc(arr_delay)) #降序排序
#select()选择列----
select(flights,year,month,day)
select(flights,year:day) #选择year和day之间的所有列
select(flights,-(year:day)) #选择不在year和day之间的所有列
rename(flights,tail_num=tailnum)
select(flights,time_hour,air_time,everything()) #将两个变量移至最前
#使用mutate()添加新变量----
flight_sml <- select(flights,year:day,ends_with("delay"),distance,air_time)
mutate(flight_sml,gain=arr_delay-dep_delay,speed=distance/air_time*60)
mutate(flight_sml,gain=arr_delay-dep_delay,hours=air_time/60,gain_per_hour=gain/hours)
#只想保留新变量，可以使用transmute()函数
transmute(flights,gain=arr_delay-dep_delay,hours=air_time/60,gain_per_hour=gain/hours)
#%/%（整数除法）和%%（求余）满足x == y * (x %/% y) + (x %% y)
#summarize()进行分组摘要----
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))
#使用管道组合多种操作----
by_dest <- group_by(flights,dest)
delay <- summarize(by_dest,count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=TRUE))
delay <- filter(delay,count>20,dest!="HNL")
#%>%可以理解为然后
delays <- flights %>% group_by(dest) %>%
  summarize(count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=TRUE)) %>% 
  filter(count>20,dest!="HNL")

