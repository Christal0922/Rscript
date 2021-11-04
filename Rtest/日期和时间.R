library(tidyverse)
library(lubridate)
library(nycflights13)
#通过字符串创建----
today()
now()
ymd("2017-01-31")
mdy("January 31st,2017")
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 09:01")
#添加一个时区
ymd(20170131,tz="UTC")
#通过各个成分创建make_datetime----
flights %>% 
  select(year,month,day,hour,minute) %>% 
  mutate(
    departure=make_datetime(year,month,day,hour,minute)
  )

make_datetime_100 <- function(year,month,day,time){
  make_datetime(year,month,day,time %/% 100,time %% 100)
}
flights_dt <- flights %>% 
  filter(!is.na(dep_time),!is.na(arr_time)) %>% 
  mutate(
    dep_time=make_datetime_100(year,month,day,dep_time),
    arr_time=make_datetime_100(year,month,day,arr_time),
    sched_dep_time=make_datetime_100(year,month,day,sched_dep_time),
    sched_arr_time=make_datetime_100(year,month,day,sched_arr_time)) %>% 
  select(origin,dest,ends_with("delay"),ends_with("time"))
flights_dt %>% ggplot(aes(dep_time))+
  geom_freqpoly(binwidth=86400) #86400秒=1天,一年间出发时间的可视化分布
flights_dt %>% 
  filter(dep_time<ymd(20130102)) %>% 
  ggplot(aes(dep_time))+
  geom_freqpoly(binwidth=600) #600秒=10min
#通过其他类型数据创建，as_datetime(),as_date()----
as_datetime(today())
as_date(now())
# 有时我们会使用“Unix 时间戳”（即1970-01-01）的偏移量来表示日期时间。如果偏移
# 量单位是秒，那么就使用as_datetime() 函数来转换；如果偏移量单位是天，则使用as_date() 函数来转换：
as_datetime(60*60*10)
as_date(365*10+2)
#日期时间成分----
# 如果想要提取出日期中的独立成分，可以使用以下访问器函数：year()、month()、mday()
# （一个月中的第几天）、yday()（一年中的第几天）、wday()（一周中的第几天）、hour()、
# minute() 和second()：
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
mday(datetime)
yday(datetime)
# 对于month() 和wday() 函数，你可以设置label = TRUE 来返回月份名称和星期数的缩写，
# 还可以设置abbr = FALSE 来返回全名：
month(datetime,label=TRUE)
wday(datetime,label=TRUE,abbr = FALSE)
#wday() 函数，我们可以知道在工作日出发的航班要多于周末出发的航班
flights_dt %>% 
  mutate(wday=wday(dep_time,label = TRUE)) %>% 
  ggplot(aes(x=wday))+geom_bar()+
  scale_x_discrete(labels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))
# 如果查看一小时内每分钟的平均出发延误，我们可以发现一个有趣的模式。似乎在第
# 20~30 分钟和第50~60 分钟内出发的航班的延误时间远远低于其他时间出发的航班！
flights_dt %>% 
  mutate(minute=minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarize(avg_delay=mean(arr_delay,na.rm=TRUE),n=n()) %>% 
  ggplot(aes(minute,avg_delay))+geom_line()
# 绘制独立日期成分的另一种方法是，通过floor_date()、round_date() 和ceiling_date()
# 函数将日期舍入到临近的一个时间单位
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()
#设置时间成分update----
year(datetime) <- 2020
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
#时间间隔-----
# Hadley多大了
h_age <- today()-ymd(19920922)
as.duration(h_age)
#时期
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)
#阶段
one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz = "America/New_York"
)
one_pm+days(1)
#我们使用阶段来解决与航班日期有关的一个怪现象。有些飞机似乎在从纽约市起飞前就到达了目的地
flights_dt %>% filter(arr_time<dep_time) %>% 
  select(origin,dest,arr_time,dep_time)
flights_dt2 <- flights_dt %>%
  mutate(
    overnight=arr_time<dep_time,
    arr_time=arr_time+days(overnight*1),
    sched_arr_time=sched_arr_time+days(overnight*1)
  )
#区间
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
(today() %--% next_year) %/% days(1) #要想知道一个区间内有多少个阶段，你需要使用整数除法
#时区
Sys.timezone()
#你还可以使用OlsonNames() 函数来查看完整的时区名称列表
length(OlsonNames())
head(OlsonNames())
x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York")
x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen")
x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland")
x1-x2
x4 <- c(x1, x2, x3)
#保持时间不变，修改其显示方式。当时间正确，但需要更直观的表示时，可以使用这种方法：
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
#修改内部时间。当时间数据被标注了错误的时区，而想要改正过来时，你就可以使用这种方法：
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
