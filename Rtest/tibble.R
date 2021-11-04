library(tidyverse)
#tibble----
as_tibble(iris)
tibble(x=1:5,y=1,z=x^2+y)
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5)
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)
nycflights13::flights %>% 
  print(n=10,width = Inf)
df <- tibble(x=runif(5),y=rnorm(5))
#read_csv ----
read_csv("a,b,c
         1,2,3
         4,5,6")
read_csv("1,2,3\n4,5,6",col_names = FALSE)
read_csv("1,2,3\n4,5,6",col_names = c("x","y","z"))
read_csv("a,b,c\n1,2,.",na=".")
#数值----
parse_double("1.23")
parse_double("1,23",locale=locale(decimal_mark=","))
#适用于美国
parse_number("$123,456,789")
#适用于多数欧洲国家
parse_number("123.456.789",locale = locale(grouping_mark="."))
#适用于瑞士
parse_number("123'456'789",locale = locale(grouping_mark="'"))
#因子----
fruit <- c("apple","banana","grape")
parse_factor(c("apple","banana","grape"),levels=fruit)
#dplyr----
library(tidyverse)
library(nycflights13)
flights2 <- flights %>% 
  select(year:day,hour,origin,dest,tailnum,carrier)
#*连接----
flights2 %>% select(-origin,-dest) %>% 
  left_join(airlines,by="carrier")
flights2 %>% select(-origin,-dest) %>% 
  mutate(name=airlines$name[match(carrier,airlines$carrier)])
flights2 %>% 
  left_join(planes,by="tailnum")
flights2 %>% 
  left_join(airports,c("dest"="faa")) #命名字符向量by = c("a" = "b")。这种方式会匹配x 表中的a 变量和y 表中的b 变量。输出结果中使用的是x 表中的变量
top_dest <- flights %>% 
  count(dest,sort=TRUE) %>% head(10)
flights %>% filter(dest %in% top_dest$dest)
flights %>% semi_join(top_dest)
flights %>% anti_join(planes,by="tailnum") %>% 
  count(tailnum,sort=TRUE)
#*集合操作----
#intersect(x, y)返回既在x 表，又在y 表中的观测。
#union(x, y)返回x 表或y 表中的唯一观测。
#setdiff(x, y)返回在x 表，但不在y 表中的观测。




