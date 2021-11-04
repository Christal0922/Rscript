library(tidyverse)
#for----
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
output <- vector("double",ncol(df))
for(i in seq_along(df)){
  output[[i]] <- median(df[[i]])
} #seq_along 等价于1:length(df)

#修改现有对象
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
#未知的输出长度
means <- c(0,1,2)
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))
#未知序列长度while----
while(condition){
  #循环体
}
for (i in seq_along()){
  #循环体
}
#等价于
i <- 1
while(i <= length(x)){
  #循环体
  i <- i+1
}
#使用while 循环找出了连续3 次掷出正面向上的硬币所需的投掷次数
flip <- function() sample(c("T","H"),1)
flips <- 0
nheads <- 0
while(nheads<3){
  if(flip()=="H"){
    nheads <- nhead+1
  } else {
    nheads <- 0
  }
  flips <- flips+1
}
flips
#函数式编程----
col_means <- function(df){
  output <- vector("double",length(df))
  for (i in seq_along(df)){
    output[i] <- mean(df[[1]])
  }
  output
}
col_summary <- function(df,fun){
  out <- vector("double",length(df))
  for (i in seq_along(df)){
    out[i] <- fun(df[[i]])
  }
}
col_summary(df,median)
#计算均值、sd、sem等----
stat <- function(df){
  out <- matrix(NA, nrow = 4, ncol = length(df), byrow = TRUE,
                dimnames = list(c("mean","sd","n","sem"),
                                c("a", "b", "c","d")))
  for (i in seq_along(df)){
    out[1,] <- mean(df[[i]])
    out[2,] <- sd(df[[i]])
    out[3,] <- length(df[[i]])
    out[4,] <- sd(df[[i]])/sqrt(length(df[[i]]))
  }
  out
}
#映射函数map()----
map_dbl(df,mean) #输出双精度型向量
df %>% map_dbl(mean)
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt,data = df))
models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))
models %>% map(summary) %>% map_dbl(~.$r.squared)
models %>% map(summary) %>% map_dbl("r.squared")
#lapply(),sapply()----
#sapply() 函数是对lapply() 的包装，可以自动简化输出
x1 <- list(rnorm(5),rnorm(5),rnorm(5))
x2 <- list(runif(5),runif(5),runif(5))
threshold <- function(x,cutoff=0.8) x[x>0.8]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()
#多参数映射----
mu <- list(5,10,-3)
sigma <- list(1,5,10)
seq_along(mu) %>% map(~rnorm(5,mu[[.]],sigma[[.]])) %>% 
  str() #设置随机数的均值和sd
map2(mu,sigma,rnorm,n=5) %>% str()
n <- list(1, 3, 5)
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>%
  pmap(rnorm) %>%
  str()
params <- tribble(
  ~mean, ~sd, ~n,
  5, 1, 1,
  10, 5, 3,
  -3, 10, 5
)
params %>%
  pmap(rnorm)
#调用不同函数invoke_map()----
f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)
invoke_map(f, param, n = 5) %>% str()
sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>%
  mutate(sim = invoke_map(f, params, n = 10))
# 游走函数walk()----
library(ggplot2)
plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")
pwalk(list(paths, plots), ggsave, path = tempdir())
#预测函数keep(),discard()----
#keep() 和discard() 函数可以分别保留输入中预测值为TRUE 和FALSE 的元素
iris %>% keep(is.factor) %>% str()
iris %>% discard(is.factor) %>% str()
#some() 和every() 函数分别用来确定预测值是否对某个元素为真以及是否对所有元素为真
x <- list(1:5,letters,list(10))
x %>% some(is_character)
x %>% every(is_vector)
#detect() 函数可以找出预测值为真的第一个元素，detect_index() 函数则可以返回这个元素的位置：
x <- sample(10)
x %>%
  detect(~ . > 5)
#head_while() 和tail_while() 分别从向量的开头和结尾找出预测值为真的元素：
x %>%
  head_while(~ . > 5)
#归纳与统计----
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs %>% reduce(full_join)








