df <- tibble::tibble(
  a=rnorm(10),
  b=rnorm(10),
  c=rnorm(10),
  d=rnorm(10)
)
rescale01 <- function(x){
  rng <- range(x,na.rm = TRUE)
  (x-rng[1])/(rng[2]-rng[1])
}
rescale01(c(0,5,10))
#if----
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
#|| &&
# 只要|| 遇到第一个TRUE，那么就会返回TRUE，不再计算其他表达式；只要&& 遇到第
# 一个FALSE，就会返回FALSE，不再计算其他表达式
dplyr::near(sqrt(2)^2,2)
identical(2*1,4-2)
#多重条件
if(this){
  #做一些操作
} else if (that){
  #做另外一些操作
} else {
  #操作
}
y <- 10
x <- if(y<20) "Too low" else "Too high"
#使用近似正态分布计算均值两端的置信区间
mean_ci <- function(x,conf=0.95){
  se <- sd(x)/sqrt(length(x))
  alpha <- 1-conf
  mean(x)+se*qnorm(c(alpha/2,1-alpha/2))
}
x <- runif(100)
mean_ci(x)
mean_ci(x,conf=0.99)

if(na.rm){
  miss <- is.na(x) | is.na(w)
  x <- x[!miss]
  w <- w[!miss]
} sum(w*x) / sum(x)

#...----
stringr::str_c("a","b","c","d","e","f")
commas <- function(...) stringr::str_c(...,collapse = ",")
commas(letters[1:10])
#return----
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  # 这里是复杂的代码
}

#缺失值
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}
mtcars %>%
  show_missings() %>%
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>%
  show_missings()





