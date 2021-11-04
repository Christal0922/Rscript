library(magrittr)
diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat=price/carat)
# 通过管道方式使用assign() 函数是无效的，因为这时赋值操作是在由%>% 建立的一个临
# 时环境中进行的。如果要通过管道方式来使用assign()，就必须显式地指定环境
env <- environment()
"x" %>% assign(100,envir = env)
rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
# “T”管道操作符%T>%----
# 它的用法和%>% 差不多，只是
# 它返回的是左侧项而不是右侧项。之所以称它为“T”操作符，是因为它起的作用类似于T 形三通管道：
rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()
#爆炸操作符%$%----
# 如果使用的函数不是基于数据框的（也就是说，你必须传给这些函数一个独立的向量，
# 不能传给它们数据框或基于数据框求值的表达式），那么你就会发现爆炸操作符%$% 的妙处。
mtcars %$% cor(disp,mpg)
#赋值操作%<>%----
mtcars2 <- mtcars %>% transform(cyl=cyl*2)
mtcars %<>% transform(cyl=cyl*2)






