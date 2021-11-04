library(tidyverse)
#NULL是一个与向量相关的对象，用于表示空向量，通常指长度为0的向量
#向量类型typeof----
typeof(letters)
typeof(1:10)
#向量长度length()----
x <- list("a","b",1:10)
length(x)
#R 中默认数值是双精度型的。如果想要创建整型数值，可以在数字后面加一个L
typeof(1)
typeof(1L)
#特殊值 NA、NaN、Inf、-Inf----
c(-1,0,1)/0
is.finite()
is.infinite()
is.na()
is.nan()
#缺失值----
NA #逻辑型
NA_integer_ #整型
NA_real_ #双精度型
NA_character_ #字符型
#强制转换----
as.logical()
as.integer()
as.double()
as.character()
#example----
x <- sample(20,100,replace = TRUE)
x <- c(10,3,NA,5,8,1,NA)
#x中的所有非缺失值
x[!is.na(x)]
#x中的所有偶数值
x[x %% 2==0]
#列表结果str()
x <- list(1,2,3)
str(x)
#一个典型的泛型函数
as.Date()
#methods()函数列举出一个泛型函数的所有方法
methods("as.Date")
#getS3method()函数查看方法的特殊实现形式
getS3method("as.Date","default")
#因子----
x <- factor(c("ab","cd","ab"),
            levels = c("ab","cd","ef"))
attributes(x)
































