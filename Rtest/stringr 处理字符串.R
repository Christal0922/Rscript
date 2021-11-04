library(tidyverse)
library(stringr)
#字符串长度----
str_length(c("a","R for data science",NA))
#字符串组合----
str_c("x","y")
str_c("x","y",sep=",")
str_c("prefix-", c("a", "b", "c"), "-suffix")
#> [1] "prefix-a-suffix" "prefix-b-suffix" "prefix-c-suffix"

#要想将字符向量合并为字符串，可以使用collapse() 函数：
str_c(c("x", "y", "z"), collapse = ", ")
#> [1] "x, y, z"

#字符串取子集str_sub ----
x <- c("Apple","Banana","Pear")
str_sub(x,1,3) #提取字符串的一部分
str_sub(x,-3,-1) #复数表示从后往前数
str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1)) #改变大小写
#基础匹配str_view ----
str_view(x,"an")
str_view(x,".a.") #.指代任意值
str_view(c("a","a.c","bef"),"a\\.c") #查找.
str_view("a\\b","\\\\") #查找\
# ^ 从字符串开头进行匹配。
# $ 从字符串末尾进行匹配。
str_view(x,"^a")
str_view(x,"a$")
# \d 可以匹配任意数字
# \s 可以匹配任意空白字符（如空格、制表符和换行符）。
# [abc] 可以匹配a、b 或c。
# [^abc] 可以匹配除a、b、c 外的任意字符。
str_view(c("grey", "gray"), "gr(e|a)y")
# ?：0 次或1 次。
# +：1 次或多次。
# *：0 次或多次。
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x,"CC+")
str_view(x,"C[LX]+")
# {n}：匹配n 次。
# {n,}：匹配n 次或更多次。
# {,m}：最多匹配m 次。
# {n, m}：匹配n 到m 次。
str_view(x,"C{2}")
str_view(x,"C{2,}")
str_view(x,"C{2,3}")
#分组与回溯引用\1,\2
fruit <- c("banana","coconut","cucumber","jujube",
           "papaya","salal berry")
str_view(fruit,"(..)\\1",match = TRUE)
#匹配检测，str_detect()----
x <- c("apple","banana","pear")
str_detect(x,"e")
#有多少个以t开头的常用单词
sum(str_detect(words,"^t"))
#以元音字母结尾的常用单词的比例是多少
mean(str_detect(words,"[aeiou]$"))
# 找出至少包含一个元音字母的所有单词，然后取反
no_vowel <- !str_detect(words,"[aeiou]")
words[str_detect(words,"x$")]
str_subset(words,"x$")
df <- tibble(word=words,i=seq_along(word))
df %>% filter(str_detect(words,"x$"))
str_count(x,"a")
df %>% 
  mutate(
    vowels=str_count(word,"[aeiou]"),
    consonants=str_count(word,"[^aeiou]")
  )
#提取匹配内容str_extract----
stringr:sentences
length(sentences)
head(sentences)
colors <- c("red","orange","yellow","green","blue","purple")
color_match <- str_c(colors,collapse = "|")
has_color <- str_subset(sentences,color_match)
#str_extract()只提取第一个匹配
matches <- str_extract(has_color,color_match)
#选取具有多于一种匹配的所有 句子
more <- sentences[str_count(sentences,color_match)>1]
str_view_all(more,color_match)
str_extract(more,color_match)
str_extract_all(more,color_match)
# 如果设置了simplify = TRUE，那么str_extract_all() 会返回一个矩阵，其中较短的匹配会扩展到与最长的匹配具有同样的长度：
str_extract_all(more, color_match, simplify = TRUE)
x <- c("a","a b","a b c")
str_extract_all(x,"[a-z]",simplify = TRUE)
#分组匹配
noun <- "(a|the) ([^ ]+)" #至少有1 个非空格字符的字符序列
has_noun <- sentences %>% str_subset(noun) %>% head(10)
has_noun %>% str_extract(noun)
#str_extract() 函数可以给出完整匹配；str_match() 函数则可以给出每个独立分组。
has_noun %>% str_match(noun)
tibble(sentence=sentences) %>% 
  tidyr::extract(
    sentence,c("article","noun"),"(a|the) ([^ ]+)",
    remove=FALSE
  )
x <- "This is a sentence."
str_extract_all(x,boundary("word"))
#替换匹配内容str_replace(),str_replace()_all，----
x <- c("apple","pear","banana")
str_replace(x,"[aeiou]","-")
str_replace_all(x,"[aeiou]","-")
x <- c("1 house","2 cars","3 people")
str_replace_all(x,c("1"="one","2"="two","3"="three"))
# 除了使用固定字符串替换匹配内容，你还可以使用回溯引用来插入匹配中的分组。在下面
# 的代码中，我们交换了第二个单词和第三个单词的顺序：
sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)","\\1 \\3 \\2") %>% 
  head(5)
#拆分str_split()----
sentences %>% head(5) %>% str_split(" ")
sentences %>% head(5) %>% str_split(" ",simplify = TRUE)
"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]
#设定拆分片段的最大数量
fields <- c("Name: Hadley","Country: NZ","Age: 35")
fields %>% str_split(": ",n=2,simplify = TRUE)
x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))
str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]
#其他模式regex()----
str_view(fruit,"nana")
str_view(fruit,regex("nana"))
#ignore_case = TRUE 既可以匹配大写字母，也可以匹配小写字母，它总是使用当前的区域设置：
bananas <- c("banana","Banana","BANANA")
str_view(bananas,"banana")
str_view(bananas,regex("banana",ignore_case = TRUE))




