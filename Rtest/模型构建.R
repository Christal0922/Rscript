library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
#diamonds 对重量和价格进行对数转换----
diamonds2 <- diamonds %>% 
  filter(carat<=2.5) %>% 
  mutate(lprice=log2(price),lcarat=log2(carat))
ggplot(diamonds2,aes(lcarat,lprice))+geom_hex(bins=50)
mod_diamond <- lm(lprice ~ lcarat, data=diamonds2)
grid <- diamonds2 %>% 
  data_grid(carat=seq_range(carat,20)) %>% 
  mutate(lcarat=log2(carat)) %>% 
  add_predictions(mod_diamond,"lprice") %>% 
  mutate(price=2^lprice)
ggplot(diamonds2,aes(x=carat,y=price))+
  geom_hex(bins=50)+
  geom_line(data=grid,color="red",size=1)
#~查看残差----
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond,"lresid")
ggplot(diamonds2,aes(lcarat,lresid))+
  geom_hex(bins=50)







