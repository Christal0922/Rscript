library(tidyverse)
library(forcats)
x1 <- c("Dec", "Apr", "Jan", "Mar")
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1,levels = month_levels)
#因子的顺序与初始数据的顺序保持一致
f1 <- factor(x1,levels = unique(x1))
#gss_cat dataset----
gss_cat %>% count(race)
#ggplot2 会丢弃没有任何数据的那些水平，你可以使用以下代码来强制显示这些水平
ggplot(gss_cat,aes(race))+geom_bar()+
  scale_x_discrete(drop=FALSE)
#修改因子水平fct_recode()----
gss_cat %>% count(partyid)
gss_cat %>% 
  mutate(partyid=fct_recode(partyid,
                            "Repulican,strong"="Strong republican",
                            "Republican,weak"="Not str republican",
                            "Independent,near rep"="Ind,near rep",
                            "Independent,near dem"="Ind,near dem",
                            "Democart,weak"="Not str democrat",
                            "Democrat,strong"="Strong democrat")) %>% 
  count(partyid)
#你可以将多个原水平赋给同一个新水平，这样就可以合并原来的分类
gss_cat %>% 
  mutate(partyid=fct_recode(partyid,
                            "Repulican,strong"="Strong republican",
                            "Republican,weak"="Not str republican",
                            "Independent,near rep"="Ind,near rep",
                            "Independent,near dem"="Ind,near dem",
                            "Democart,weak"="Not str democrat",
                            "Democrat,strong"="Strong democrat",
                            "Other"="No answer",
                            "Other"="Don't know",
                            "Other"="Other party")) %>% 
  count(partyid)
#合并多个水平fct_collapse()----
gss_cat %>% 
  mutate(partyid=fct_collapse(partyid,
                              other=c("No answer","Don't know","Other party"),
                              rep=c("Strong republican","Not str republican"),
                              ind=c("Ind,near rep","Independent","Ind,near dem"),
                              dem=c("Not str democrat","Strong democrat")
          )) %>% 
  count(partyid)




