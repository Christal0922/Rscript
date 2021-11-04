require(data.table)
df <- data.table(group=rep(c(1,2),each=3),value=c(10,20,25,5,10,15))
setkey(df,group)
df[,diff:=c(NA,diff(value)),by=group]
setDF(df)

#·¨2
library(dplyr)
df %>%
  group_by(group) %>%
  mutate(Diff = c(NA, diff(value)))


