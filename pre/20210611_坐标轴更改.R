library(ggplot2)
windowsFonts(myFont = windowsFont("Arial"))   ##如果要更改字体，需要先“绑定”字体
dt = data.frame(A = 1:10, B = c(2,15,6,18,9,7,13,15,10,3), C = c('A','C','A','B','C','D','A','C','D','B'))
p = ggplot(dt, aes(x = A, y = B, color = C, group = factor(1))) + 
  geom_point(size = 3.8) +
  geom_line(size = 0.8) +
  geom_text(aes(label = B, vjust = 1.1, hjust = -0.5, angle = 45), show_guide = FALSE)


## face取值：plain普通，bold加粗，italic斜体，bold.italic斜体加粗
p + xlab("这是 X 轴") + ylab("这是 Y 轴") + ggtitle("这是标题")   ## 修改文字方法一
p + labs(x = "这是 X 轴", y = "这是 Y 轴", title = "这是标题")   ## 修改文字方法二

#### 修改 X 轴标签的大小、字体、颜色、加粗、位置、角度 ####
p + xlab("这是 X 轴") + theme(axis.title.x = element_text(size = 15, family = "myFont", color = "green", face = "bold", vjust = 0.5, hjust = 0.5, angle = 45))

## face取值：plain普通，bold加粗，italic斜体，bold.italic斜体加粗