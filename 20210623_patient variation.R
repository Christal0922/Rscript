library(openxlsx)
Morpho <- read.xlsx("E:/R/PCA/20210623_morpho89.xlsx",sheet=1,rowNames=TRUE)
Val <- as.matrix(colnames(Morpho))

#¼ÆËã
mean <- apply(Morpho[,23:33],2,mean)
sd <- apply(Morpho[,23:33],2,sd)
linematrix <- data.frame(mean,sd)
linematrix$s3d <- 3*linematrix$sd
linematrix$p3d <- linematrix$mean+linematrix$s3d
linematrix$n3d <- linematrix$mean-linematrix$s3d

#×÷Í¼
library(ggplot2)
a <- sort(unique(Morpho$patient.id2))
a[1:3] <- c("1FCD","3FCD","4FCD")
Val[23:33] <- c("Soma Surface Area(¦Ìm2)","Maximum Branch Order",
                "Contraction","Soma Volume(¦Ìm3)",
                "Local Angle(¡ã)","Max Local Angle",
                "Dendrite Quantity","Dendrite Nodes",
                "Dendrite Ends","Dendrite Total Length(¦Ìm)",
                "Tree Termination Distance(¦Ìm)")


pdf("E:/R/Morpho2/20210623_variation_morpho.pdf",width = 4.5,height = 4)
for (i in 23:33){
  p <- ggplot(Morpho,aes(x=patient.id2,y=Morpho[,i]))+
    geom_point()+
    scale_x_discrete(labels=a)+
    labs(x="",y=Val[i])+
    geom_abline(intercept=linematrix[i-22,1],slope=0,linetype=1,color="blue")+
    geom_abline(intercept=linematrix[i-22,4],slope=0,linetype=2,color="red")+
    geom_abline(intercept=linematrix[i-22,5],slope=0,linetype=2,color="red")+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size=12,angle = 90,hjust = 1,color="black"),
          axis.text.y = element_text(size=12,vjust = 1,color = "black"),
          axis.title = element_text(size = 20),
          axis.line = element_line(),
          panel.background = element_blank())
  print (p)
  i=i+1
}
dev.off()

#²âÊÔ
for (i in 23:24){
  i=24
  p <- ggplot(Morpho,aes(x=patient.id2,y=Morpho[,i]))+
    geom_point()+
    geom_abline(intercept=linematrix[i-22,1],slope=0,linetype=1,color="blue")+
    geom_abline(intercept=linematrix[i-22,4],slope=0,linetype=2,color="red")+
    geom_abline(intercept=linematrix[i-22,5],slope=0,linetype=2,color="red")
  p
  i=i+1
}





