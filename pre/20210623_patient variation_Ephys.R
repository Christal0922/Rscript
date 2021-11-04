library(openxlsx)
Ephys <- read.xlsx("E:/R/PCA/20210623_Ephys340.xlsx",sheet=1,rowNames=TRUE)
Val <- as.matrix(colnames(Ephys))
FCD <- Ephys[which(Ephys$pathology=="FCD2"),]
Val[12:24] <- c("Rheobase(pA)","RP(mV)","Rin(m¦¸)","Cm(pF)","Threshold(mV)",
                "Peak(mV)","Halfwidth(ms)","AHP(mV)","Latency(ms)","UDR",
                "Ins.freq(Hz)","Freq(Hz)","AI")
#¼ÆËã
mean <- apply(FCD[,12:24],2,mean)
sd <- apply(FCD[,12:24],2,sd)
linematrix <- data.frame(mean,sd)
linematrix$s3d <- 3*linematrix$sd
linematrix$p3d <- linematrix$mean+linematrix$s3d
linematrix$n3d <- linematrix$mean-linematrix$s3d

#×÷Í¼
library(ggplot2)
a <- sort(unique(Ephys$patient.id2))
a[1:8] <- c("2FCD","3FCD","4FCD","5FCD","6FCD",
            "7FCD","8FCD","9FCD")

pdf("E:/R/Morpho2/20210623_variation_Ephys.pdf",width = 4.5,height = 4)
for (i in 12:24){
  p <- ggplot(FCD,aes(x=patient.id2,y=FCD[,i]))+
    geom_point()+
    scale_x_discrete(labels=a)+
    labs(x="",y=Val[i])+
    geom_abline(intercept=linematrix[i-11,1],slope=0,linetype=1,color="blue")+
    geom_abline(intercept=linematrix[i-11,4],slope=0,linetype=2,color="red")+
    geom_abline(intercept=linematrix[i-11,5],slope=0,linetype=2,color="red")+
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

