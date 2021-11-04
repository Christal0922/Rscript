a <- read.table("E:/R/PCA/20201210_89cells.txt",row.names=1,head=TRUE,sep="\t")
b <- lm(Rheobase~Rin+RP+AI,data=a)
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
} #计算相对权重函数
relweights(b,col="blue")
Ephysvar <- c("Rheobase","RP","Rin","Cm","Thres","Peak","hw","Ahp","latency","UDR","Ins.Freq","Freq","AI")
colnames(a)[12:21] <- c("termination","s.area","contraction","b.order","s.volume","angle","d.quantity","d.nodes","d.ends","d.length")
Morphovars <- c("termination","s.area","contraction","b.order","s.volume","angle","d.quantity","d.nodes","d.ends","d.length")
b <- lm(Rheobase~Rin+RP+AI,data=a)