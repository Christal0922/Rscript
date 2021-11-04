kegg_result2 <- kegg_result
kegg_result2$gene <- kegg_result2$geneID
kegg_result2$gene <- strsplit(kegg_result2$gene,"/",fixed = TRUE)

tird2 <- na.omit(trid)
tird2$ENTREZID <- as.character(tird2$ENTREZID)

for (k in 1:(length(kegg_result2$gene))){
  d <- kegg_result2$Count[k]
  for (i in 1:d){
    kegg_result2$gene[[k]][i] <- tird2[tird2$ENTREZID==kegg_result2$gene[[k]][i],1]
    i=i+1
  }
  k=k+1
}

kegg_result3 <- kegg_result2

kegg_result3$gene3 <- NA
for (k in 1:(length(kegg_result3$gene))){
  d <- kegg_result2$Count[k]
  kegg_result3$gene3[k] <- kegg_result3$gene[[k]][1]
  for (i in 2:d){
    kegg_result3$gene3[k] <- paste(kegg_result3$gene3[k],kegg_result3$gene[[k]][i],sep = "/")
    i=i+1
  }
  k=k+1
}
kegg_result3 <- kegg_result3[,c(-8,-10)]
write.table(kegg_result3,"E:/R/patch/20210120_kegg3.txt",row.names = F,sep = '\t')
write.table(tird2,"E:/R/patch/20210120_tird2.txt",row.names = F,sep = '\t')

p1 <- heatplot(kegg) 
p1d <- p1$data
p1d$Gene2 <- NA
p1d$Gene2 <- tird2[p1d$Gene%in%tird2$ENTREZID,1]
p1$data$Gene <- tird2[p1d$Gene%in%tird2$ENTREZID,1]
p1

