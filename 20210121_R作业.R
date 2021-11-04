#1.根据R包org.Hs.eg.db找到下面ensembl 基因ID 对应的基因名(symbol)#
options(stringsAsFactors = F)
a <- read.table("example/ensembl.txt",header = F)
#去除点后面的内容，法一，stringr#
library(stringr)
a$ensembl_id <- str_split(a$V1,'[.]',simplify=T)[,1]
#法二，lapply#
a$ensembl_id2 <- unlist(lapply(a$V1,function(x){
  strsplit(x,'[.]')[[1]][1]
}))

library(org.Hs.eg.db)
g2s <- toTable(org.Hs.egSYMBOL)
g2e <- toTable(org.Hs.egENSEMBL)
b <- merge(a,g2e,by='ensembl_id',all.x=T)
d <- merge(b,g2s,by='gene_id',all.x=T)

# 2. 根据R包hgu133a.db找到下面探针对应的基因名(symbol)
a <- read.table("example/tanzhen.txt")
library(hgu133a.db)
ids <- toTable(hgu133aSYMBOL)
b <- merge(a,ids,by.x='V1',by.y='probe_id')

#install GenomicRangers
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GenomicRanges")

library(GenomicRanges)
gr2 <- GRanges(seq=c("chr1","chr2","chr2","chr2","chr3"),
               ranges = IRanges(start = 101:105,width=10),
               strand = c("+","+","+","+","+"))
Rle(c("chr1","chr2","chr3","chr1"),c(1,2,3,2))



