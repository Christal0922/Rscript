library(Seurat)
library(dyplyr)
#加载PBMC dataset#
pbmc.data <- Read10X(data.dir="E:/R/Seurat/pbmc")
#查看稀疏矩阵维度#
dim(pbmc.data)
#Examine the memory savings between regular and sparse matrices#
dense.size <- object.size(x=as.matrix(x=pbmc.data))
dense.size
sparse.size <- object.size(x=pbmc.data)
dense.size/sparse.size
#预览稀疏矩阵#
pbmc.data[1:10,1:6]
#在使用CreateSeuratObject()创建对象的同时，过滤数据质量差的细胞。保留在》=3个细胞中表达的基因；保留能检测到>=200个基因的细胞#
pbmc <- CreateSeuratObject(counts=pbmc.data,project="pbmc2700",min.cells = 3, min.features = 200)
#计算每个细胞的线粒体基因转录本数的百分比（%），使用[[]]操作符存放到metadata中#
pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc,pattern = "^MT-")
#作图#
VlnPlot(object = pbmc, features = c("nCount_RNA", "nFeature_RNA", "percent.mt"), ncol = 3)
#过滤细胞：保留基因数大于200小于2500的细胞；目的是去掉空GEMS和1个GEMS包含2个以上细胞的数据；而保留线粒体基因的转录本数低于5%的细胞，为了过滤掉死细胞等低质量的细胞数据#
pbmc <- subset(pbmc,subset=nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
#对过滤后的QC metrics 进行可视化（绘制散点图）#
plot1 <- FeatureScatter(pbmc,feature1 = "nCount_RNA",feature2 = "percent.mt")+NoLegend()
plot1
plot2 <- FeatureScatter(pbmc,feature1 = "nCount_RNA",feature2 = "nFeature_RNA")+NoLegend()
plot2
CombinePlots(plots=list(plot1,plot2))
#表达量数据标准化：LogNormalize的算法：A=log(1+(UMIA/UMITotal)*10000)#
pbmc <- NormalizeData(pbmc,normalization.method = "LogNormalize",scale.factor = 10000)
#鉴定表达高变基因2000个，用于下游分析，如PCA#
pbmc <- FindVariableFeatures(pbmc,selection.method = "vst",nfeatures = 2000)
#提取表达量变化最高的10个基因#
top10 <- head(VariableFeatures(pbmc),10)
top10
plot3 <- VariableFeaturePlot(pbmc)+NoLegend()
plot4 <- LabelPoints(plot=plot3,points = top10,repel = TRUE,xnudge = 0,ynudge = 0)
plot4

#PCA分析#
#使用ScaleData()进行数据归一化；默认只是标准化高变基因2000个，速度更快，不影响PCA和分群，但影响热图的绘制#
pbmc <- ScaleData(pbmc,vars.to.regress = "percent.mt")
#而对所有基因进行标准化的方法如下#
all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc,features = all.genes,vars.to.regress = "percent.mt")
#线性降维PCA，默认使用高变基因集，但也可通过features参数自己指定#
pbmc <- RunPCA(pbmc,features = VariableFeatures(object = pbmc))
#查看PCA分群结果，这里只展示前12个PC，每个PC只显示3个基因#
print(pbmc[["pca"]],dims = 1:12,nfeatures = 3)
#绘制pca散点图#
DimPlot(pbmc,reduction = "pca")+NoLegend()
#画前2个主成分的热图#
DimHeatmap(pbmc,dims=1:2,cells=500,balanced = TRUE)
#确定数据集的分群个数#
#方法1，Jackstraw 置换检验算法；重复取样（原数据的1%），重跑PCA，鉴定p-value较小的PC,计算‘null distribution'(即零假设成立时)的基因score#
pbmc <- JackStraw(pbmc,num.replicate = 100)
pbmc <- ScoreJackStraw(pbmc,dims=1:20)
JackStrawPlot(pbmc,dims=1:15)
#方法2：肘部图（碎石图），基于每个主成分对方差解释率的排名；#
ElbowPlot(pbmc)
#分群个数这里选择10，建议尝试选择多个主成分个数做下游分析，对整体影响不大；在选择此参数时
# 建议选择偏高的数字（“宁滥勿缺’，为了活得更多的稀有分群）；有些亚群很罕见，如果没有先验知识，很难将这种大小的数据集与背景噪声区分开来

#非线性降维UMAP/tSNE#
#基于PCA空间中的欧式距离计算nearest neighbor graph, 优化任意两个细胞间的距离权重（输入上一步得到的PC维数#
pbmc <- FindNeighbors(pbmc,dims=1:10)
#接着优化模型，resolution参数决定下游聚类分析得到的分群数，对于3k左右的细胞，设为0.4-1.2能得到较好的结果（官方说明）；如果数据量增大，该参数也应该适当增大#
pbmc <- FindClusters(pbmc,resolution = 0.5)
#使用Idents()函数可查看不同细胞的分群#
head(Idents(pbmc),8)
pbmc <- RunTSNE(pbmc,dims=1:10)
pbmc <- RunUMAP(pbmc,dims = 1:10)
umapplot <- UMAPPlot(pbmc,label = TRUE, pt.size=1.5)+NoLegend()
umapplot
#用DimPlot()函数绘制散点图，reduction="tsne",指定绘制类型；如果不指定，默认先umap，后tsne,最后pca
#也可以直接使用PCAPlot(),TSNEPlot()，UMAPPlot()；cols,pt.size分别调整分组颜色和点的大小#
#绘制Marker基因的umap图#
FeaturePlot(pbmc,features=c("MS4A1","CD14"))

#为分群重新指定细胞类型#
new.cluster.ids <- c("Naive CD4 T","Memory CD4 T","CD14 + Mono","B","CD8 T","FCGR3A+Mono","NK","DC","Platelet")
#将pbmc的水平属性赋值给new.cluster.ids的name属性#
names(new.cluster.ids)
levels(pbmc)
names(new.cluster.ids) <- levels(pbmc)
pbmc <- RenameIdents(pbmc,new.cluster.ids)
#绘制umap图（修改标签后的）#
umapplot2 <- UMAPPlot(pbmc,label = TRUE,pt.size = 0.5)+NoLegend()
umapplot2
