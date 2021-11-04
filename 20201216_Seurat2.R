library(Seurat)
library(dplyr)
library(patchwork)
#加载PBMC dataset#
pbmc.data <- Read10X(data.dir="E:/R/Seurat/pbmc")
#查看稀疏矩阵维度#
dim(pbmc.data)
pbmc.data[1:10,1:6]
pbmc <- CreateSeuratObject(counts=pbmc.data,project="pbmc3k",min.cells = 3, min.features = 200)
pbmc
#Lets examine a few genes in the first thirty cells#
pbmc.data[c("CD3D", "TCL1A", "MS4A1"), 1:30]
dense.size <- object.size(as.matrix(pbmc.data))
dense.size
sparse.size <- object.size(pbmc.data)
sparse.size
dense.size/sparse.size

#standard pre-processing workflow#
# The [[ operator can add columns to object metadata. This is a great place to stash QC stats
pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")
# Show QC metrics for the first 5 cells
head(pbmc@meta.data, 5)
# Visualize QC metrics as a violin plot
VlnPlot(pbmc, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
# FeatureScatter is typically used to visualize feature-feature relationships, but can be used
# for anything calculated by the object, i.e. columns in object metadata, PC scores etc.
plot1 <- FeatureScatter(pbmc, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot2 <- FeatureScatter(pbmc, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot1 + plot2
pbmc <- subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)

#Normalizing the data#
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)

#Identification of highly variable features (feature selection)#
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)
# Identify the 10 most highly variable genes
top10 <- head(VariableFeatures(pbmc), 10)
# plot variable features with and without labels
plot1 <- VariableFeaturePlot(pbmc)+NoLegend()
plot2 <- LabelPoints(plot=plot1,points = top10,repel = TRUE,xnudge = 0,ynudge = 0)
plot1+plot2

#Scaling the data#
#mean expression across cells is 0, the variance across cells is 1#
all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)
#remove unwanted sources of variation#
# we could ‘regress out’ heterogeneity associated with (for example) cell cycle stage, or mitochondrial contamination. 
pbmc <- ScaleData(pbmc, vars.to.regress = "percent.mt")

#Perform linear dimensional reduction#
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))
# Examine and visualize PCA results a few different ways
print(pbmc[["pca"]], dims = 1:5, nfeatures = 5)
VizDimLoadings(pbmc, dims = 1:2, reduction = "pca")
DimPlot(pbmc, reduction = "pca")
# DimHeatmap allows for easy exploration of the primary sources of heterogeneity in a dataset, and can be useful when trying to decide which PCs to include for further downstream analyses. Both cells and features are ordered according to their PCA scores
DimHeatmap(pbmc, dims = 1, cells = 500, balanced = TRUE)
DimHeatmap(pbmc, dims = 1:15, cells = 500, balanced = TRUE)

#Determine the 'dimensionality' of the dataset
# In Macosko et al, we implemented a resampling test inspired by the JackStraw procedure. We randomly permute a subset of the data (1% by default) and rerun PCA, constructing a 'null distribution' of feature scores, and repeat this procedure. We identify 'significant' PCs as those who have a strong enrichment of low p-value features.
# NOTE: This process can take a long time for big datasets, comment out for expediency. More
# approximate techniques such as those implemented in ElbowPlot() can be used to reduce
# computation time
pbmc <- JackStraw(pbmc, num.replicate = 100)
pbmc <- ScoreJackStraw(pbmc, dims = 1:20)
# The JackStrawPlot function provides a visualization tool for comparing the distribution of p-values for each PC with a uniform distribution (dashed line). 'Significant' PCs will show a strong enrichment of features with low p-values (solid curve above the dashed line). In this case it appears that there is a sharp drop-off in significance after the first 10-12 PCs.
JackStrawPlot(pbmc, dims = 1:15)
#An alternative heuristic method generates an 'Elbow plot': a ranking of principle components based on the percentage of variance explained by each one (ElbowPlot function). In this example, we can observe an 'elbow' around PC9-10, suggesting that the majority of true signal is captured in the first 10 PCs.
ElbowPlot(pbmc)

#Cluster the cells#
#基于PCA空间中的欧式距离计算nearest neighbor graph, 优化任意两个细胞间的距离权重（输入上一步得到的PC维数#
pbmc <- FindNeighbors(pbmc,dims=1:10)
#接着优化模型，resolution参数决定下游聚类分析得到的分群数，对于3k左右的细胞，设为0.4-1.2能得到较好的结果（官方说明）；如果数据量增大，该参数也应该适当增大#
pbmc <- FindClusters(pbmc,resolution = 0.5)
#使用Idents()函数可查看不同细胞的分群#
head(Idents(pbmc),8)

#Run non-linear dimensional reduction#
pbmc <- RunUMAP(pbmc, dims = 1:10)
# note that you can set `label = TRUE` or use the LabelClusters function to help label
# individual clusters
DimPlot(pbmc, reduction = "umap")
#You can save the object at this point so that it can easily be loaded back in without having to rerun the computationally intensive steps performed above, or easily shared with collaborators.
saveRDS(pbmc, file = "E:/R/Seurat/pbmc/output/pbmc_tutorial.rds")

#Finding differentially expressed features(cluster biomarkers)
# find all markers of cluster 1
cluster1.markers <- FindMarkers(pbmc, ident.1 = 1, min.pct = 0.25)
head(cluster1.markers, n = 5)
# The min.pct argument requires a feature to be detected at a minimum percentage in either of the two groups of cells, and the thresh.test argument requires a feature to be differentially expressed (on average) by some amount between the two groups. You can set both of these to 0, but with a dramatic increase in time - since this will test a large number of features that are unlikely to be highly discriminatory

# find all markers distinguishing cluster 5 from clusters 0 and 3
cluster5.markers <- FindMarkers(pbmc, ident.1 = 5, ident.2 = c(0, 3), min.pct = 0.25)
head(cluster5.markers, n = 5)

# find markers for every cluster compared to all remaining cells, report only the positive ones
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
pbmc.markers %>% group_by(cluster) %>% top_n(n = 2, wt = avg_logFC)

# Seurat has several tests for differential expression which can be set with the test.use parameter (see our DE vignette for details). For example, the ROC test returns the 'classification power' for any individual marker (ranging from 0 - random, to 1 - perfect).
cluster1.markers <- FindMarkers(pbmc, ident.1 = 0, logfc.threshold = 0.25, test.use = "roc", only.pos = TRUE)

#We include several tools for visualizing marker expression. VlnPlot (shows expression probability distributions across clusters), and FeaturePlot (visualizes feature expression on a tSNE or PCA plot) are our most commonly used visualizations. We also suggest exploring RidgePlot, CellScatter, and DotPlot as additional methods to view your dataset.#
VlnPlot(pbmc, features = c("MS4A1", "CD79A"))
# you can plot raw counts as well
VlnPlot(pbmc, features = c("NKG7", "PF4"), slot = "counts", log = TRUE)
FeaturePlot(pbmc, features = c("MS4A1", "GNLY", "CD3E", "CD14", "FCER1A", "FCGR3A", "LYZ", "PPBP", 
                               "CD8A"))
top10 <- pbmc.markers %>% group_by(cluster) %>% top_n(n = 10, wt = avg_logFC)
DoHeatmap(pbmc, features = top10$gene) + NoLegend()

#Assigning cell type identity to clusters#
new.cluster.ids <- c("Naive CD4 T", "Memory CD4 T", "CD14+ Mono", "B", "CD8 T", "FCGR3A+ Mono", 
                     "NK", "DC", "Platelet")
names(new.cluster.ids) <- levels(pbmc)
pbmc <- RenameIdents(pbmc, new.cluster.ids)
DimPlot(pbmc, reduction = "umap", label = TRUE, pt.size = 0.5) + NoLegend()

saveRDS(pbmc, file = "E:/R/Seurat/pbmc/output/pbmc_tutorial.rds")