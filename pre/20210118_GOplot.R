library(GOplot)
data(EC)
head(EC$david)
head(EC$genelist)
head(EC$eset)
head(EC$genes)
head(EC$process)
circ <- circle_dat(EC$david,EC$genelist)
circ <- circ[order(circ$adj_pval),]
circ2 <- unique(sort(circ$adj_pval))
circ2[50]
d <- which(circ$adj_pval<1e-20)
circ3 <- circ[1:872,]
#Barplot#
GOBar(subset(circ,category='BP'))
GOBar(circ,display = 'multiple')
# Facet the barplot, add a title and change the colour scale for the z-score
GOBar(circ, display = 'multiple', title = 'Z-score coloured barplot', zsc.col = c('yellow', 'black', 'cyan'))

#the bubble plot (GOBubble)#
# Generate the bubble plot with a label threshold of 3
GOBubble(circ3, labels = 3)
# Add a title, change the colour of the circles, facet the plot according to the categories and change the label threshold
GOBubble(circ, title = 'Bubble plot', colour = c('orange', 'darkred', 'gold'), display = 'multiple', labels = 3)  
# Colour the background according to the category
GOBubble(circ, title = 'Bubble plot with background colour', display = 'multiple', bg.col = T, labels = 3)  
# Reduce redundant terms with a gene overlap >= 0.75...
reduced_circ <- reduce_overlap(circ, overlap = 0.75)
# ...and plot it
GOBubble(reduced_circ, labels = 2.8)

#Circular visualization of results of gene-annotation enrichemnt analysis(GOCircle)#
#Generate a circular visualization of the results of gene- annotation enrichment analysis
GOCircle(circ)

# Generate a circular visualization of selected terms
IDs <- c('GO:0007507', 'GO:0001568', 'GO:0001944', 'GO:0048729', 'GO:0048514', 'GO:0005886', 'GO:0008092', 'GO:0008047')
GOCircle(circ, nsub = IDs)

# Generate a circular visualization for 10 terms
GOCircle(circ, nsub = 10)

#GOChord Display of the relationship between genes and terms GOChord#
# Define a list of genes which you think are interesting to look at. The item EC$genes of the toy 
# sample contains the data frame of selected genes and their logFC. Have a look...
head(EC$genes)
# Since we have a lot of significantly enriched processes we selected some specific ones (EC$process)
EC$process
# Now it is time to generate the binary matrix
chord <- chord_dat(circ, EC$genes, EC$process)
head(chord)
# Generate the matrix with a list of selected genes
chord <- chord_dat(data = circ, genes = EC$genes)
# Generate the matrix with selected processes
chord <- chord_dat(data = circ, process = EC$process)
# Create the plot
chord <- chord_dat(data = circ, genes = EC$genes, process = EC$process)
GOChord(chord, space = 0.02, gene.order = 'logFC', gene.space = 0.25, gene.size = 5)

# Display only genes which are assigned to at least three processes
GOChord(chord, limit = c(3, 0), gene.order = 'logFC')

#GOHeat Heatmap of genes and terms#
GOHeat(chord[,-8], nlfc = 0)

# Now we create the heatmap with logFC values and user-defined colour scale
GOHeat(chord, nlfc = 1, fill.col = c('red', 'yellow', 'green'))

#GOCluster#
GOCluster(circ, EC$process, clust.by = 'logFC', term.width = 2)
GOCluster(circ, EC$process, clust.by = 'term', lfc.col = c('darkgoldenrod1', 'black', 'cyan1'))

#GOVenn#
l1 <- subset(circ, term == 'heart development', c(genes,logFC))
l2 <- subset(circ, term == 'plasma membrane', c(genes,logFC))
l3 <- subset(circ, term == 'tissue morphogenesis', c(genes,logFC))
GOVenn(l1,l2,l3, label = c('heart development', 'plasma membrane', 'tissue morphogenesis'))



