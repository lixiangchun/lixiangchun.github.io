
library(lxctk)
data("SMG", package="lxctk")
data("LandscapeColor", package="lxctk")
data("icgc.hcc", package="lxctk") ## ICGC liver cancer data and SMGs selected for NMF clustering.

genes <- names(sort(colSums(icgc.hcc[,Smgs_for_nmf]>2), decreasing=TRUE)) ## Sort by frequency
d <- icgc.hcc[,genes]

#image.color.lixc <- c('white','grey88','#644B39','forestgreen','#FF8B00','#9867CC','#DB1C00', 'black')
## Set the 1st element of image.color.lixc to get rid of background color
image.color.lixc <- c(NA,'grey88','#644B39','forestgreen','#FF8B00','#9867CC','#DB1C00', 'black')

y <- sort.data.frame.by.index(d, rep(0,nrow(d)))
#y <- sort.data.frame.by.index(d, data$nmf_clustid)  ## Sort by black of rows specified in nmf_clustid.

legend.panel <- rbind(data.frame(V1=8,V2='TERT.promoter'), legend.panel)


y = as.matrix(y)
z = t(y)
z = z[rev(rownames(z)),]

freq = paste(signif(apply(z,1,function(e) sum(e>2)) / ncol(z) * 100, 3), '%', sep='')
ynames <- paste(rownames(z), freq)
#p = plot_ly(z=z[rev(rownames(z)),],type='heatmap', colors='Reds')
p = plot_ly(z=z, y=ynames, type='heatmap', colors='Reds') %>% plotly::layout(yaxis=list(title=""), margin=list(l=120,r=50,b=30,t=10), font=list(size=12,family = "Helvetica, Italic"))

