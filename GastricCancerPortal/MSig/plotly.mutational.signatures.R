
plotly.mutational.signatures.core <- 
  function(types, subtypes, w, 
           ylab = 'Percentage of mutations', xlab = '', title = '',
           colors = rep(brewer.pal(6, 'Dark2'), each=16),
           rect.colors = brewer.pal(6, 'Dark2')
           )
{
  n <- length(w)
  data_bar <- list(
    x=1:n, y=w, subtypes=subtypes, types=types, 
    marker = list(color=colors),
    colors = colors,
    text=paste('Context: ',types, ' in ', subtypes, '; percentage: ', round(w,4)*100,'%',sep='')
    )
  ymax <- 30
  opacity <- 0.2
  p <- plot_ly(
    data_bar, x=x, y=y*100, type='bar', hoverinfo='text', text=text,
    group=types, colors='Reds') %>%           ## colors = colors can be removed
    plotly::layout(
      yaxis=list(title=ylab, showticklabels=TRUE, range=c(0,ymax)),
      xaxis=list(title=xlab, showticklabels=FALSE),
      title = title,
      shapes=list(
        list(type='rect',fillcolor=rect.colors[1],line=list(color=rect.colors[1]),
             opacity=opacity,x0=0,x1=16+0.25,y0=0,y1=ymax),
        list(type='rect',fillcolor=rect.colors[2],line=list(color=rect.colors[2]),
             opacity=opacity,x0=16*1+0.75,x1=16*2+0.25,y0=0,y1=ymax),
        list(type='rect',fillcolor=rect.colors[3],line=list(color=rect.colors[3]),
             opacity=opacity,x0=16*2+0.75,x1=16*3+0.25,y0=0,y1=ymax),
        list(type='rect',fillcolor=rect.colors[4],line=list(color=rect.colors[4]),
             opacity=opacity,x0=16*3+0.75,x1=16*4+0.25,y0=0,y1=ymax),
        list(type='rect',fillcolor=rect.colors[5],line=list(color=rect.colors[5]),
             opacity=opacity,x0=16*4+0.75,x1=16*5+0.25,y0=0,y1=ymax),
        list(type='rect',fillcolor=rect.colors[6],line=list(color=rect.colors[6]),
             opacity=opacity,x0=16*5+0.75,x1=16*6+0.25,y0=0,y1=ymax)
      ),
      showlegend=TRUE
    )
  b <- plot_ly(data_bar, x=w*100, group = types, type='box') %>%
    plotly::layout(
      xaxis=list(title='Percentage of mutations'),
	  showlegend=TRUE
    )
  list(p=p,b=b)
}

plotly.mutational.signatures <- function(infile, ...)
{
  ##d <- read.table(infile,header=TRUE,stringsAsFactors = FALSE)
  d <- read.table(infile,header=FALSE,stringsAsFactors = FALSE)
  colnames(d) <- c('types','subtypes', paste('V',1:7,sep=""))
  d <- sortDataFrame(d, c('types','subtypes'))
  #d <- sortDataFrame(d, colnames(d)[1:2])
types <- d$types
subtypes <- d$subtypes
  
  res <- list()
  for (i in 3:ncol(d)) {
    w <- as.numeric(d[,i])
    res[[i-2]] <- plotly.mutational.signatures.core(types, subtypes, w)
  }
  res
}

# infile:
# /Users/lixiangchun/Public/WorkSpace/Project/EvalutionMutationalSignatureAnalysisAlgs/evaluate_mutational_signature_analysis_algs/Supplementaries/stability.txt
plotly.stability <- function(infile)
{
  p <- plot_ly(d,x=Rank,y=als,name='als',marker=list(size=10)) %>% 
    add_trace(y=mult,name='mult') %>% 
    add_trace(y=brunet,name='brunet') %>% 
    add_trace(y=gdclsNMF,name='gdclsNMF') %>% 
    add_trace(y=sNMFR,name='sNMFR') %>% 
    plotly::layout(
      yaxis=list(title='Stability',range=c(0.1,1.1),showgrid=TRUE),
      xaxis=list(title='NMF rank', range=c(0.5,17),showgrid=TRUE) 
    )
  p
}

library(plotly)
library(RColorBrewer)
setwd('/Users/lixiangchun/Public/WorkSpace/Web/evaluate_mutational_signature_analysis_algs/PhD/MSig')
infile='/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnly/MutationalSignature/output/res_example2_sig_7_signatures.txt'

d <- read.table(infile,header=FALSE,stringsAsFactors = FALSE)

r = plotly.mutational.signatures(infile)

i=1; subplot(r[[i]]$p, r[[i]]$b %>% 
 layout(annotations=list(x=48,y=22,showarrow=FALSE, font=list(size=20),
 text="Dominated by mutations of <br /> T>C and T>G at Cp<em>T</em>pT.", 
 ax=40,ay=40), evaluate=TRUE),nrows=2)


i=2; subplot(r[[i]]$p, r[[i]]$b %>% 
 layout(annotations=list(x=64,y=22,showarrow=FALSE,font=list(size=20),
 text="Dominated by <em>C</em>pG <br />mutations and <br />associated with age.", 
 ax=40,ay=-40), evaluate=TRUE),nrows=2)

i=3; subplot(r[[i]]$p, r[[i]]$b %>% 
 layout(annotations=list(x=70,y=22,showarrow=FALSE,font=list(size=20),
 text="Dominated by <br />mutations at Tp<em>C</em>pT, <br />Tp<em>C</em>pG and Tp<em>T</em>pT.", 
 ax=40,ay=-40), evaluate=TRUE),nrows=2)

i=4; subplot(r[[i]]$p, r[[i]]$b %>% 
 layout(annotations=list(x=70,y=22,showarrow=FALSE,font=list(size=18),
 text="Dominated by mutations of<br />C>T at Gp<em>C</em>pC and Gp<em>C</em>pG.<br />Prevalent in HGC vs RGC.", 
 ax=40,ay=-40), evaluate=TRUE),nrows=2)


i=5; subplot(r[[i]]$p, r[[i]]$b %>% 
 layout(annotations=list(x=70,y=22,showarrow=FALSE,font=list(size=18),
 text="Dominated by mutations of<br />C>T and T>C.<br />Prevalent in HGC vs RGC.", 
 ax=40,ay=-40), evaluate=TRUE),nrows=2)


i=6; subplot(r[[i]]$p %>% layout(yaxis=list(range=c(0,15))), r[[i]]$b %>% 
 layout(annotations=list(x=50,y=12,showarrow=FALSE,font=list(size=18),
 text="APOBEC signature mutations. <br />Prevalent in RGC vs HGC.", 
 ax=40,ay=-40)),nrows=2)

i=7; subplot(r[[i]]$p, r[[i]]$b %>% 
 layout(annotations=list(x=50,y=22,showarrow=FALSE,font=list(size=18),
 text="Dominated by mutations<br /> of C>A at Cp<em>C</em>pC and Cp<em>C</em>pT, <br />and T>C at ApTpG and CpTpG <br />Prevalent in HGC vs RGC.", 
 ax=40,ay=-40)),nrows=2)




