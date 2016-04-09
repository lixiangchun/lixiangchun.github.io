



plotly.mutational.signatures <-
  function(infile, color = c('#702E33','#BE5C81','#836E8D','#EC8C70','#596C22','#A7B762'),
           colors = rep(color, each = 16),
           title = 'Mutational signatures operative in cancer',
           xlab = '', showxticklabels = FALSE,
           ylab = 'Mutations (%)')
  {
    
  bp <- list()
  bx <- list()
    
  d <- read.table(infile, header = TRUE, strip.white = FALSE)
  subtypes <- d[,'subtypes']
  types <- d[,'types']
  contexts <- paste(subtypes, types, sep = '.')
  
  # https://plot.ly/r/axes/ for more info about axes adjustment.
  
  f <- list(
    family = "Arial, sans-serif",
    size = 6,
    color = "lightgrey"
  )
  nr <- ncol(d)
  for (i in 3:nr) {
    data_bar <- list(x=contexts, y=as.numeric(d[,i]), subtypes=subtypes, types=types, 
                     marker = list(color=colors), name=paste('MSig',i-2),
                     text=paste('Context: ',types, ' in ', subtypes, '; percentage: ', round(as.numeric(d[,i]),3)*100,'%',sep=''),
                     legend=list(font=list(color=color[i])) )
    
    f <- list(
      ##family = "Arial, sans-serif",
      size = 7,
      color = 'black'
    )
    xaxis_layout <- list(title = xlab, showticklabels = showxticklabels, tickfont = f)
    #if (max(d[,i]) < 0.2) {
    #  yaxis_layout <- list(title = ylab, showticklabels = TRUE, range=c(0,0.2))
    #} else {
    #  yaxis_layout <- list(title = ylab, showticklabels = TRUE)
    #}
    
    ## dynamically set range does not work, the following range always =c(0,0.3) @@@@
    yaxis_layout <- list(title = ylab, showticklabels = TRUE, 
                         range=c(0, ifelse(max(d[,i]) < 0.3, 0.3, max(d[,i])))) 
    #p <- plot_ly(data = data_bar, x = x, y = y, type = 'bar', marker = marker) %>%
    #plotly::layout(title = title, yaxis=yaxis_layout, xaxis=xaxis_layout)
    #p <- plot_ly(data = data_bar, x = x, y = y, type = 'bar', marker = marker)
    
    ## Material links:
    ## Adding Text and Annotations in R   https://plot.ly/r/text-and-annotations/
    #lf <- list(
    #  family = "sans-serif",
    #  size = 12,
    #  color = colors
    #)
    #l <- list(
    #  x = 0.5, y = -0.3,
    #  font = lf,
    #  bgcolor = "#E2E2E2",
    #  bordercolor = "#FFFFFF",
    #  borderwidth = 2
    #)
    
    p <- plot_ly(
      data=data_bar, x=contexts, y=y, type='bar', hoverinfo='text', marker=marker, name=name,
      text=text) %>% 
      plotly::layout(title=title, showlegend=FALSE,
                     yaxis=yaxis_layout,
                     xaxis=xaxis_layout,
                     width=700, height=360
                     )
    b <- plot_ly(data_bar, x=y*100, group=types,type='box') %>% 
        plotly::layout(
            yaxis=list(title='Spectrum'), 
            xaxis=list(title='Fraction of mutations (%)'))
    bp[[i - 3 + 1]] <- p
    bx[[i - 3 + 1]] <- b
  }
  list(barplot=bp,boxplot=bx)
}

plotly.mutational.signatures_v2 <- function(d)
{
  subtypes <- d[,'subtypes']
  types <- d[,'types']
  contexts <- paste(subtypes, types, sep = '.')
  d$contexts <- contexts
  p <- plot_ly(d, x=contexts, y=processes1, xaxis='x1', yaxis='y1', type='bar')
  p <- add_trace(p, x=contexts, y=processes2, xaxis='x1', yaxis='y2', type='bar')
  p <- layout(p, showlegend=FALSE, yaxis=list(anchor='x'),
              yaxis2=list(anchor='x'), title='test')
  p
}

library(plotly)
setwd('/Users/lixiangchun/Public/WorkSpace/Web/evaluate_mutational_signature_analysis_algs/PhD/MSig')
infile='/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnly/MutationalSignature/output/res_example2_sig_7_signatures.txt'
r <- plotly.mutational.signatures(infile, showxticklabels = FALSE)
p <- list()
for (i in 1:7) {
  p[[i]] <- subplot(r$barplot[[i]],r$boxplot[[i]],nrows=2)
}

#htmlwidgets::saveWidget(as.widget(res[[1]]),'hcc_sig1.html')

#subplot(p1,p2,nrows=1, filename='mutational_signatures_in_HCC.html')

#subplot(res[[1]],res[[2]], res[[3]],nrows=3,margin = 0.015) %>% layout(showlegend=F, autosize=F, width=800,height=80*6)



