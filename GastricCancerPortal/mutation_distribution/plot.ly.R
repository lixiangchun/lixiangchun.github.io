library(plotly)
load('/Users/lixiangchun/Public/WorkSpace/Project/TCGA/BasicAna/PANCAN.regular_mutated.gene_mut_freq.RData')
d <- read.table('/Users/lixiangchun/Public/WorkSpace/Project/TCGA/BasicAna/regular_mutated.summary.txt',header=TRUE,stringsAsFactors=FALSE)
n <- d[,2]
names(n) <- d[,1]
cancer_types <- sort(d[,1])

gene <- 'TP53'
freq <- x[gene,]
freq <- freq[cancer_types]

s1 <- paste(cancer_types, ' (n=', n[cancer_types], ')', sep='')
s2 <- paste("Mutation frequency: ", signif(freq, 3) * 100, '%', sep="") 
txt <- paste(s1, s2, sep=". ")
p <- plot_ly(x=cancer_types, y=as.numeric(freq)*100, type='bar', text=txt, hoverinfo='text',marker = list(color = toRGB("grey30"))) %>% layout(title=sprintf('Mutation frequencies of <em>%s</em> among <br />multiple human cancer types', gene), showlegend=FALSE, margin=list(l=50,r=50,b=120,t=100), xaxis=list(title=''), yaxis=list(title='Mutation frequency (%)'))

