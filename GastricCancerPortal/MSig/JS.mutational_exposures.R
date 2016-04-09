
library(Ckmeans.1d.dp)
#library(RColorBrewer)
#library(plotly)

htmlBegin <- "
<head>
<script src='../TumorPortal_files/plotly-latest.min.js'></script>
</head>

<body>
<div id=\"myDiv\" style=\"width: 780px; height: 600px;\"><!-- Plotly chart will be drawn inside this DIV --></div>
<script>
"
htmlEnd <- "
</script>
</body>
"

JS.number_of_mutation.scatter_plot <- function(x, outfile) {
  htmlBegin <- "
<head>
  <script src='../../TumorPortal_files/plotly-latest.min.js'></script>
  </head>
  
  <body>
  <div id=\"myDiv\" style=\"width: 780px; height: 460px;\"><!-- Plotly chart will be drawn inside this DIV --></div>
  <script>
  "
  htmlEnd <- "
  </script>
</body>
  "
  
  x_vals <- paste(1:nrow(x), collapse = ",")
  y_vals <- paste(x$number_of_mutations, collapse = ",")
  sample_names <- rownames(x)
  
  text_vals <- paste(paste("'", paste("sample:", rownames(x), "<br />Number of mutations:", x$number_of_mutations, "<br />Ckmeans cluster:", x$cluster), "'", sep=""), collapse=",")
  
  n <- table(x$cluster)
  cols <- rep(brewer.pal(3, "Dark2"), n)
  col_vals <- paste(paste("'", cols, "'", sep=""), collapse = ",")
  
  s <- sprintf("
  
var CkmeansClustering = {
  x: [%s],
  y: [%s],
  text: [%s],
  mode: 'markers',
  hoverinfo: 'text',
  marker: {
    size: 8,
    symbol: 'circle',
    color: [%s],
  }
};

var data = [CkmeansClustering];
var layout = {
  title: 'Number of mutations across 100 WGS of GCs',
  xaxis: {
   showticklabels: true,
   title: 'Sample index ordered by cluster ID'
  },
  yaxis: {
   showticklabels: true,
   title: 'Number of mutations'
  }
};
               
Plotly.newPlot('myDiv', data, layout);
  ", x_vals, y_vals, text_vals, col_vals)
  

  f = file(outfile,'w')
  writeLines(htmlBegin, f)
  writeLines(s, f)
  writeLines(htmlEnd, f)
  close(f)
}

paste_strings <- function(string_array=c())
{
  paste(paste("'", string_array, "'", sep=""), collapse=",")
}
paste_numeric <- function(numeric_array=c())
{
  paste(numeric_array, collapse = ",")
}

JS.mutational_exposures.PieChart <- function(ultra.df, hyper.df, regular.df, outfile) {

pie_func <- function(trace_name, d, domain.x, domain.y, name) {
labels <- paste_strings(d$labels)
values <- paste_numeric(d$values)
s <- sprintf("
var %s = {
  values: [%s],
  labels: [%s],
  domain: {
    x: %s, 
    y: %s
  },
  name: '%s',
  //hoverinfo: 'label+percent+name',
  type: 'pie'
};
", trace_name, values, labels, domain.x, domain.y, name)	
}

ultra <- pie_func("ultra", ultra.df,"[0, 0.4]", "[0.4, 1]", "Ultra-mutated")
hyper <- pie_func("hyper", hyper.df, "[0.6, 1]", "[0.4, 1]", "Hyper-mutated")
regular <- pie_func("regular", regular.df, "[0.25, 0.75]", "[0, 0.6]", "Regular-mutated")

lo <- "
  var data = [ultra, hyper, regular];
  var layout = {
    title: 'Mutational exposures in 544<br /> Ultra-, Hyper- and Regular-mutated<br /> gastric cancers'
  };
  Plotly.newPlot('myDiv', data, layout);
"

f = file(outfile, 'w')
writeLines(htmlBegin, f)
writeLines(ultra, f)
writeLines(hyper, f)
writeLines(regular, f)
writeLines(lo,f)
writeLines(htmlEnd, f)
close(f)
}

#setwd("/Users/lixiangchun/Public/WorkSpace/Web/PhD/WGS_MSig/brunet")

#d <- read.table("/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnlyReanalyses/Mutational_Signature_WGS_STAD/originalGenomes")
#sample_names <- read.table("/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnlyReanalyses/Mutational_Signature_WGS_STAD/sampleNames", stringsAsFactors = FALSE)$V1
#number_of_mutations <- colSums(d)

#r <- Ckmeans.1d.dp(number_of_mutations)

#y <- data.frame(cluster=r$cluster, number_of_mutations=number_of_mutations)
#rownames(y) <- sample_names

#x <- y[order(y$number_of_mutations, decreasing = FALSE),]

#JS.number_of_mutation.scatter_plot(x, "Ckmeans_clustering.html")

#d <- read.table('/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnlyReanalyses/Mutational_Signature_WGS_STAD/brunet/output/Rank_eq_9.exposures.txt',header=TRUE)
#d[,1] <- NULL
#regular_mutated_samples <- rownames(x)[x$cluster==1]
#hyper_mutated_samples <- rownames(x)[x$cluster==2]
#ultra_mutated_samples <- rownames(x)[x$cluster==3]

#ultra.df <- data.frame(labels=paste("MSig",1:9), values=rowSums(d[,ultra_mutated_samples]))
#hyper.df <- data.frame(labels=paste("MSig",1:9), values=rowSums(d[,hyper_mutated_samples]))
#regular.df <- data.frame(labels=paste("MSig",1:9), values=rowSums(d[,regular_mutated_samples]))

load('/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnly/ClinicalAna/clinInfoWithMutationalSignature.RData')
group <- as.integer(factor(clinInfoWithMutationalSignature$MutNumClustID, levels=c('UltraMutated','HyperMutated','RegularMutated')))

signatureNames <- paste('MutationalSignature',1:7, sep="")

Ultra=colSums(clinInfoWithMutationalSignature[clinInfoWithMutationalSignature$MutNumClustID=='UltraMutated',signatureNames])
ultra.df=data.frame(labels=paste('MSig',1:7), values=Ultra)
#ultra <- plot_ly(ultra.df, labels=labels, values=values, type='pie',hole=0.5) %>% layout(annotations=list(x=0.5,y=0.5,text='Ultra-mutated GC', showarrow=FALSE, font=list(size=20)))

Hyper=colSums(clinInfoWithMutationalSignature[clinInfoWithMutationalSignature$MutNumClustID=='HyperMutated',signatureNames])
hyper.df=data.frame(labels=paste('MSig',1:7), values=Hyper)
#hyper <- plot_ly(hyper.df, labels=labels, values=values, type='pie',hole=0.5) %>% layout(annotations=list(x=0.5,y=0.5,text='Hyper-mutated GC', showarrow=FALSE, font=list(size=20)))

Regular=colSums(clinInfoWithMutationalSignature[clinInfoWithMutationalSignature$MutNumClustID=='RegularMutated',signatureNames])
regular.df=data.frame(labels=paste('MSig',1:7), values=Regular)
#regular <- plot_ly(regular.df, labels=labels, values=values, type='pie',hole=0.5) %>% layout(annotations=list(x=0.5,y=0.5,text='Regular-mutated GC', showarrow=FALSE, font=list(size=20)))

#p = plot_ly(ultra.df, labels = labels, values = values, type = "pie", hole=0.3,domain = list(x = c(0, 0.4), y = c(0.4, 1)), 
#        name = "Ultra-mutated", showlegend = T) %>%
#  add_trace(data = hyper.df, labels = labels, values = values, type = "pie",hole=0.3, domain = list(x = c(0.6, 1), y = c(0.4, 1)),
#            name = "Hyper-mutated", showlegend = T) %>% 
#  add_trace(data = regular.df, labels = labels, values = values, type = "pie", hole=0.3, domain = list(x = c(0.25, 0.75), y = c(0, 0.6)),
#            name = "Regular-mutated", showlegend = T) %>% 
#  layout(title = "Mutational exposures in <br /> Ultra-, Hyper- and Regular-mutated<br /> gastric cancers")
#htmlwidgets::saveWidget(as.widget(p), "mutational_exposure.html")
JS.mutational_exposures.PieChart(ultra.df, hyper.df, regular.df, "tmp.html")
