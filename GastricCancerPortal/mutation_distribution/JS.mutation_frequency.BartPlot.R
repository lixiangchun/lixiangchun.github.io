
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

paste_strings <- function(string_array=c())
{
  paste(paste("'", string_array, "'", sep=""), collapse=",")
}
paste_numeric <- function(numeric_array=c())
{
  paste(numeric_array, collapse = ",")
}


JS.mutation_frequency.BarPlot <- function(x_vals, y_vals, txt, gene_symbol, outfile, divID="myDiv")
{
  s <- sprintf("

var trace1 = {
  x: [%s],
  y: [%s],
  //hoverinfo: 'text',
  text: [%s],
  type: 'bar',
  marker: {
    color: '#75778A'
  }
};

var data = [trace1];

var layout = {
  title: 'Mutation frequencies of <em>%s</em> across <br /> human cancers',
  width: 800,
  height: 360,
  yaxis: {
    title: 'Mutation freq (%s)'
  },
  xaxis: {
    title: 'Abbr. of human cancer types',
    titlefont: {
      size: 18,
      color: 'black'
    },
    tickfont: {}
  }
};

Plotly.newPlot('%s', data, layout);
       
", x_vals, y_vals, txt, gene_symbol, "%", divID)

f = file(outfile, 'w')
writeLines(htmlBegin, f)
writeLines(s, f)
writeLines(htmlEnd, f)
close(f)

}

SMGs <- c("TP53","ARID1A","CDH1","PIK3CA","XIRP2","NBEA","APC","SMAD4","ERBB4",
          "RHOA","KRAS","COL14A1","AKAP6","CDH11","CTNNB1","BNC2","ERBB2","ITGAV",
          "TGFBR2","CNBD1","RNF43","FBXW7","MAP2K7","CDKN2A","SAMSN1","PIK3R1",
          "ELF3","PIGR","THEMIS","AKAP2","ZHX3")

load('/Users/lixiangchun/Public/WorkSpace/Project/TCGA/BasicAna/PANCAN.regular_mutated.gene_mut_freq.RData')
d <- read.table('/Users/lixiangchun/Public/WorkSpace/Project/TCGA/BasicAna/regular_mutated.summary.txt',header=TRUE,stringsAsFactors=FALSE)
n <- d[,2]
names(n) <- d[,1]
cancer_types <- sort(d[,1])

for (gene in SMGs) {
#gene <- 'PIK3CA'
freq <- x[gene,]
freq <- freq[cancer_types]

s1 <- paste(cancer_types, ' (n=', n[cancer_types], ')', sep='')
s2 <- paste("mutation frequency: ", signif(freq, 3) * 100, '%', sep="") 
txt <- paste(s1, s2, sep="; ")

x_vals <- paste_strings(cancer_types)
y_vals <- paste_numeric(as.numeric(freq) * 100)
txt <- paste_strings(txt)
outfile <- paste(gene, ".html", sep="")
JS.mutation_frequency.BarPlot(x_vals, y_vals, txt, gene, outfile)
}
