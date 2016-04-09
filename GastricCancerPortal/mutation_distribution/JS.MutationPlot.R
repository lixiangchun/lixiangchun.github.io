
library(lxctk)

htmlBegin <- "
<head>
<script src='../TumorPortal_files/plotly-latest.min.js'></script>
</head>

<body>
<div id=\"myDiv\"><!-- Plotly chart will be drawn inside this DIV --></div>
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


JS.mutation_frequency.BarPlot <- function(x_vals, y_vals, txt, gene_symbol, f, barCol_vals, divID="myDiv", newPlotlyJSWillBeAppendedToThisFile=FALSE)
{
  s <- sprintf("

var trace1 = {
  x: [%s],
  y: [%s],
  //hoverinfo: 'text',
  text: [%s],
  type: 'bar',
  marker: {
    color: [%s]
  }
};

var data = [trace1];

var layout = {
  title: 'Mutation frequencies of <em>%s</em> across human cancers',
  width: 880,
  height: 320,
  margin: {
    b: 100
  },
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
       
", x_vals, y_vals, txt, barCol_vals, gene_symbol, "%", divID)

#f = file(outfile, 'w')
  writeLines(htmlBegin, f)
  writeLines(s, f)
  if (newPlotlyJSWillBeAppendedToThisFile) {
    writeLines("</script>", f)
  } else {
    writeLines(htmlEnd, f)
  }
#close(f)
}

#JS.MutationPlot <- function(r, outfile)
## r: a data frame
## f: a writable file handler, e.g. f = file(outfile, 'w')
JS.MutationPlot <- function(r, f, appendToExistingJSplotFile=FALSE)
{
  Missense_Mutation_Shape <- "star-diamond-dot-open"
  Nonsense_Mutation_Shape <- "star-open-dot"
  Splice_Site_Shape <- "y-down-open"
  Frame_Shift_Del_Shape <- "triangle-up-open-dot"
  Frame_Shift_Ins_Shape <- "triangle-down-open-dot"
  In_Frame_Del_Shape <- "hexagon2-open-dot"
  In_Frame_Ins_Shape <- "circle-open-dot"
  
  Nonstop_Mutation_Shape <- "circle-x-open"
  Stop_Codon_Del_Shape <- "hexagram-open"
  Stop_Codon_Ins_Shape <- "hexagram-open-dot"
  # n = 10
  Start_Codon_SNP_Shape <- "star-triangle-down-open-dot"
  Start_Codon_Del_Shape <- "star-triangle-down-open"
  De_novo_Start_OutOfFrame_Shape <- "star-square-open-dot"
  De_novo_Start_InFrame_Shape <- "star-square-open"
  X3UTR_Shape <- "line-ne-open"
  X5UTR_Shape <- "line-nw-open"
  
  Variant_Classification_Shapes <- c(Missense_Mutation_Shape, Nonsense_Mutation_Shape, Splice_Site_Shape,
                Frame_Shift_Del_Shape, Frame_Shift_Ins_Shape, In_Frame_Del_Shape, In_Frame_Ins_Shape,
                Nonstop_Mutation_Shape, Stop_Codon_Del_Shape, Stop_Codon_Ins_Shape, Start_Codon_SNP_Shape,
                Start_Codon_Del_Shape, De_novo_Start_OutOfFrame_Shape, De_novo_Start_InFrame_Shape,
                X3UTR_Shape, X5UTR_Shape)
  Variant_Classification_Shape_Names <- c("Missense_Mutation",'Nonsense_Mutation','Splice_Site',
                          'Frame_Shift_Del','Frame_Shift_Ins','In_Frame_Del','In_Frame_Ins',
                          'Nonstop_Mutation','Stop_Codon_Del','Stop_Codon_Ins','Start_Codon_SNP',
                          'Start_Codon_Del','De_novo_Start_OutOfFrame','De_novo_Start_InFrame',
                          "3'UTR", "5'UTR")
  names(Variant_Classification_Shapes) <- Variant_Classification_Shape_Names
  
MutationTrace <- function(r, Variant_Classification_Type, idx)
{
  d <- r[r$Variant_Classification == Variant_Classification_Type,]
  Protein_Change <- paste_strings(d$Protein_Change)
  Number_of_mutations <- paste_numeric(d$Number_of_mutations)
  txt <- paste_strings(d$functional_pred)
  Variant_Symbols <- paste_strings(Variant_Classification_Shapes[d$Variant_Classification])
  Trace_name <- paste("trace", idx, sep="")
  
  cols <- brewer.pal(8, "Dark2")
  if (length(unique(d$UniProt_Region)) > 8) {
    cols <- colors()
    cols[cols=='white'] <- 'black'
  }
  Domain_idx <- as.integer(as.factor(d$UniProt_Region))
  color_array <- cols[Domain_idx]
  color_array[d$UniProt_Region=='Not_Available'] <- "black"
  color_string <- paste_strings(color_array)
  
Trace_code <- sprintf("
  
var %s = {
  x: [%s],
  y: [%s],
  text: [%s],
  mode: 'markers',
  name: '%s',
  marker: {
    symbol: [%s],
    size: 8,
    color: [%s]
  }
};
", Trace_name, Protein_Change, Number_of_mutations, txt, Variant_Classification_Type, Variant_Symbols, color_string)
  
  list(Trace_name=Trace_name, Trace_code=Trace_code)
}
 
  Variant_Classification_Types <- unique(r$Variant_Classification)
  n <- length(Variant_Classification_Types)
  Trace_codes <- list()
  Trace_names <- rep(NA, n)
  for (i in 1:n) {
    res <- MutationTrace(r, Variant_Classification_Types[i], i)
    Trace_codes[[i]] <- res$Trace_code
    Trace_names[i] <- res$Trace_name
  }
  
  data_string <- sprintf("data = [%s];", paste(Trace_names, collapse=", "))
  Hugo_Symbol <- r$Hugo_Symbol[1]
  
  myDivID <- "myDivUniqueID" ## Make sure that this ID is unique from the previous plotlyjs figure file
  htmlCode <- sprintf("
<div id='%s'></div>
<script>  
", myDivID) ## if appended to other existing plotlyjs figure file, use this htmlCode
  
  layout_string <- sprintf("
var layout = {
  title:'Mutation plot of <em>%s</em> in gastric cancer (Li X.C. et al.)',
  height: 400,
  width: 1000,
  margin: {
    b: 160
  },
  xaxis: {
    title: 'Amino acid changes <br /> Note: different colors except black represent different Pfam domains. <br /> The black color for Pfam domain Not_Available. <br /> Functional prediction with SVM and LR from dbNSFP are provided.'
  },
  yaxis: {
    title: 'Number of mutations'
  }
};
Plotly.newPlot('%s', data, layout);

", Hugo_Symbol, myDivID)
  
  #f = file(outfile, 'w')
  if (appendToExistingJSplotFile == FALSE) {
    writeLines(htmlBegin, f) 
  } else {
    writeLines(htmlCode, f)
  }
  for (i in 1:n) {
    writeLines(Trace_codes[[i]], f)
  }
  writeLines(data_string, f)
  writeLines(layout_string, f)
  writeLines(htmlEnd, f)
  #close(f)
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

SMGs <- setdiff(SMGs, c("AKAP2"))
setwd("/Users/lixiangchun/Public/WorkSpace/Web/PhD/mutation_distribution")
load('../functional_domain/PANCAN_GI.544_Stomach.CancerResearchPaper.functional_domain.RData')

#barCols <- rep('#75778A', length(cancer_types))
barCols <- rep('#4D87CF', length(cancer_types))
barCols[cancer_types=='STAD_LiXCEtAl'] <- "#EE5000"
barCols[cancer_types=='PANCAN'] <- "#FFC409"
barCol_vals <- paste_strings(barCols)

cancer_types_abbr <- read.table("/Users/lixiangchun/Public/WorkSpace/Web/PhD/mutation_distribution/cancer_types_abbr.txt",header=TRUE, stringsAsFactors = FALSE, sep = "\t")
cancer_types_fullname <- cancer_types_abbr$DiseaseName
names(cancer_types_fullname) <- cancer_types_abbr$Cohort

for (gene in SMGs) {
  #gene <- 'PIK3CA'
  print(gene)
  freq <- x[gene,]
  freq <- freq[cancer_types]
  
  s1 <- paste(cancer_types_fullname[cancer_types], ' (n=', n[cancer_types], ')', sep='')
  s2 <- paste("Mutation frequency: ", signif(freq, 3) * 100, '%', sep="") 
  txt <- paste(s1, s2, sep="<br /> ")
  
  x_vals <- paste_strings(cancer_types)
  y_vals <- paste_numeric(as.numeric(freq) * 100)
  txt <- paste_strings(txt)
  
  outfile <- paste(gene, ".html", sep="")
  f = file(outfile, "w")
  
  JS.mutation_frequency.BarPlot(x_vals, y_vals, txt, gene, f, barCol_vals, newPlotlyJSWillBeAppendedToThisFile =TRUE)
  
  ### for mutation plot
  r <- subset(regular_mutated, Hugo_Symbol==gene)
  r <- r[order(r$Start_position, decreasing=FALSE),]
  r$functional_pred <- paste(r$UniProt_Region, r$functional_pred, sep="<br />")
  
  r <- sortDataFrame(r, c("Start_position", "UniProt_Region"))
  
  #h <- subset(hyper_mutated, Hugo_Symbol==gene)
  #h <- h[order(h$Start_position, decreasing=FALSE),]
  #h$functional_pred <- paste(h$UniProt_Region, h$functional_pred, sep="<br />")
  JS.MutationPlot(r, f, appendToExistingJSplotFile=TRUE)
  close(f)
}


