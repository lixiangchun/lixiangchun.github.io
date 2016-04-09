

JSCodeCore <- function(i, x=c(), y=c(), color, name, text, title="") {
s1 <- sprintf("
var trace%s = {
  x: [%s],
  y: [%s],
  marker: {
    color: '%s',
  },
  name: '%s',
  type: 'bar',
  hoverinfo: 'text',
  text: [%s],
};", i, paste(x, collapse=','), paste(y, collapse=','), color, name, text)

list(tracecode=s1, tracename=paste("trace",i,sep=""))
}

HtmlJS <- function(plotlyJSFile, htmlJSOutFile, width="800px", height="420px")
{
s <- sprintf("
<head>
  <script src=\"plotly-latest.min.js\"></script>
</head>
<body>
  <div id=\"myDiv\" style=\"width: %s; height: %s;\"><!-- Plotly chart will be drawn inside this DIV --></div>
  <script src=\"%s\">
    <!-- JAVASCRIPT CODE GOES HERE -->
  </script>
</body>", width, height, plotlyJSFile)
f <- file(htmlJSOutFile, 'w')
writeLines(s, f)
close(f)
}

JSCode <- function(infile, plotlyjs=NULL, width=NULL, height=NULL, colors=brewer.pal(6, 'Dark2'), output.file.prefix='MSig') {
  
  if (is.null(plotlyjs) || missing(plotlyjs)) {
    plotlyjs <- "../../TumorPortal_files/plotly-latest.min.js"
  }
  if (is.null(width)) {width <- "800px"}
  if (is.null(height)) {height <- "360px"}
  
htmlCodeBegin <- sprintf("
<head>
  <script src=\"%s\"></script>
</head>
<body>
  <div id=\"myDiv\" style=\"width: %s; height: %s;\"><!-- Plotly chart will be drawn inside this DIV --></div>
  <script>
", plotlyjs, width, height)

htmlCodeEnd <- "
  </script>
</body>
"

	d <- read.table(infile,header=TRUE,stringsAsFactors=TRUE)
	d <- lxctk::sortDataFrame(d, c('types','subtypes'))
	idx <- 1:nrow(d)
	categs <- levels(d$types)
	ncateg <- nlevels(d$types)
	types <- as.character(d$types)
	subtypes <- as.character(d$subtypes)
	for (i in 3:ncol(d)) {
		tracenames <- c()
		shapes <- rep("", ncateg)
		htmlJSOutFile <- paste(output.file.prefix, i-2, ".html", sep="")
		f <- file(htmlJSOutFile, 'w')
		writeLines(htmlCodeBegin, f)
		for (j in 1:ncateg) {
			x <- idx[types == categs[j]]
			y <- d[,i][types == categs[j]] * 100
			txt <- subtypes[types == categs[j]]
			text <- paste(categs[j], ' mutation in ', txt, '<br /> Percentage:', paste(round(y,2),'%',sep=""), sep="")
			text <- paste(paste("'", text, "'", sep=""), collapse=',')
			r <- JSCodeCore(i-2+j, x=x, y=y, color=colors[j], name=categs[j], text=text, title=paste('MSig',i-2,sep=''))
			writeLines(r$tracecode, f)
			tracenames <- c(tracenames, r$tracename)
      if (j == 1) {
				shapes[j] <- sprintf("{type: 'rect', xref: 'x', x0: 0, x1: 16 + 0.25, y0:0,y1:30,fillcolor:'%s',opacity:0.3,line:{color:'%s'}}", colors[j], colors[j])
			} else {
				shapes[j] <- sprintf("{type:'rect',xref:'x',x0:16*(%s-1)+0.75, x1:16*%s+0.25,y0:0,y1:30,fillcolor:'%s', opacity:0.2,line:{color:'%s'}}", j, j, colors[j], colors[j])
			}
		}
writeLines(sprintf("var data = [%s];", paste(tracenames, collapse=',')), f)
s2 <- sprintf("var layout = {
  yaxis: {
    title: 'Percentage of <br />mutations',
    range: [0, 30],
  },
  xaxis: {
    showticklabels: true
  },
  //barmode: 'group',
  showlegend: true,
  shapes: [%s]
};", paste(shapes, collapse=','))
writeLines(s2, f)
writeLines("Plotly.newPlot('myDiv', data, layout);", f)
writeLines(htmlCodeEnd, f)
close(f)
	}
}

library(RColorBrewer)
library(lxctk)
infile="/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnlyReanalyses/Mutational_Signature_WGS_STAD/brunet/output/Rank_eq_9.processes.txt"
JSCode(infile)
