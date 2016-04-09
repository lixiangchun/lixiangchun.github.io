
library(lxctk)
library(plotly)
library(lsa)
Alexandrov <- read.table('/Users/lixiangchun/Public/WorkSpace/Database/AlexandrovEtAl_signatures.txt',header=TRUE,stringsAsFactors=FALSE,sep="\t")
#lixc <- read.table('/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnly/MutationalSignature/output/res_example2_sig_7_signatures.txt',header=FALSE,stringsAsFactors=FALSE)
lixc <- read.table('/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnlyReanalyses/Mutational_Signature_WGS_STAD/brunet/output/Rank_eq_9.processes.txt',header=TRUE, stringsAsFactors=FALSE)

Alexandrov$Somatic.Mutation.Type <- NULL

Alexandrov <- sortDataFrame(Alexandrov, c('Substitution.Type', 'Trinucleotide'))
#lixc <- sortDataFrame(lixc, c('V1','V2'))
lixc <- sortDataFrame(lixc, c('types','subtypes'))

Alexandrov$Substitution.Type = NULL
Alexandrov$Trinucleotide = NULL
#lixc$V1 = NULL
#lixc$V2 = NULL
lixc[,"types"] <- NULL
lixc[,"subtypes"] = NULL

x <- matrix(nr=ncol(lixc), nc=ncol(Alexandrov), NA)
	print(head(lixc))
for (i in 1:ncol(lixc)) {
	for (j in 1:ncol(Alexandrov)) {
		x[i, j] <- cosine(lixc[,i], Alexandrov[,j])
	}
}
colnames(x) <- colnames(Alexandrov)
rownames(x) <- paste('MSig',1:nrow(x),sep="")
txt <- round(x,2)
txt[txt<0.5] <- NA
#aheatmap(x, 'Reds', revC = F, Rowv = F, Colv = F, txt=txt)

Cosine <- x
#p = plot_ly(z=Cosine,type='heatmap', colorscale='Reds', y=rownames(x), x=colnames(x)) %>% layout(yaxis=list(title='Li X.C. WGS et al'),xaxis=list(title='Alexandrov et al. 21 mutational signatures',ticklen=0, showticklabels=FALSE))
#htmlwidgets::saveWidget(as.widget(p), "Cosine_similarity_with_Alexandrov_etal.html")



