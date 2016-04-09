

load('/Users/lixiangchun/Public/WorkSpace/Project/DigestiveSystemCancer/Analysis/iCGA_PanelOfNormals/StomachPublishedDataOnly/ClinicalAna/clinInfoWithMutationalSignature.RData')
group <- as.integer(factor(clinInfoWithMutationalSignature$MutNumClustID, levels=c('UltraMutated','HyperMutated','RegularMutated')))

signatureNames <- paste('MutationalSignature',1:7, sep="")

Ultra=colSums(clinInfoWithMutationalSignature[clinInfoWithMutationalSignature$MutNumClustID=='UltraMutated',signatureNames])
ultra.df=data.frame(labels=paste('MSig',1:7), values=Ultra)
ultra <- plot_ly(ultra.df, labels=labels, values=values, type='pie',hole=0.5) %>% layout(annotations=list(x=0.5,y=0.5,text='Ultra-mutated GC', showarrow=FALSE, font=list(size=20)))

Hyper=colSums(clinInfoWithMutationalSignature[clinInfoWithMutationalSignature$MutNumClustID=='HyperMutated',signatureNames])
hyper.df=data.frame(labels=paste('MSig',1:7), values=Hyper)
hyper <- plot_ly(hyper.df, labels=labels, values=values, type='pie',hole=0.5) %>% layout(annotations=list(x=0.5,y=0.5,text='Hyper-mutated GC', showarrow=FALSE, font=list(size=20)))

Regular=colSums(clinInfoWithMutationalSignature[clinInfoWithMutationalSignature$MutNumClustID=='RegularMutated',signatureNames])
regular.df=data.frame(labels=paste('MSig',1:7), values=Regular)
regular <- plot_ly(regular.df, labels=labels, values=values, type='pie',hole=0.5) %>% layout(annotations=list(x=0.5,y=0.5,text='Regular-mutated GC', showarrow=FALSE, font=list(size=20)))

p = plot_ly(ultra.df, labels = labels, values = values, type = "pie", hole=0.3,domain = list(x = c(0, 0.4), y = c(0.4, 1)), 
        name = "Ultra-mutated", showlegend = T) %>%
  add_trace(data = hyper.df, labels = labels, values = values, type = "pie",hole=0.3, domain = list(x = c(0.6, 1), y = c(0.4, 1)),
            name = "Hyper-mutated", showlegend = T) %>% 
  add_trace(data = regular.df, labels = labels, values = values, type = "pie", hole=0.3, domain = list(x = c(0.25, 0.75), y = c(0, 0.6)),
            name = "Regular-mutated", showlegend = T) %>% 
  layout(title = "Distribution of mutational exposures in <br /> Ultra-, Hyper- and Regular-mutated<br /> gastric cancers")
