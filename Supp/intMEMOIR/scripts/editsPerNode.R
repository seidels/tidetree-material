library(ggplot2)
library(reshape2)
library(RColorBrewer)

load(file = "../../../Fig3/1_preprocessing/branchDat_trueTrees.Rdat") #branchDat_trueTrees.Rdat")
load(file = "../../../Fig3/1_preprocessing/treeDat.Rdat")#.Rdat")
load(file = "../../../Fig3/1_preprocessing/alignmentDat_trueTrees.Rdat")#.Rdat")

# add origin branch
rootNodes = branchDat[which(branchDat$branchLength ==0), ]
rootNodes$branchLength = rootNodes$nodeTime
rootNodes$branchStartTime = 0
branchDat[which(branchDat$branchLength ==0), ] = rootNodes
branchDat$nEditsPerH = branchDat$nEdits / branchDat$branchLength

plotDir = "../"
mycolors = colorRampPalette(brewer.pal(name = "Blues", n = 9))(10)
width = 20
height = 14
unit = "cm"
textsize=20

#
branchDat = branchDat[which(!(branchDat$nodeTime == branchDat$branchStartTime)), ]

nodeTimeBins = cut(branchDat$nodeTime, breaks = seq(0,55,5))
branchDat$nodeTimeBins = nodeTimeBins
unbins = sort(unique(nodeTimeBins))
newdat = data.frame(bins = rep(unbins, 10), edits = rep(0:9, length(unbins)), frequency=0)

#n0=0, n1=0, n2=0,n3=0, n4=0, n5=0, n6=0, n7=0, n8=0, n9=0)
for (bin in unbins){
  freq = as.data.frame(table(branchDat[which(branchDat$nodeTimeBins == bin), "editsUntilNode"]))
  freq$Freq = freq$Freq/sum(freq$Freq)

  for(i in 1:nrow(freq)){
    edit = as.character(freq[i, "Var1"])
    newdat[which(newdat$bins == bin & newdat$edits == edit), "frequency"] = freq[i, "Freq"]
  }
}

g = ggplot(newdat, aes(x=bins, y=frequency, fill=as.factor(edits)))+
  geom_bar(stat = "identity") +
  xlab("Binned node time [h]") +
  ylab("Proportion of #edits")+
  scale_fill_manual(values = mycolors)+
  theme_minimal() + guides(fill=guide_legend(title="#Edits"))+
  theme(legend.position = "top",
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        text = element_text(size = textsize))

g

svg(filename = paste0(plotDir, "nEditsPerNodeTime.svg"))
g
dev.off()

ggsave(plot = g, filename = paste0(plotDir, "nEditsPerNodeTime.pdf"),
       units = unit,
       dpi = 300,
       width = width, height = height )



