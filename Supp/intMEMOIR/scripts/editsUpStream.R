library(ggplot2)
library(reshape2)
library(RColorBrewer)

load(file = "../../../Fig3/1_preprocessing/branchDat_trueTrees.Rdat") #branchDat_trueTrees.Rdat")
load(file = "../../../Fig3/1_preprocessing/treeDat.Rdat")#.Rdat")
load(file = "../../../Fig3/1_preprocessing/alignmentDat_trueTrees.Rdat")#.Rdat")

plotDir = "../"
mycolors = colorRampPalette(brewer.pal(name = "Blues", n = 9))(10)
width = 14
height = 8
unit = "cm"

# add origin branch
rootNodes = branchDat[which(branchDat$branchLength ==0), ]
rootNodes$branchLength = rootNodes$nodeTime
rootNodes$branchStartTime = 0
branchDat[which(branchDat$branchLength ==0), ] = rootNodes


branchDat$nEditsPerH = branchDat$nEdits/branchDat$branchLength
branchStops = sort(unique(branchDat$nodeTime))
editsUpStream = data.frame(branchStops = branchStops, averageEdits=0)

for (i in 1:length(branchStops)){
  branchStop = branchStops[i]
  matchingBranches = branchDat[which(branchDat$nodeTime == branchStop), ]
  averageEdits = sum(matchingBranches$nEditsPerH)/nrow(matchingBranches)
  editsUpStream[i, "averageEdits"] = averageEdits
}

g= ggplot(editsUpStream, aes(x=branchStops, y=averageEdits)) +
  geom_point()+
  theme_classic()+
  ylab("Average #edits per hour") +
  xlab("Averaged across branches stopping at [h]")

g

svg(filename = paste0(plotDir, "editsUpStream.svg"))
g
dev.off()

ggsave(plot = g, filename = paste0(plotDir, "editsUpStream.pdf"),
       units = unit,
       dpi = 300,
       width = width, height = height )

