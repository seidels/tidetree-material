# Description
# This script reads in the ground truth newick trees from dream challenge data frame
# Then it scales to branch lengths from movie frames to hours; it further changes the tip labels to 1:#tips
# Then, it writes the trees as newick files in the same directory as the input trees

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#input
dreamChallenge = read.delim("../dat/dreamChallengeDat.tsv" )
#merged = readRDS(file = "~/Projects/trees-in-devBio/code/intMemoir_analysis/data_processing/correctedMergedInput.Rds")

#output
outputDir = "../dat/trueTrees/"
library(ape)

# transform from frames to hours. 1 frame stands for 15 minutes.
scaleBy = 15/60

for (i in 1:nrow(dreamChallenge)){
 newick_in_frames =  dreamChallenge[i, "ground"]
 tree = read.tree(text = newick_in_frames)
 tree$edge.length = tree$edge.length * scaleBy

 # write tree with cell ids as in intmemoir paper
 write.tree(phy = tree, file = paste0(outputDir, dreamChallenge[i, "fileName"], "_paperLabels.newick"))

 # sort tip labels
 tip_labels = tree$tip.label
 new_labels = 1:length(tip_labels)
 names(new_labels) = tip_labels

 tree$tip.label = new_labels

# write tree with cell ids from 1:#tips --> allows to compare to beast output trees
 write.tree(phy = tree, file = paste0(outputDir, dreamChallenge[i, "fileName"], ".newick"))
}




