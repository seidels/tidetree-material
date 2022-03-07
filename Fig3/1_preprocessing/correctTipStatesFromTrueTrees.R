library(ape)

# tree dir
dir = "../dat/trueTrees/"
files = list.files(dir, pattern =  "paperLabels")

#alignment dir
outDir = "../dat/correctedFilteredData/"

for (file in files){

 colony = strsplit(file, split = "_paperLabels") [[1]][1]

 # extract tip states from tree files
 tree = read.tree(file = paste0(dir, colony, "_paperLabels.newick"))

 labels = tree$tip.label
 tipDat = t(as.data.frame(sapply(labels, FUN = function(x){strsplit(x, "_", fixed = T)})))
 rownames(tipDat) = NULL
 colnames(tipDat) = c("cell", "state")

 write.table(x = tipDat, sep = "\t",
             row.names = F, quote = F,
             file = paste0(outDir, colony, ".txt")
 )

}

