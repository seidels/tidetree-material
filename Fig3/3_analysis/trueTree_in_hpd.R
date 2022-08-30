## ---------------------------
##
## Script name: true_tree_in_hpd.R
##
## Purpose of script: Estimate how often the true
##  tree was recovered in the posterior credible set.
##
## Author: Sophie Seidel
##
## Date Created: 2022-08-29
##
## Copyright (c) Sophie Seidel, 2022
## Email: sophie.seidel@posteo.de
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## set working directory for Mac

setwd("~/Projects/trees-in-devBio/papers/bayesianPhylogeneticInference/Figures/Fig3/3_analysis/")      # Sophie's working directory (mac)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

library(ape)

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R")

## ---------------------------


plotDir = "../4_plots/"


dirindex = 3
hpdtreeDirs = c("../2_inference/inference_logs/10clocks_20scars/", "../2_inference/inference_logs/1clock_20scars/",
                "../2_inference/inference_logs/1clock_2scars/")
hpdtreeDir = hpdtreeDirs[dirindex]

truetreeDir = "../dat/trueTrees/"

hpdfiles =list.files(hpdtreeDir, pattern = "*.hpd.trees")

output = data.frame(dat=rep("", 106), treeInHPD = F, nTreesInHPD=0)

ctr = 1
for (hpdfile in hpdfiles){

  print(hpdfile)

  #get true tree
  if (dirindex == 2){
    datname = strsplit(hpdfile, split = ".", fixed = T)[[1]][3]
  }else{
    datname = strsplit(hpdfile, split = ".", fixed = T)[[1]][3]
  }

  truetreefile = paste0(truetreeDir, datname, ".newick")
  truetree = read.tree(file = truetreefile)

  output[ctr, "dat"] = datname

  # iterate through trees in 95% hpd set and check for identity
  newicks = read.delim(file = paste0(hpdtreeDir, hpdfile), comment.char = "#", skip = 3)
  output[ctr, "nTreesInHPD"] = nrow(newicks)

  for ( newick in newicks$Tree){
    tree = read.tree(text = paste0(newick, ";"))
    recovered = all.equal.phylo(target = truetree, current = tree, use.edge.length = F)
    if(recovered){
      print("Jeih")
      output[ctr, "treeInHPD"] = T
    }
  }
  ctr = ctr + 1
}

saveRDS(object = output, file = paste0(hpdtreeDir, "treeInHPD.RDS"))
output = readRDS(file = paste0(hpdtreeDir, "treeInHPD.RDS"))

# coverage
sum(output$treeInHPD)/nrow(output)

#coverage conditional on <= 10^4 trees in credible set
output_meaningful_credible_set = output[which(output$nTreesInHPD <= 10^4), ]
sum(output_meaningful_credible_set$treeInHPD)/nrow(output_meaningful_credible_set)

g = ggplot(output, aes(treeInHPD, nTreesInHPD))+
  geom_boxplot()
ggsave(filename = paste0(plotDir, "treeInHPD.pdf"), plot = g)
g
