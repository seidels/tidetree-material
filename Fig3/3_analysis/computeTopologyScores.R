## ---------------------------
##
## Script name: computeTologoyScores.R
##
## Purpose of script: Calculate the score assessing tree topology
##
## Author: Sophie Seidel
##
## Date Created: 2022-08-17
##
## Copyright (c) Sophie Seidel, 2022
## Email: sophie.seidel@posteo.de
##
## ---------------------------
##
## Notes: Adjust the subDirIdx from 1-3 to compute the
##  topology scores for all 3 analyses
##
## ---------------------------

## set working directory for Mac

setwd("~/Projects/trees-in-devBio/papers/bayesianPhylogeneticInference/Figures/Fig3/3_analysis/")

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up our functions into memory

source("tree_scripts.R")

## ---------------------------

## init directories and find mcc (maximum clade credibility) tree files

intmemdir = "../2_inference/inference_logs/"
subdirs = c("1clock_2scars/", "1clock_20scars/", "10clocks_20scars/")
subdirIdx = 3 # run consecutively for 1:3

intmemdir = paste0(intmemdir, subdirs[subdirIdx])
files = list.files(intmemdir, pattern = "mcc.tree")

## ---------------------------

## Convert the mcc trees from nexus into newick files, s.t. TreeCmp can process them

for (nexusFile in files){
  print(nexusFile)

  filename_without_extension = strsplit(x = nexusFile, split = ".tree$")[[1]][1]
  newickFile = paste0(filename_without_extension, ".newick")
  print(newickFile)
  convert_nexus_to_newick(nexusFile = paste0(intmemdir, nexusFile),
                          newickFile = paste0(intmemdir, newickFile))

}

## ---------------------------

## run TreeCmp to score the estimated tree topology

newickFiles = list.files(path = intmemdir, pattern = "*.newick")
trueTreeDir = "../dat/trueTrees/"
scoreDir = paste0("../2_inference/treecmpScores/",
                  subdirs[subdirIdx])

for (newickFile in newickFiles){

  colonyName = strsplit(newickFile, split = ".", fixed = T)[[1]][3]
  dreamChallengeRange = grepl(pattern = "dreamChallengeRange", x = newickFile)
  trueTreeFile = paste0(trueTreeDir, colonyName, ".newick")

  command = "java -jar /Users/seidels/frameworks/TreeCmp/bin/TreeCmp.jar"
  if (dreamChallengeRange){
    args = paste0("-P -N -I -d 'rc' 'tt' -r ",  trueTreeFile,
                  " -i ", paste0(intmemdir, newickFile),
                  " -o ", paste0(scoreDir, "treecmp_score_dreamChallengeRange_", colonyName, ".txt"))
  }else{
    args = paste0("-P -N -I -d 'rc' 'tt' -r ",  trueTreeFile,
                  " -i ", paste0(intmemdir, newickFile),
                  " -o ", paste0(scoreDir, "treecmp_score_", colonyName, ".txt"))
  }
  system(command = paste(command, args))
}
