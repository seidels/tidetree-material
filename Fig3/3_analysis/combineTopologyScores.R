## ---------------------------
##
## Script name: combineTopologyScores.R
##
## Purpose of script: Compute the average topology scores
##  across all trees for each analysis and save in scores.tsv
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
## Notes: Run with subDirIdx from 1-3 to run each analysis
##
##
## ---------------------------

## set working directory for Mac

setwd("~/Projects/trees-in-devBio/papers/bayesianPhylogeneticInference/Figures/Fig3/3_analysis/")      # Sophie's working directory (mac)

## ---------------------------

## set up dirs
scoreDir ="../2_inference/treecmpScores/"
subdirs = c("1clock_2scars/", "1clock_20scars/", "10clocks_20scars/")
subdirIdx = 3
scoreDir = paste0(scoreDir, subdirs[subdirIdx])

scoreFiles = list.files(scoreDir, pattern = "dreamChallenge")


## ---------------------------

## collect scores from files and store in data frame

scores = data.frame(file=scoreFiles, RF=0, triplet=0)

for (i in 1:length(scoreFiles)){

  scoreFile = scoreFiles[i]
  score = read.delim(paste0(scoreDir, scoreFile))


  scores[i, "file"] = scoreFile
  scores[i, "RF"] = score$R.F_Cluster_toYuleAvg[1]
  scores[i, "triplet"] = score$Triples_toYuleAvg[1]
}

## ---------------------------

#mean metrics across all 106 colonies
round(mean(as.numeric(scores$RF) , na.rm = T), digits = 2)
round(mean(as.numeric(scores$triplet), na.rm = T), digits = 2)

#mean metrics across 30 test cases
challenge = "../dat/dreamChallengeDat.tsv"
challenge = read.delim(challenge)
challenge$fileName = paste0(challenge$fileName, ".txt")
testset = challenge[which(challenge$partition == "test"), ]

scores$fileName = sapply(X = scores$file, function(x){strsplit(x, split = "treecmp_score_dreamChallengeRange_")[[1]][2]})

merged = merge(scores, challenge, by = "fileName")

mean_rf = round(mean(as.numeric(merged[which(merged$partition == "test"), "RF.x"]), na.rm = T), digits = 2)
mean_triplet = round(mean(as.numeric(merged[which(merged$partition == "test"), "triplet"]), na.rm = T), digits = 2)

write(x = c(subdirs[subdirIdx], mean_rf, mean_triplet), file = "scores.tsv",
      append = T, sep = "\t", ncolumns = 3)
