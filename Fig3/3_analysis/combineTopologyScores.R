scoreDir ="../dat/treecmpScores/"
subdirs = c("priorInfoOnScarring/", "unsupervised/" )
subdirIdx = 2
scoreDir = paste0(scoreDir, subdirs[subdirIdx])

scoreFiles = list.files(scoreDir, pattern = "dreamChallenge")

scores = data.frame(file=scoreFiles, RF=0, triplet=0)

for (i in 1:length(scoreFiles)){

  scoreFile = scoreFiles[i]
  score = read.delim(paste0(scoreDir, scoreFile))


  scores[i, "file"] = scoreFile
  scores[i, "RF"] = score$R.F_Cluster_toYuleAvg[1]
  scores[i, "triplet"] = score$Triples_toYuleAvg[1]
}

#mean metrics across all 93 colonies
round(mean(as.numeric(scores$RF) , na.rm = T), digits = 2)
round(mean(as.numeric(scores$triplet), na.rm = T), digits = 2)

#mean metrics across 30 test cases
challenge = "../dat/dreamChallengeDat.tsv"
challenge = read.delim(challenge)
challenge$fileName = paste0(challenge$fileName, ".txt")
testset = challenge[which(challenge$partition == "test"), ]

scores$fileName = sapply(X = scores$file, function(x){strsplit(x, split = "treecmp_score_dreamChallengeRange_")[[1]][2]})

merged = merge(scores, challenge, by = "fileName")

round(mean(as.numeric(merged[which(merged$partition == "test"), "RF.x"]), na.rm = T), digits = 2)
round(mean(as.numeric(merged[which(merged$partition == "test"), "triplet"]), na.rm = T), digits = 2)

