library(ape)
library(treeio)

get_tree_heights = function(phylo){

  nodeTimes = node.depth.edgelength(phy = phylo)
  rootHeight = max(nodeTimes)
  heights = rootHeight - nodeTimes

  return(heights)
}

dirs = c(
  "../inference/inferenceOutput/baseline/",
  "../inference/inferenceOutput/fixScarring/",
  "../inference/inferenceOutput/fixScarring_10alignments/"
)

for (dir in dirs){

  mccFiles = list.files(path = dir, pattern = "stats")

  # take only the first 100 seeds from the large simulation study
  if(dir == dirs[1]){
    mccFiles = mccFiles[sapply(mccFiles, function(x){as.numeric(strsplit(x, split = ".", fixed = T)[[1]][2]) <= 100})]
  }

  output = data.frame(mccFiles = mccFiles, meanPosteriorAb16 = 0, seed=0)
  minsupport = data.frame(mccFiles = mccFiles, minPosteriorAb16 = 0, seed=0)

  for (mccFile in mccFiles){

    #get seed
    seed = as.numeric(strsplit(mccFile, split = ".", fixed = T)[[1]][2])
    output[which(output$mccFiles == mccFile), "seed"] = seed
    minsupport[which(minsupport$mccFiles == mccFile), "seed"] = seed

    # get node annotations
    tree = read.beast(file = paste0(dir, mccFile))
    treedat = as_tibble(tree)

    # get true node heights
    ptree = as.phylo(tree)
    treedat$trueHeight = get_tree_heights(ptree)

    if (max(treedat$trueHeight) <16){
      output[which(output$mccFiles == mccFile), "meanPosteriorAb16"] = NA
      minsupport[which(minsupport$mccFiles == mccFile), "minPosteriorAb16"] = NA

    }else{
      meanPosterior = mean(as.data.frame(treedat[which(treedat$trueHeight >=16), "posterior"])[,1])
      minPosterior = min(as.data.frame(treedat[which(treedat$trueHeight >=16), "posterior"])[,1])
      if(is.na(meanPosterior)){
        stop("Something's going wrong in mean posterior calculation"
        )
      }
      output[which(output$mccFiles == mccFile), "meanPosteriorAb16"] = meanPosterior
      minsupport[which(minsupport$mccFiles == mccFile), "minPosteriorAb16"] = minPosterior
    }
  }
  saveRDS(object = output, file = paste0(dir, "posteriorSupportAtNodes.RDS"))
  saveRDS(object = minsupport, file = paste0(dir, "minSupportAtNodes.RDS"))
}

