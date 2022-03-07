source("tree_scripts.R")

# Convert the mcc trees from nexus into newick files, s.t. TreeCmp can process them
intmemdir = "../2_inference/inference_logs/"
subdirs = c("priorInfoOnScarring/", "unsupervised/")
subdirIdx = 2

intmemdir = paste0(intmemdir, subdirs[subdirIdx])
files = list.files(intmemdir, pattern = "mcc.tree")

for (nexusFile in files){
  print(nexusFile)

  filename_without_extension = strsplit(x = nexusFile, split = ".tree$")[[1]][1]
  newickFile = paste0(filename_without_extension, ".newick")
  print(newickFile)
  convert_nexus_to_newick(nexusFile = paste0(intmemdir, nexusFile),
                          newickFile = paste0(intmemdir, newickFile))

}

# run TreeCmp
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
