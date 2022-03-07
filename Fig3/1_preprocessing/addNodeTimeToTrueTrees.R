
change_branch_lengths = function(childNode, tree){

  currentBrachLength = childNode['spn']

  if(childNode['tip']){
    newBranchLength = currentBrachLength + 0.125
  }else{
    newBranchLength = currentBrachLength + 0.25
  }
  tree = setNdSpn(tree, id = node['id'], val = newBranchLength)

  return(tree)
}



library(treeman)
dir = "../dat/trueTrees/"
outDir = "../dat/trueTrees/trueTreesCenteredNodeHeights/"

treeFiles = list.files(dir, pattern = "data.newick")

for (file in treeFiles){

  tree = treeman::readTree(file = paste0(dir, file))

  for (nodeID in c(tree['nds'], tree['tips'])){
    print(nodeID)
    node = tree[[nodeID]]
    if (node['root']){
       next()
     }else{
       tree = change_branch_lengths(node, tree = tree)
     }
  }
  writeTree(tree = tree, file = paste0(outDir, file))

}


