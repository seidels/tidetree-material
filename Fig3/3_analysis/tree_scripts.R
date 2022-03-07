
convert_nexus_to_newick = function(nexusFile, newickFile){
  tree = ape::read.nexus(nexusFile)
  tree$node.label = NULL
  ape::write.tree(phy = tree, file = newickFile)
}
