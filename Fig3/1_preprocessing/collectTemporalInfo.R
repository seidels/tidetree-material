library(treeman)
library(stringr)

tipTime = 54

ndLabels = function(nd){
  id = nd[['id']]
  return(id)
}

set_tip_states = function(tree, alignment){
  #sort tip ids by their numeric meaning
  ids = as.character(sort(as.numeric(tree['tips'])))
  # set tip states
  newTree = setNdsOther(tree = tree, ids = ids, vals = alignment$state, slt_nm = "state" )
  newTree = updateSlts(newTree)

  return(newTree)
}

calc_internal_node_state = function(state_child1, state_child2){

  if(is.na(state_child1)){
  stop("State of child 1 is NA")
  }

  if(is.na(state_child2)){
    stop("State of child 1 is NA")
  }

  nrDiffs = adist(state_child1, state_child2)[1,1]

  if(nrDiffs > 0){

    differences = as.data.frame(strsplit(c(state_child1, state_child2), split = ""), col.names = c("state1", "state2"))
    differences$newState = differences$state1
    differences$identical =  differences$state1 == differences$state2
    # Where a character differs between the child states, set the parent state to 1 (unedited)
    differences[which(!differences$identical), "newState"] = 1

    state = paste0(differences$newState, collapse = "")

    # calc difference for each child to parent state
    nrDiffs_child1 = adist(state_child1, state)[1,1]
    nrDiffs_child2 = adist(state_child2, state)[1,1]

    return(list(state, nrDiffs_child1, nrDiffs_child2))

  }else{

      return(list(state_child1, 0, 0))
  }
}

add_forward_time_to_parent = function(tree, node){

  branchLengthToParent  = node['spn']

  if (node['tip']){

    forwardTime = tipTime - branchLengthToParent

  }else{

    nodeTime = getNdSlt(tree = tree, slt_nm = "forwardTime", id = node['id'])
    forwardTime = nodeTime - branchLengthToParent
  }

  parent = node['prid']
  tree = setNdOther(tree, parent, slt_nm = "forwardTime", val = forwardTime)
  tree = updateSlts(tree)

  return(tree)
}

calc_edits_root_to_origin = function(rootState){

  origin_state = "1111111111"

  nrDiffs = adist(rootState, origin_state)[1,1]

  return(nrDiffs)
}

traverse_postorder = function(tree, node){

  if(!node['tip']){
    print(node)

    children = node['ptid']
    child1 = tree[[children[1]]]
    child2 = tree[[children[2]]]

    tree = traverse_postorder(tree, child1)
    tree = traverse_postorder(tree, child2)

    # get children with updated node slots
    child1 = tree[[children[1]]]
    child2 = tree[[children[2]]]

    state1 = getNdSlt(tree = tree, slt_nm = "state", id = child1['id'])
    state2 = getNdSlt(tree = tree, slt_nm = "state", id = child2['id'])

    result = calc_internal_node_state(state1, state2)

    # set number of edits to branches toward children nodes
    tree = setNdOther(tree, node['id'], slt_nm = "state", val = result[[1]])
    tree = updateSlts(tree)
    tree = setNdOther(tree, node['ptid'][1], slt_nm = "nEdits", val = result[[2]])
    tree = updateSlts(tree)
    tree = setNdOther(tree, node['ptid'][2], slt_nm = "nEdits", val = result[[3]])
    tree = updateSlts(tree)

    #set forward time
    tree = add_forward_time_to_parent(tree = tree, node = node)

    # add number of edits to branch from origin to root
    if (node['root']){
      nEdits = calc_edits_root_to_origin(result[[1]])
      tree = setNdOther(tree, node['id'], slt_nm = "nEdits", val = nEdits)
      tree = updateSlts(tree)
    }
  }else{
    tree = setNdOther(tree, node['id'], slt_nm = "forwardTime", val = tipTime)
    tree = updateSlts(tree)
    tree = add_forward_time_to_parent(tree = tree, node = node)
  }
  return(tree)
}


merged = readRDS(file = "correctedMergedInput.Rds")
fileDir = "../dat/correctedFilteredData/"
trueTreeDir = "../dat/trueTrees/trueTreesCenteredNodeHeights/manualCorrection/"
trueTreeFiles = list.files(trueTreeDir)
files = list.files(fileDir)

file=files[1]

n = nrow(merged)

dat = data.frame(colony = rep("",n), treeHeight=rep(0,n), treeLength=0, medEdits=rep(0,n), dist=rep(0,n), maxDist=0)
branchDat = data.frame(colony = rep("", n*60), nodeId = 0, branchLength =0, nEdits=0, nodeTime = 0, branchStartTime=0, editsUntilNode=0)
alignmentDat = data.frame(colony = rep("", n*60), cellID=0, state="", n0=0, n1=0, n2=0)
#intNodeDat = data.frame(colony=rep("", n*60), cellID=0, )
branchDatCtr = 1
alignmentDatCtr = 1


for (i in 1:n){
  colony =merged[i, "colony_id"]

  #get alignment for colony
  file = paste0(colony, ".txt")
  filePath = paste(fileDir, file, sep = "")
  alignment = read.csv(filePath, sep = "", colClasses = "character")
  nCells = nrow(alignment)
  alignment$cellNr = 1:nCells

  #get true tree for colony
  treeFile =  paste0(colony, ".newick")
  tree = readTree(file = paste0(trueTreeDir, treeFile))

  #time and edits -> cor tree height and #edits
  dat[i, "colony"] = colony
  dat[i, "treeHeight"] = tree['age']

  alignment$n0 = str_count(string = alignment[, "state"], pattern = "0")
  alignment$n1 = str_count(string = alignment[, "state"], pattern = "1")
  alignment$n2 = str_count(string = alignment[, "state"], pattern = "2")

  alignmentDat[alignmentDatCtr : (alignmentDatCtr + nCells - 1), "colony"] = colony
  alignmentDat[alignmentDatCtr : (alignmentDatCtr + nCells - 1), "state"] = alignment[, "state"]
  alignmentDat[alignmentDatCtr : (alignmentDatCtr + nCells - 1), "n0"] = alignment[, "n0"]
  alignmentDat[alignmentDatCtr : (alignmentDatCtr + nCells - 1), "n1"] = alignment[, "n1"]
  alignmentDat[alignmentDatCtr : (alignmentDatCtr + nCells - 1), "n2"] = alignment[, "n2"]
  alignmentDat[alignmentDatCtr : (alignmentDatCtr + nCells - 1), "cellID"] = alignment[, "cellNr"]

  alignmentDatCtr = alignmentDatCtr + nCells

  dat[i, "medEdits"] = mean(alignment$n2 + alignment$n0)

  # get intra manhattan distance
  stateMatrix = t(data.frame(strsplit(alignment[, "state"], "")))
  rownames(stateMatrix) = c(paste0("cell", 1:nCells))
  distMatrix = dist(stateMatrix, method = "manhattan")
  dat[i, "dist"] = sum(distMatrix)/ (nCells * (nCells-1) / 2)
  dat[i, "maxDist"] = max(distMatrix)

  # do ancestral state reconstruction
  newTree = set_tip_states(tree = tree, alignment = alignment)
  root =  newTree[[newTree['root']]]
  newTree = traverse_postorder(tree = newTree, node = root)

  #collect branches
  ndIds = c(newTree['nds'], newTree['tips'])
  editsOnBranch = getNdsSlt(tree = newTree, ids = ndIds, slt_nm = "nEdits")
  nodeStates = getNdsSlt(tree = newTree, ids = ndIds, slt_nm = "state")
  editsUntilNode = sapply(nodeStates, FUN = function(x){calc_edits_root_to_origin(x)})
  branches = getNdsSlt(tree = newTree, ids = ndIds, slt_nm = "spn")
  forwardTimes = getNdsSlt(tree = newTree, ids = ndIds, slt_nm = "forwardTime")
  branchDat[branchDatCtr: (branchDatCtr + length(editsOnBranch) -1), "branchLength"] = branches
  branchDat[branchDatCtr: (branchDatCtr + length(editsOnBranch) -1), "nEdits"] = editsOnBranch
  branchDat[branchDatCtr: (branchDatCtr + length(editsOnBranch) -1), "editsUntilNode"] = editsUntilNode
  branchDat[branchDatCtr: (branchDatCtr + length(editsOnBranch) -1), "nodeTime"] = forwardTimes
  branchDat[branchDatCtr: (branchDatCtr + length(editsOnBranch) -1), "nodeId"] =  ndIds
  branchDat[branchDatCtr: (branchDatCtr + length(editsOnBranch) -1), "colony"] = colony
  branchDatCtr = branchDatCtr + length(editsOnBranch)

  dat[i, "treeLength"] = sum(branches)
}
branchDat = branchDat[which(branchDat$colony!=""), ]
branchDat$branchStartTime = branchDat$nodeTime - branchDat$branchLength
alignmentDat = alignmentDat[which(alignmentDat$state != ""), ]
alignmentDat$nEdits = alignmentDat$n0 + alignmentDat$n2

dat$nCells = merged$n.cells
dat$nSampledCells = merged$n_cells

save(alignmentDat, file = "alignmentDat_trueTrees.Rdat")
save(branchDat, file = "branchDat_trueTrees.Rdat")
save(dat, file = "treeDat.Rdat")

#for tests
#save(branchDat, file = "~/Projects/trees-in-devBio/code/intMemoir_analysis/data_processing/branchDat_s1_c1_data_for_tests.Rdat")


