# Functions

#Converts data to xml entry
convert_intMemoir_state = function(inputState){

  if(inputState == 1){
    return(0)
  }else if(inputState == 2){
    return(1)
  }else if(inputState == 0){
    return(2)
  }else{
    stop(paste("This is no valid intMemoir input state:", inputState))
  }
}

# convert array into beast array format
convert_intMemoir_array = function(stateNumbers){
  #stateNumbers = as.character(stateNumbers)
  stateList = strsplit(stateNumbers, "")[[1]]

  convertedStates = unname(sapply(stateList, convert_intMemoir_state))

  return(convertedStates)
}


write_alignment_to_xml = function(alignment, filename, site, datName){

  for (i in 1:nrow(alignment)){
    cellNr = alignment[i, "cell"]
    cellState = paste(alignment[i, "beastState"][[1]], collapse=",")

    write(x = c(paste0('<sequence id="', datName, '_cell_', cellNr, '" spec="Sequence" taxon="',
                       i,'" value="', cellState, ',"/>')),
          file = filename, ncolumns = 1, append = TRUE, sep = " ")
  }
}
write_site_alignment_to_xml = function(alignment, filename, site, datName){

  for (i in 1:nrow(alignment)){
    cellNr = alignment[i, "cell"]
    cellState = alignment[i, "beastState"][[1]][site]

    write(x = c(paste0('<sequence id="', datName, '_cell_', cellNr, '_site', site, '" spec="Sequence" taxon="',
                       i,'" value="', cellState, ',"/>')),
          file = filename, ncolumns = 1, append = TRUE, sep = " ")
  }
}

append_site_alignment = function(alignment, filename, datName, nStates=3){

  for (site in 1:10){
    # write cluster alignment start
    write(x = c(paste0('<data  id="', datName, '_site', site, '" spec="Alignment" name="alignment">
                       <userDataType spec="beast.evolution.datatype.ScarData"
                     nrOfStates="', nStates, '" />')),
          file = filename, ncolumns = 1, append = TRUE, sep = " ")

    #write cluster alignment body
    write_site_alignment_to_xml(alignment, filename, site, datName)

    # write alignment end
    write(x = "</data>", file = filename, append = TRUE, sep = " ")
  }

}

append_alignment = function(alignment, filename, datName, nStates=3){


    # write cluster alignment start
    write(x = c(paste0('<data  id="', datName, '" spec="Alignment" name="alignment">
                       <userDataType spec="beast.evolution.datatype.ScarData"
                     nrOfStates="', nStates, '" />')),
          file = filename, ncolumns = 1, append = TRUE, sep = " ")

    #write cluster alignment body
    write_alignment_to_xml(alignment, filename, 1:10, datName)

    # write alignment end
    write(x = "</data>", file = filename, append = TRUE, sep = " ")
}

append_dateTrait_xml = function(nSeqs, date, filename, datName){

  # assemble xml elements
  header = paste0('<traitSet id="', datName, '_dateTrait" spec="beast.evolution.tree.TraitSet" taxa="@', datName,
                  '_TaxonSet" traitname="date-forward"')
  bod = 'value="'

  elems = paste(1:nSeqs, "=", date, sep = "")
  elems = paste(elems, collapse = ",")

  bod = paste0(bod, elems, '"/>')

  # write to xml
  write(x = header,
        file = filename, ncolumns = 1, append = TRUE, sep = " ")
  write(x=bod, file = filename, ncolumns = 1, append = TRUE, sep = " ")
}

append_typeTrait_xml = function(nSeqs, types, filename, datName){

  # assemble xml elements
  header = paste0('<traitSet id="', datName, '_typeTrait" spec="beast.evolution.tree.TraitSet" taxa="@', datName,
                  '_TaxonSet" traitname="type"')
  bod = 'value="'

  elems = paste(1:nSeqs, "=", types, sep = "")
  elems = paste(elems, collapse = ",")

  bod = paste0(bod, elems, '"/>')

  # write to xml
  write(x = header,
        file = filename, ncolumns = 1, append = TRUE, sep = " ")
  write(x=bod, file = filename, ncolumns = 1, append = TRUE, sep = " ")
}

append_rho_xml = function(index, rho, filename){

 parameter = paste0('<parameter id="rho_BDSKY_Contemp.t:input_alignment_', index,
 '" spec="parameter.RealParameter" lower="0.0" name="stateNode" upper="1.0">',
 rho, '</parameter>')

  # write to xml
  write(x=parameter, file = filename, ncolumns = 1, append = TRUE, sep = " ")
}

#Specify directories
fileDir = "../dat/correctedFilteredData/"
files = list.files(fileDir)
dreamChallenge = read.csv(file = "../dat/dreamChallengeDat.tsv", sep = "\t")
colnames(dreamChallenge)[2] = "colony_id"


alignmentXml = "../dat/inputForXml/alignments.xml"
siteAlignmentXml = "../dat/inputForXml/siteAlignments.xml"
dateTraitXml = "../dat/inputForXml/dateTraits.xml"
typeTraitXml = "../dat/inputForXml/typeTraits.xml"

#Run
for (file in files){

  filePath = paste(fileDir, file, sep = "")
  dat = read.csv(filePath, sep = "", colClasses = "character")
  dat$beastState = lapply(X = dat$state, FUN = convert_intMemoir_array)

  append_alignment(alignment = dat, filename = alignmentXml, datName = file)
  append_site_alignment(alignment = dat, filename = siteAlignmentXml, datName = file)
  append_dateTrait_xml(nSeqs = nrow(dat), date = 54, filename = dateTraitXml, datName = file)
  append_typeTrait_xml(nSeqs = nrow(dat), types = 1, filename = typeTraitXml, datName = file)
  }

groundTruthNewickFile = "../dat/groundTruthNewick.csv"
table_s2 = "../dat/abb3099_table_s2.csv"

rhoFile = "../dat/inputForXml/rho.xml"

trueNewick = read.csv(groundTruthNewickFile, sep = "\t", header = T)
colnames(trueNewick)[2] = "colony_id"
subsampledNewick = read.csv(table_s2, sep = ",", header = T, skip = 1)

# generate rho vector
merged = merge(dreamChallenge, trueNewick, by = "colony_id")
#merged =merge(subsampledNewick, trueNewick, by = "colony_id")
merged$rho = merged$nCells / merged$n.cells

# get train / test vector
#merged = merge(merged, dreamChallenge, by = "colony_id")
trainDatVector = merged[which(merged$partition == "train"), "colony_id"]
testDatVector = merged[which(merged$partition == "test"), "colony_id"]

for (i in 1:nrow(merged)){
  index = paste0(merged[i, "colony_id"], ".txt")
  append_rho_xml(index = index, rho = round(merged[i, "rho"], digits = 2), filename = rhoFile)
}

#generate range vector (to be inserted into the xml file in the top range entry)
paste(paste0(merged$colony_id, ".txt"), collapse = ",")

saveRDS(object = merged, file = "correctedMergedInput.Rds")

#test
assertthat::are_equal(convert_intMemoir_array("11111"), c(0,0, 0, 0, 0))
assertthat::are_equal(convert_intMemoir_array("120"), c(0, 1, 2))
