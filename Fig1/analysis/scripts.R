library(tracerer)

get_sumstat_names_from_parameters = function(parameters){

  paramCols = unlist(lapply(parameters, function(x){
    c(paste0(x,"_lower"), paste0(x, "_median"), paste0(x, "_upper"))
  }))

  return(paramCols)
}

scar_present = function(alignment_file){

  if (any(grepl(" 1,", readLines(alignment_file))) |
      any(grepl(",1,", readLines(alignment_file))) |
      any(grepl(",1$", readLines(alignment_file))) ){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

get_ntaxa = function(alignment_file){

  line_4 = strsplit(readLines(alignment_file)[4], split = "=")

  #check that ntax is defined on that line
  if(grepl(line_4[[1]][1],pattern = "ntax")){

    ntaxa = strsplit(line_4[[1]][2], split = ";")[[1]][1]
    return(ntaxa)
  }else{
    stop("ntaxa not defined in line 4")
  }
}

get_seed_from_file_name = function(filename){
  return(strsplit(filename, fixed = TRUE, split = ".")[[1]][2])
}

get_simulation_params_from_file = function(simFile){

  simDat = read.csv(file = paste0(simFile), sep = ",", header = T )

  return(simDat)
}

get_tree_params_from_sim_log = function(simulation_log){

  simOutput = read.csv(simulation_log, header = T, sep = "\t")

  if(nrow(simOutput == 1)){
    return(c(simOutput$simulatedTree.height, simOutput$simulatedTree.treeLength))
  }else{
    return(c(NA, NA))
  }
}


get_summary_stats_from_log = function(estimates, parameter_col_index_map){

  parameters = names(parameter_col_index_map)

  output = data.frame(matrix(nrow = 1,  ncol = 3*length(parameters)))
  colnames(output) = get_sumstat_names_from_parameters(parameters)

  for (parameter in parameters){

    col_index = as.numeric(parameter_col_index_map[parameter])
    parameter_posterior = estimates[, col_index]
    hpd = get_hpd_from_posterior(posterior = parameter_posterior)
    med = median(as.numeric(parameter_posterior))

    output[1, get_sumstat_names_from_parameters(parameter)] = c(hpd[[1]], med, hpd[[2]])
  }
  return(output)
}

get_coverage_from_dat = function(outputDat, simParams, parameters) {

  nParams = length(parameters)
  coverage = data.frame(matrix(nrow = nrow(outputDat), ncol = nParams, data = 0))
  colnames(coverage) = parameters

  for (i_seed in 1:nrow(outputDat)){
    for(parameter in parameters){

      coverage[i_seed, parameter] =
        get_coverage(hpd_lower=outputDat[i_seed, paste0(parameter, "_lower")],
                     hpd_upper=outputDat[i_seed, paste0(parameter, "_upper")],
                     truth = simParams[i_seed, parameter])
    }
  }

  #coverage = coverage/nrow(outputDat)

  return(coverage)
}

get_hpd_from_dat = function(outputDat, simParams, parameters){

  nParams = length(parameters)
  hpd = data.frame(matrix(nrow = nrow(outputDat), ncol = nParams, data = 0))
  colnames(hpd) = parameters

  for (i_seed in 1:nrow(outputDat)){
    for(parameter in parameters){

      truth = simParams[i_seed, parameter]

      parameter_hpd = get_hpd_width_rel(hpd_lower = outputDat[i_seed, paste0(parameter, "_lower")],
                                        hpd_upper = outputDat[i_seed, paste0(parameter, "_upper")],
                                        truth = truth)
      hpd[i_seed, parameter] = parameter_hpd
    }
  }
  #hpd = hpd/nrow(outputDat)
  return(hpd)
}
get_rmse_from_dat = function(outputDat, simParams, parameters){

  nParams = length(parameters)
  rmse = data.frame(matrix(nrow = nrow(outputDat), ncol = nParams, data = 0))
  colnames(rmse) = parameters

  for (i_seed in 1:nrow(outputDat)){
    for(parameter in parameters){

      truth = simParams[i_seed, parameter]
      parameter_rmse = get_RMSE_rel(posterior_median = outputDat[i_seed, paste0(parameter, "_median")],
                                    truth = truth)
      rmse[i_seed, parameter] =  parameter_rmse
    }
  }

  #rmse = rmse/nrow(outputDat)
  return(rmse)
}

get_bias_from_dat = function(outputDat, simParams, parameters){

  nParams = length(parameters)
  bias = data.frame(matrix(nrow = nrow(outputDat), ncol = nParams, data = 0))
  colnames(bias) = parameters

  for (i_seed in 1:nrow(outputDat)){
    for(parameter in parameters){

      truth = simParams[i_seed, parameter]

      parameter_bias = get_bias_rel(posterior_median = outputDat[i_seed, paste0(parameter, "_median")],
                                    truth = truth)

      bias[i_seed, parameter] = parameter_bias

    }
  }
  #bias = bias/nrow(outputDat)
  return(bias)
}
