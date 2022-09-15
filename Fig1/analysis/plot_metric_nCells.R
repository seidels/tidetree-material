## ---------------------------
##
## Script name: plot_metric_nCells.R
##
## Purpose of script: Generate plots that depict the metrics of TiDeTrees performance for increasing number of cells.
##
## Author: Sophie Seidel
##
## Date Created: 2022-06-20
##
## Copyright (c) Sophie Seidel, 2022
## Email: sophie.seidel@posteo.de
##
## ---------------------------

exclude = c(767, 928)


bias_all$seed = outputDat_cleaned$seed
bias_all$nCells = simParams_cleaned$nCells
bias_all = bias_all[which(!(bias_all$seed %in% exclude )),]

bias_plot = melt(bias_all[, c("treeHeight", "treeLength", "birthRate", "rho", "nCells")],
                 id.vars = "nCells" )
