
## ---------------------------
##
## Script name: estimate_run_time.R
##
## Purpose of script: Estimate and plot
## the number of hours required for an average
## analysis of 100, 250 or 500 cells to converge.
##
## Author: Sophie Seidel
##
## Date Created: 2022-06-29
##
## Copyright (c) Sophie Seidel, 2022
## Email: sophie.seidel@bsse.ethz.ch
##
## ---------------------------

setwd("~/Projects/trees-in-devBio/papers/bayesianPhylogeneticInference/Figures/Supp/simulationStudy/runtime/")

# libs
library(tracerer)
library(RColorBrewer)
library(ggplot2)


# plot
colours = brewer.pal(n = 7, name = "Blues")[c(3, 4, 7)]
width = 10
height = 10
font = "Arial"
text_size = 20

# collect data from inferences
nCells = list.dirs(path = ".", recursive = F)

out = data.frame(nCells = rep("", 100), runTime=rep("", 100), ess= rep(0, 100), fixScarring = rep(F, 100), log=rep("", 100))
means = data.frame(nCells = rep("", 100), runTime=rep("", 100), meanEss= rep(0, 100), sdEss = rep(0,100), fixScarring = rep(F, 100))

ctr = 1
ctr_means = 1
for (x in nCells){

  runtimes = list.files(x, pattern = "*h", recursive = F)
  #runtimes = unname(sapply(runtime, function(x){strsplit(x = x, split = "/")[[1]][2]}))

  print(x)
  for (runtime in runtimes){

    print(runtime)

    log_files = list.files(path = paste0("./", x, "/", runtime, "/"), pattern = "realistic_inference_1type.[0-9]*.log")
    log_files_fix_scarring = list.files(path = paste0("./", x, "/", runtime, "/"), pattern = "realistic_inference_1type_fixScarring.[0-9]*.log")

    esses = rep(0, 20)
    for (i in 1:length(log_files)){
      log_file = log_files[i]
      log = read.delim(paste0("./", x, "/", runtime, "/",log_file))

      # remove burn in
      n = nrow(log)
      burn_in_10 = round(0.1 * n, digits = 0)
      log = log[burn_in_10:n, ]

      ess = calc_ess(log$posterior, sample_interval = 1)
      esses[i] = ess
      out[ctr, ] = c(x, runtime, ess, F, log_file)

      ctr = ctr + 1
    }

    # update table of means
    mean_ess = mean(esses) # always 20 log files
    sd_ess = sd(esses)
    means[ctr_means, ] = c(x, runtime, mean_ess, sd_ess, F)
    ctr_means = ctr_means + 1

    esses = rep(0, 20)
    if (length(log_files_fix_scarring) == 0){
      next()
    }else{
      for (i in 1:length(log_files_fix_scarring)){
        log_file = log_files_fix_scarring[i]
        log = read.delim(paste0("./", x, "/", runtime, "/",log_file))

        # remove burn in
        n = nrow(log)
        burn_in_10 = round(0.1 * n, digits = 0)
        log = log[burn_in_10:n, ]

        ess = calc_ess(log$posterior, sample_interval = 1)
        esses[i] = ess
        out[ctr, ] = c(x, runtime, ess, T, log_file)
        ctr = ctr + 1
      }
      # update table of means
      mean_ess = mean(esses) # always 20 log files
      sd_ess = sd(esses)
      means[ctr_means, ] = c(x, runtime, mean_ess, sd_ess, T)
      ctr_means = ctr_means + 1
    }

   }
}

# post processing
out$runTime = unlist(strsplit(out$runTime, split = "h"))
out$runTime = as.numeric(out$runTime)

means = means[which(!(means$nCells == "")),]
means$runTime = unlist(strsplit(means$runTime, split = "h"))
means$runTime = as.numeric(means$runTime)
means$meanEss = as.numeric(means$meanEss)
means$sdEss = as.numeric(means$sdEss)
means$tree_size = as.numeric(unname(sapply(means$nCells, function(x){strsplit(x = x, split = "./chains_")[[1]][2]})))


# plot
p = ggplot(data=subset(means, fixScarring==F & (nCells != "./chains_500_WBsubTrees"  & nCells != "./chains_500_WBsubTrees_adaptedWeights")
                                                ), aes(x=runTime, y=meanEss, col=as.factor(tree_size)))+
  geom_point() +
  geom_errorbar(aes(ymin = meanEss - 0.5 * sdEss, ymax = meanEss + 0.5 * sdEss)) +
  geom_hline(yintercept = 200, col = "darkgreen", linetype = "dashed")+
  theme_bw() +
  scale_color_discrete(name="Tree size")+
  #scale_color_continuous(trans = "reverse")+
  #scale_color_manual(values = colours, name="Tree size")+
  xlab("Run time in hours") +
  ylab("Effective sample size")+
  theme(text = element_text(family = font, size = text_size),
        legend.position = c(0.9, 0.9))

p
svg(filename = "../runtime.svg", width = width, height = height, family = font)
p
dev.off()


