library(ggplot2)
library(reshape2)

# plotting setup
plotDir = "."
width = 8
height = 8
font = "Arial"

#useful functions
recovered = function(true, upper, lower){
  return( (true <= upper) & (true >= lower))
}

# true tree data
load(file = "../1_preprocessing/treeDat.Rdat")
dat$colony = paste0(dat$colony, ".txt")

# load inference data from setup where scarring start varies
#logFile = "../2_inference/inference_logs/priorInfoOnScarring/all_trees_rho_origin_reparScarClock_treeInit25_PriorOnScarringRatesI_scarringStart_clock_ExpPriorScarringStart_combined.log"
logFile = "../2_inference/inference_logs/unsupervised_siteDep/all_trees_rho_origin_dreamChallengeRange.combined.log"
log = read.delim(logFile)

# compute posterior stats for tree heights
treeHeightDat = log[, startsWith(x = colnames(log), prefix = "treeHeight")]
colonies = as.character(sapply(colnames(treeHeightDat), function(x){ strsplit(x, split = "treeHeight.t_", fixed = T)[[1]][2]}))

summaryStats = as.data.frame(t(apply(treeHeightDat, MARGIN = 2, function(x){quantile(x, c(0.05, 0.5, 0.95))})))
colnames(summaryStats) = c("HPD_lower", "median", "HPD_upper")
summaryStats$colony = colonies
#load(file = paste0(dir, "statistics.Rdat"))
mergedStats = merge(x = summaryStats, dat, by="colony" )

# coverage
rec = recovered(true = mergedStats$treeHeight, lower=mergedStats$HPD_lower, upper = mergedStats$HPD_upper)
coverage = round(sum(rec)/length(rec), digits =2)
coverage

# coverage with hour resolution
rec = recovered(true = mergedStats$treeHeight,
                lower = round(mergedStats$HPD_lower, digits = 0),
                upper = round(mergedStats$HPD_upper, digits = 0))
coverage = round(sum(rec)/length(rec), digits =2)
coverage

#plot
mergedStats = mergedStats[order(mergedStats$treeHeight), ]
mergedStats$treeNumber=1:nrow(mergedStats)

g = ggplot(mergedStats, aes(x=treeNumber, y=median)) +
  geom_point()+
  geom_errorbar(aes(ymin=HPD_lower, ymax=HPD_upper), alpha=0.4) +
  geom_point(aes(y=treeHeight), col="darkgreen")+
  theme_minimal()+
  xlab("Colony number") +
  ylab("Estimated tree height | median and HPD ")+
  ylim(0,55)+
  theme(axis.title = element_text(size=20), text = element_text(size = 15, family =font))
  #scale_x_discrete("colonies", labels  = statistics$colony_id)+
g

svg(filename = paste0(plotDir, "treeheight.svg"),
    width = width,
    height = height, family = font)
g
dev.off()
