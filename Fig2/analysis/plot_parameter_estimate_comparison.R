library(ggplot2)
library(reshape2)
library(wesanderson)

font = "Arial"

params = c("treeHeight", "treeLength")
#params = c("birthRate", "deathRate")
params=c("growthRate", "turnover")
# input
metricDir = "../metrics/"
subDirs = c( "baseline/",
            "fixScarring/",
            "fixScarring_10alignments/")
map_subdir_inference = data.frame("A", "B", "C")
names(map_subdir_inference) = subDirs
metrics = c("Bias", "RMSE", "HPD", "Coverage")

# output
combined = data.frame(Bias=0, RMSE=0, HPD = 0, Coverage=0, inference="", param="")
sds = data.frame(Bias_sd=0, RMSE_sd=0, HPD_sd = 0, Coverage_sd=0, inference="", param="")

plotDir = "../plots/"

# plot settings
colors = wes_palette("Darjeeling2", n = 5, type = "discrete")[2:4]
ctr = 1
for (param_of_interest in params){
  for (subDir in subDirs){

    # add the parameter names before the .1 gets added
    combined[ctr, "param"] = param_of_interest
    sds[ctr, "param" ] = param_of_interest

    if ((( subDir == "fixScarring_10alignments/") | (subDir == "fixScarring_10alignments_unfinished/")) &
        startsWith(param_of_interest, prefix = "tree")){
      param_of_interest = paste0(param_of_interest, ".0")
    }

    for (metric in metrics){
      dat = readRDS(file = paste0(metricDir, subDir, metric, ".Rds"))
      combined[ctr, metric] = dat[1, param_of_interest]
      combined[ctr, "inference"] = map_subdir_inference[subDir]


      sds[ctr, paste0(metric, "_sd")] = dat[2, param_of_interest]
      sds[ctr, "inference"] = map_subdir_inference[subDir]
    }
    ctr = ctr +1
  }
}
#combined$variable = "mean"
#sds$variable = "sd"
sds_melted = melt(sds, variable.name = "sds", value.name = "sds_values")
combined_melted = melt(combined, id.vars = c("inference", "param"))

combined_melted = cbind(combined_melted, sds_melted[, 3:4])

# height = combined
# height$param = "Height"
# combined$param = "Length"
# combined = rbind(height, combined)
# combined_melted = melt(combined, id.vars = c("inference", "param"))
#

# melted = melt(combined, id.vars = c("inference"))
combined_melted$optimal = as.numeric(combined_melted$variable == "Coverage")

#sub = melted[which(melted$parameter==param_of_interest & melted$variable==metric_of_interest), ]
#sub = melted[which(melted$variable==metric_of_interest), ]

p = ggplot(combined_melted, aes(x=inference, y=value, col=inference))+
  facet_wrap(~variable, scales = "free")+
  geom_hline(aes(yintercept = optimal), linetype="dashed") +
  geom_point(aes(shape=param), position = position_dodge(width = 0.9), size=6)+
  geom_errorbar(aes(ymin=(value- 0.5*sds_values), ymax=(value + 0.5*sds_values)),
                position = position_dodge2(width = 0.1))+
  #geom_point( position = position_dodge(width = 0.5), size=6)+
  xlab("") + ylab("")+
  theme_bw()+ scale_color_manual(values = colors, guide="none")+
  theme(legend.position = "none", text = element_text(size = 20, family = font))

  p
#svg(filename = paste0(plotDir, "metricsAcrossInferences_phylodynamicParameters", ".svg"), family = font )
#svg(filename = paste0(plotDir, "metricsAcrossInferences_treeParameters", ".svg"),family = font )
svg(filename = paste0(plotDir, "metricsAcrossInferences_growthTurnover", ".svg"), family = font )
p
dev.off()

