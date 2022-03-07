library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(wesanderson)

# plot config
plotDir = "./"
mycolors = rep(wes_palette(n=4, name="GrandBudapest1")[c(1,2,4)], each=2)
width = 8
height = 8
unit = "cm"
font = "Arial"

#load posterior
logF = "../2_inference/inference_logs/priorInfoOnScarring/all_trees_rho_origin_reparScarClock_treeInit25_PriorOnScarringRatesI_scarringStart_clock_ExpPriorScarringStart_combined.log"

posterior = read.delim(file = paste0(logF), header = T)

print("report posterior median and 95% HPD interval for the birth rate:")
round(quantile(posterior$birthRate, c(0.05, 0.5, 0.95)), digits = 3)
print("report posterior median and 95% HPD interval for the death rate:")
round(quantile(posterior$deathRate, c(0.05, 0.5, 0.95)), digits = 3)


n = nrow(posterior)
posterior$growthRate = posterior$birthRate - posterior$deathRate
posterior$turnover = posterior$deathRate/ posterior$birthRate

# add prior distr data points
set.seed(6)
#posterior$birthPrior = rlnorm(n = n, meanlog = 0, sdlog = 0.6)
#posterior$deathPrior = rlnorm(n=n, meanlog = -2, sdlog = 1.4)
posterior$growthPrior = runif(n = n, 0, 0.1)
posterior$turnoverPrior = runif(n = n, 0, 1)


#plot
dat = melt(posterior[, c("growthPrior", "growthRate", "turnoverPrior", "turnover", "birthRate", "deathRate")])
g = ggplot(dat, aes(x=variable, y=value, fill=variable, dist="lnorm")) +
  geom_violin(draw_quantiles = c(0.05, 0.95))+
  scale_fill_manual(values = mycolors) +
  #ylim(0,0.4)+
  theme_classic() +
  xlab("")+
  theme(legend.position = "none")+
  ylab("density")+
  theme(text=element_text(size = 20, family = font),
        axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.65, ))

g

#save
svg(paste0(plotDir, "phylodynamicEstimates.svg"), family = font)
g
dev.off()

# ggsave(plot = g, filename = paste0(plotDir, "phylodynamicEstimates.pdf"),
#        units = unit,
#        dpi = 300,
#        width = width, height = height )

