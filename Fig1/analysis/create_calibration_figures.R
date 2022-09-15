setwd("/Users/seidels/Projects/trees-in-devBio/papers/bayesianPhylogeneticInference/Figures/Fig1/analysis/")
library(ggplot2)

font = "Arial"
plotDir = "../"
thisAnalysis = "realistic_inference_1type"
resultsDir = "../inference/inferenceOutput/"

parameters = c("treeHeight", "treeLength", "birthRate", "deathRate", "rho", paste0("scarringRate.", 1:50), "clockRate")

load(file = paste(resultsDir, thisAnalysis, ".Rdat", sep = ""))
load(file = paste(resultsDir, thisAnalysis, "_simParams.Rdat", sep = ""))

outputDat_cleaned = outputDat[which(!(is.na(outputDat$seed))), ]
simParams_cleaned = simParams[which(simParams$seed %in% outputDat_cleaned$seed), ]

create_plot = function(dat){

  p = ggplot(data = dat, aes(x=simulated, y=inferred)) +
          geom_point() +
          geom_errorbar(aes(ymin = lower, ymax=upper), alpha=0.4)+
          geom_abline(intercept = 0, slope = 1, col="darkgreen") +
          theme_bw()+
          ylab("Estimated median and 95% HPD interval")+
          xlab("True value")+
          theme(text=element_text(size = 20, family = font))

  return(p)
}


#tree height
treeheight = data.frame(simulated=simParams_cleaned$treeHeight, inferred=outputDat_cleaned$treeHeight_median,
                        upper=outputDat_cleaned$treeHeight_upper, lower=outputDat_cleaned$treeHeight_lower)

p_treeheight = create_plot(dat = treeheight) +
  geom_hline(yintercept = 16, linetype="dotted") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))

svg(filename = paste0(plotDir, "calibtrated_tree_height.svg"), bg = "transparent", family = font)
p_treeheight
dev.off()

## correlation
cor.test(treeheight$simulated, treeheight$inferred) # without height diff

treeheight_larger16 = treeheight[which(treeheight$simulated >= 16), ]
cor.test(treeheight_larger16$simulated, treeheight_larger16$inferred)

treeheight_lower16 = treeheight[which(treeheight$simulated < 16), ]
cor.test(treeheight_lower16$simulated, treeheight_lower16$inferred)

# tree length
treeLength = data.frame(simulated=simParams_cleaned$treeLength, inferred=outputDat_cleaned$treeLength_median,
                        upper=outputDat_cleaned$treeLength_upper, lower=outputDat_cleaned$treeLength_lower)
p_treelength = create_plot(dat = treeLength)
svg(filename = paste0(plotDir, "calibtrated_tree_length.svg"))
p_treelength
dev.off()

## correlation
cor.test(treeLength$simulated, treeLength$inferred) # without Length diff

treeLength_larger16 = treeLength[which(treeheight$simulated >= 16), ]
cor.test(treeLength_larger16$simulated, treeLength_larger16$inferred)

treeLength_lower16 = treeLength[which(treeheight$simulated < 16), ]
cor.test(treeLength_lower16$simulated, treeLength_lower16$inferred)

#birth rate
birthRate = data.frame(simulated=simParams_cleaned$birthRate, inferred=outputDat_cleaned$birthRate_median,
                       upper=outputDat_cleaned$birthRate_upper, lower=outputDat_cleaned$birthRate_lower)

p_birthrate = create_plot(birthRate)
svg(filename = paste0(plotDir, "calibrated_birth_rate.svg"))
p_birthrate
dev.off()

## correlation
cor.test(birthRate$simulated, birthRate$inferred) # without Length diff

birthRate_larger16 = birthRate[which(treeheight$simulated >= 16), ]
cor.test(birthRate_larger16$simulated, birthRate_larger16$inferred)

birthRate_lower16 = birthRate[which(treeheight$simulated < 16), ]
cor.test(birthRate_lower16$simulated, birthRate_lower16$inferred)

#sampling proportion
rho = data.frame(simulated=simParams_cleaned$rho, inferred=outputDat_cleaned$rho_median,
                 upper=outputDat_cleaned$rho_upper, lower=outputDat_cleaned$rho_lower)

p_rho = create_plot(rho)
svg(filename = paste0(plotDir, "calibrated_rho.svg"))
p_rho
dev.off()

cor.test(rho$simulated, rho$inferred) # without Length diff

rho_larger16 = rho[which(treeheight$simulated >= 16), ]
cor.test(rho_larger16$simulated, rho_larger16$inferred)

rho_lower16 = rho[which(treeheight$simulated < 16), ]
cor.test(rho_lower16$simulated, rho_lower16$inferred)


#scarring rates
plotDat = data.frame(
  simulationNr= c(1:nrow(outputDat_cleaned)),
  simulated = simParams_cleaned$scarringRate.1,
  inferred = outputDat_cleaned$scarringRate.1_median,
  upper = outputDat_cleaned$scarringRate.1_upper,
  lower = outputDat_cleaned$scarringRate.1_lower,
  scarPresent = simParams_cleaned$scarPresent
)
plotDat$scarPresent = factor(plotDat$scarPresent, labels =c("Scar absent", "Scar present"))

g_scars = create_plot(plotDat) +
  facet_grid(~scarPresent)

svg(filename = paste0(plotDir, "calibrated_scarring_rate.svg"), width = 16, height = 8)
g_scars
dev.off()
