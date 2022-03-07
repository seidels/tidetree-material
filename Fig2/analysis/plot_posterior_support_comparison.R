library(ggplot2)
library(ggridges)
library(wesanderson)

# plot settings
colors = wes_palette("Darjeeling2", n = 5, type = "discrete")[2:4]
plotDir = "../plots/"
font = "Arial"

dirs = c(
  "../inference/inferenceOutput/baseline/",
  "../inference/inferenceOutput/fixScarring/",
  "../inference/inferenceOutput/fixScarring_10alignments/"
)

dat1 = readRDS(file = paste0(dirs[1], "posteriorSupportAtNodes.RDS"))
dat1 = dat1[which(dat1$seed <= 100), ]
dat1$setup = "A"

dat2 = readRDS(file = paste0(dirs[2], "posteriorSupportAtNodes.RDS"))
dat2$setup = "B"
dat3 = readRDS(file = paste0(dirs[3], "posteriorSupportAtNodes.RDS"))
dat3$setup = "C"


means = c(mean(as.numeric(dat1$meanPosteriorAb16), na.rm = T),
          mean(dat2$meanPosteriorAb16, na.rm = T),
          mean(dat3$meanPosteriorAb16, na.rm = T))

sds =  c(mean(dat1$meanPosteriorAb16, na.rm = T)/sqrt(nrow(dat1)),
         mean(dat2$meanPosteriorAb16, na.rm = T)/sqrt(nrow(dat2)),
         mean(dat3$meanPosteriorAb16, na.rm = T)/sqrt(nrow(dat3)))

dat = data.frame(setup = c("A", "B", "C"),
                 means = means,
                 sds = sds)

g = ggplot(dat, aes(x=setup, y=means, fill=setup, col=setup))+
  scale_fill_manual(values = colors)+
  scale_color_manual(values=colors)+
  geom_point(size=10)+
  geom_errorbar(aes(ymin=means-0.5*sds, ymax=means+0.5*sds))+
  theme_bw() + ylab("Mean posterior support")+
  xlab("")+theme(legend.position = "None", text = element_text(size = 20))

ggsave(plot = g, filename = paste0(plotDir, "meanPosteriorSupportCompared.pdf"))
svg(filename =paste0(plotDir, "meanPosteriorSupportCompared.svg"), height = 8, width = 3.85)
g
dev.off()


dat = rbind(dat1, dat2, dat3)

g_mean = ggplot(dat, aes(x=setup, y=meanPosteriorAb16, fill=setup)) +
  #geom_boxplot()+ geom_jitter(shape=16, position=position_jitter(0.2))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_manual(values = colors)+
  theme_bw() + ylab("Mean posterior support")+
  xlab("") + theme(legend.position = "None", text = element_text(size = 20))

g_mean
svg(filename =paste0(plotDir, "meanPosteriorSupport.svg"), family = font)
g_mean
dev.off()

min1 = readRDS(file = paste0(dirs[1], "minSupportAtNodes.RDS"))
min1 = min1[which(min1$seed <= 100), ]
min1$setup = "A"

min2 = readRDS(file = paste0(dirs[2], "minSupportAtNodes.RDS"))
min2$setup = "B"
min3 = readRDS(file = paste0(dirs[3], "minSupportAtNodes.RDS"))
min3$setup = "C"

min = rbind(min1, min2, min3)

g_min = ggplot(min, aes(x=setup, y=minPosteriorAb16, fill=setup)) +
  geom_violin()+
  #geom_boxplot()+ geom_jitter(shape=16, position=position_jitter(0.2))+
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_manual(values = colors)+
  theme_bw() + ylab("Minimum posterior support")+
  xlab("")+theme(legend.position = "None", text = element_text(size = 20))

g_min
svg(filename =paste0(plotDir, "minPosteriorSupport.svg"))
g_min
dev.off()

means = c(mean(min1$minPosteriorAb16, na.rm = T),
          mean(min2$minPosteriorAb16, na.rm = T),
          mean(min3$minPosteriorAb16, na.rm = T))

sds =  c(mean(min1$minPosteriorAb16, na.rm = T)/sqrt(nrow(min1)),
         mean(min2$minPosteriorAb16, na.rm = T)/sqrt(nrow(min2)),
         mean(min3$minPosteriorAb16, na.rm = T)/sqrt(nrow(min3)))

dat = data.frame(setup = c("A", "B", "C"),
                 means = means,
                 sds = sds)

g = ggplot(dat, aes(x=setup, y=means, fill=setup, col=setup))+
  scale_fill_manual(values = colors)+
  scale_color_manual(values=colors)+
  geom_point(size=10)+
  geom_errorbar(aes(ymin=means-0.5*sds, ymax=means+0.5*sds))+
  theme_bw() + ylab("Minimum posterior support")+
  xlab("")+theme(legend.position = "None", text = element_text(size = 20))

ggsave(plot = g, filename = paste0(plotDir, "minPosteriorSupportCompared.pdf"))
svg(filename =paste0(plotDir, "minPosteriorSupportCompared.svg"), height = 8, width = 3.85)
g
dev.off()
