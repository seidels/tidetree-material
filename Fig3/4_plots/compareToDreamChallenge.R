library(ggplot2)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(rcartocolor)

nColor <- 8


plotDir = "../3_plots/"

mycolors =carto_pal(n = nColor, "Safe")[c(1,2,4,3,5:8)]

#grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

width = 8
height = 8
unit = "cm"
font = "Arial"


scoresF = "../dat/dreamChallengeScoresRed.csv"
scores = read.csv(scoresF, stringsAsFactors = F)
scores[10, ] = c(9, "13.1.22", "TiDeTree", 0.59, 0.57, "?")
scores[11, ] = c(10, "12.1.21", "AMbeRland*", 0.52, 0.52, "?")

scores[1:5,3] = c("Cassiopeia", "Guan Lab", "Jasper06", "pRennert", "RnILabs")
scores[7, 3] = "WhatATeam"

scores$RF_average = as.numeric(scores$RF_average)
scores$Triples_average = as.numeric(scores$Triples_average)

scores$sum = scores$RF_average + scores$Triples_average
scores= scores[order(scores$sum), ]
scores = scores[1:8, ]

scores$submitterId = factor(scores$submitterId, levels = scores$submitterId)
scores$shape = scores$submitterId == "TiDeTree"

g = ggplot(scores, aes(y=Triples_average, x=RF_average, col=submitterId, fill=submitterId))+
    geom_point(aes(shape=shape), size=5)+
    theme_bw()+ scale_color_manual(values = mycolors)+
    scale_shape_manual(values = c(3,23))+
    xlim(0.49,0.9) + ylim(0.49,0.9)+
    theme(legend.position = "top")+
    ylab("Mean RF distance") +
    xlab("Mean triplet distance")+
    theme(legend.title = element_blank(),
        legend.text = element_text(size = 15),
        text = element_text(size = 20, family = font), panel.grid.minor = element_blank())+
    guides(shape="none")
g

svg(filename = paste0(plotDir, "topologyRanking.svg"),
    width = width, height = height, family = font)
g
dev.off()

#ggsave(plot = g, filename = paste0(plotDir, "topologyRanking.pdf"),
#       width = width, height = height)
#g

