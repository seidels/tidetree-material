## ---------------------------
##
## Script name: compareToDreamChallenge.R
##
## Purpose of script: Compare TiDeTree's topology score
##  to methods from the DREAM challenge
##
## Author: Sophie Seidel
##
## Date Created: 2022-08-17
##
## Copyright (c) Sophie Seidel, 2022
## Email: sophie.seidel@posteo.de
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## set working directory for Mac

setwd("~/Projects/trees-in-devBio/papers/bayesianPhylogeneticInference/Figures/Fig3/3_analysis/")

## ---------------------------

## load up the packages we will need:  (uncomment as required)

library(ggplot2)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(rcartocolor)

## ---------------------------

## figure settings

nColor <- 8
width = 8
height = 8
unit = "cm"
font = "Arial"
text_size = 12
mycolors_ =carto_pal(n = nColor, "Safe")[c(1,4,4,4,3,2,5,6)]

plotDir = "../4_plots/"

## ---------------------------

## collect scores
scoresF = "../dat/dreamChallengeScoresRed.csv"
scores = read.csv(scoresF, stringsAsFactors = F)
scores[10, ] = c(9, "13.1.22", "TiDeTree-c", 0.55, 0.55, "?") # from scores.tsv in 3_analysis
scores[11, ] = c(10, "12.1.21", "AMbeRland*", 0.52, 0.52, "?")
scores[12, ] = c(11, "13.1.22", "TiDeTree-b", 0.55, 0.55 , "?") # from scores.tsv in 3_analysis
scores[13, ] = c(12, "13.1.22", "TiDeTree-a", 0.56, 0.57 , "?") # from scores.tsv in 3_analysis
scores[1:5,3] = c("Cassiopeia", "Guan Lab", "Jasper06", "pRennert", "RnILabs")
scores[7, 3] = "WhatATeam"

scores$RF_average = as.numeric(scores$RF_average)
scores$Triples_average = as.numeric(scores$Triples_average)

## sort by the sum of the metric (the smaller their sum the better)
scores$sum = scores$RF_average + scores$Triples_average
scores= scores[order(scores$sum), ]
scores = scores[1:8, ]

## ---------------------------

## create different shape for TiDeTree
scores$submitterId = factor(scores$submitterId, levels = scores$submitterId)
scores$shape = (startsWith(x = as.character(scores$submitterId), "TiDeTree"))
scores$shape[2:4] = c(1,2,3)

## create plot

g = ggplot(scores, aes(y=Triples_average, x=RF_average, col=submitterId, fill=submitterId))+
    geom_point(aes(shape=as.factor(shape)), size=5)+
    theme_classic()+
    scale_color_manual(values = mycolors_)+
    scale_fill_manual(values = mycolors_)+
    scale_shape_manual(values = c(3,22,23,24))+
    xlim(0.49,0.9) + ylim(0.49,0.9)+
    theme(legend.position = c(0.8, 0.7))+
    ylab("Mean RF distance") +
    xlab("Mean triplet distance")+
    theme(legend.title = element_blank(),
        legend.text = element_text(size = text_size),
        text = element_text(size = text_size, family = font), panel.grid.minor = element_blank())+
    guides(shape="none")
g

svg(filename = paste0(plotDir, "topologyRanking_supp.svg"),
    width = width, height = height, family = font,)
g
dev.off()

#ggsave(plot = g, filename = paste0(plotDir, "topologyRanking.pdf"),
#       width = width, height = height)
#g

