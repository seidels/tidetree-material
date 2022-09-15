## ---------------------------
##
## Script name: plot_cell_number.R
##
## Purpose of script: Plot the distribution
## of the number of cells contained in the 106 alignments
## of the DREAM challenge data set.
##
## Author: Sophie Seidel
##
## Date Created: 2022-06-21
##
## Copyright (c) Sophie Seidel, 2022
## Email: sophie.seidel@posteo.de
##
## ---------------------------

# libs
library(ggplot2)

# img settings
font = "Arial"
width = 8
height = 8
unit = "cm"
text_size = 20

# read dat and plot
dat = readRDS(file = "../../../Fig3/1_preprocessing/correctedMergedInput.Rds")

p = ggplot(dat, aes(x=nCells))+
  geom_histogram(binwidth = 1)+
  theme_bw()+
  theme(text = element_text(family = font, size = text_size))+
  xlab("Number of cells per alignment")+ ylab("Count")

p
# save plot
svg(filename = "../number_of_cells.svg", width = width, height = height, family = font)
p
dev.off()
