# set directory
setwd("~/Desktop/Desktop - Emily’s MacBook Pro/Spring 2023/LAS Data")

library(tidyverse)
library(plyr)
library(irr)
library(compare)
library(arsenal)
library(tidyr)

# read in data file
reldat <- read.table('relLAS.txt', header=T)

newrel <- subset(reldat, select = c("prim", "sec"))

icc(newrel, model= "twoway", type = "agreement", unit = "single")