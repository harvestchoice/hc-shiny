#####################################################################################
# Title:   HarvestChoice CELL5M Data Validation Tests
# Date:    July 2015
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

setwd("/home/projects/shiny/tmp")

library(data.table)
library(Factoshiny)

# Load sample data from GHS 2012
load("./nga_bmgf_seg_1.RData")