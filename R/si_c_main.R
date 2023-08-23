library(data.table)
library(ggplot2)
library(tidyverse)
library(plyr)
library(broom)
library(ggpubr)
library(ggsci)
library(desiderata)
library(grid)

version = 'av' # or 'bypx
load(file.path(data_path,'RDA/','econ',paste0('conc_fasstr_',version,'.RData')))
datConc = data.table(datConc)
source('si_c_functions.R')
###########################################################################################
#                               DISTRIB & CUMULATIVE PLOT                                 #
###########################################################################################

# plot and save distrib+cum with all pollutants per region
doC_FIGURE_cum_distrib(datConc)

###########################################################################################
#                                       P-VALUE PLOT                                      #
###########################################################################################

# plot and save p-value with all pollutants and all regions
doC_ks_test_plot(datConc)