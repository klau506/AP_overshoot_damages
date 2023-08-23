library(data.table)
library(ggplot2)
library(tidyverse)
library(plyr)
library(broom)
library(ggpubr)
library(ggsci)
library(desiderata)
library(grid)

load(file.path(data_path,'RDA/','econ','emi_engage.RData'))
datEmi = data.table(datEmi)
source('si_e_functions.R')
###########################################################################################
#                               DISTRIB & CUMULATIVE PLOT                                 #
###########################################################################################

# plot and save distrib+cum with all pollutants per region
doE_FIGURE_cum_distrib(datEmi)

###########################################################################################
#                                       P-VALUE PLOT                                      #
###########################################################################################

# plot and save p-value with all pollutants and all regions
doE_ks_test_plot(datEmi %>% filter(region != 'R10Shipping_Aviation'))
