data_path = '../fasstr_data'
outD = 'econ'

library(data.table)
library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(plyr)
library(broom)
library(ggpubr)
library(ggsci)
library(Matching)
library(desiderata)
library(ggnewscale)
library(FedData)
library(strex)
library(binom)

# Styles
source('zzz.R')


# SI emissions figures
source('si_e_main.R')

# SI concentrations figures
source('si_c_main.R')

# SI mortality figures
source('si_m_main.R')

# SI economy figures
source('si_econ_main.R')

# SI uncertainty figures
source('si_uncert_main.R')