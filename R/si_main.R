data_path = '../fasstr_data'
outD = 'econ3'

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

# figure models-scenario matrix
ms_data_NPi = rbind(
  readxl::read_excel(paste0(data_path,'/SI_models_scen.xlsx'), sheet = 'NPi_NZ') %>% 
    mutate(scen = 'NZ'),
  readxl::read_excel(paste0(data_path,'/SI_models_scen.xlsx'), sheet = 'NPi_EoC') %>% 
    mutate(scen = 'EoC')) %>% 
  tidyr::pivot_longer(2:7, names_to = 'model') %>% 
  mutate(cb = factor(cb, levels = c('200', '300', '400', '450', '500', '600', '700',
                                    '800', '900', '1000', '1200', '1400', '1600',
                                    '1800', '2000', '2500', '3000', 'NPi_2100')))

ggplot(ms_data_NPi, aes(x = model, y = cb, fill = !is.na(value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("white", "#4682B4"), guide = FALSE) +
  geom_text(aes(label = ifelse(!is.na(value), "x", "")), size = 6) +
  facet_wrap(. ~ scen) +
  labs(x = "Model", y = "Carbon Budget") +
  ggpubr::theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  theme(panel.spacing.x = unit(1, "cm"))

ggsave(file = 'Results/Methods/NPi_meth.pdf', width = 200, height = 150, units = 'mm')


ms_data_INDC = rbind(
  readxl::read_excel(paste0(data_path,'/SI_models_scen.xlsx'), sheet = 'INDC_NZ') %>% 
    mutate(scen = 'NZ'),
  readxl::read_excel(paste0(data_path,'/SI_models_scen.xlsx'), sheet = 'INDC_EoC') %>% 
    mutate(scen = 'EoC')) %>% 
  tidyr::pivot_longer(2:7, names_to = 'model') %>% 
  mutate(cb = factor(cb, levels = c('200', '300', '400', '450', '500', '600', '700',
                                    '800', '900', '1000', '1200', '1400', '1600',
                                    '1800', '2000', '2500', '3000', 'INDC_2100')))

ggplot(ms_data_INDC, aes(x = model, y = cb, fill = !is.na(value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("white", "#4682B4"), guide = FALSE) +
  geom_text(aes(label = ifelse(!is.na(value), "x", "")), size = 6) +
  facet_wrap(. ~ scen) +
  labs(x = "Model", y = "Carbon Budget") +
  ggpubr::theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  theme(panel.spacing.x = unit(1, "cm"))

ggsave(file = 'Results/Methods/INDC_meth.pdf', width = 200, height = 150, units = 'mm')


