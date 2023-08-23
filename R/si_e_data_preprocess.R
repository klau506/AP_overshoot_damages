# EMISSIONS data preprocess: create columns from the info extracted from the
# scen_model_complete name.
# Data: ENGAGE DB (r10 regions)
library(data.table)
library(tidyverse)

#########################################################################################
#                                       IMPORT FILES                                    #
#########################################################################################

datEmi = read.csv('../dat_eng.csv')

#########################################################################################
#                                     DATA PRE-PROCESS                                  #
#########################################################################################

# group carbon budgets
datEmi <- datEmi %>%
  mutate(cb_group = case_when(
    datEmi$carbon_budget < 1000 ~ "<1000",
    datEmi$carbon_budget %in% (1000:2000) ~ "[1000,2000]",
    datEmi$carbon_budget %in% (2500:5000) ~ ">2000"
  ))
datEmi$cb_group <- factor(datEmi$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

datEmi[datEmi == "Other (R10)"] = 'ShippingAviation'
datEmi = data.table(datEmi[,-1]) %>%
  dplyr::rename('region' = 'REGION')

# account for the world emissions (only r10, not shipping+aviation)
datEmi_world = datEmi[region != "ShippingAviation", list(value=sum(value,na.rm=TRUE)),
                      by=c('year','pollutant','policy','scenario','carbon_budget',
                           'cb_group','model','scen_complete_name','unit')]
datEmi_world$region = 'WORLD'

# add world emissions as a new region
datEmi_tmp = rbind(datEmi,datEmi_world)
rm(datEmi,datEmi_world)

# consider BC+OC
datEmi_BC_OC = datEmi_tmp[pollutant %in% c('BC','OC'), list(value=sum(value,na.rm=TRUE)),
                          by=c('year','policy','scenario','carbon_budget','region',
                               'cb_group','model','scen_complete_name','unit')]
datEmi_BC_OC$pollutant = 'BC+OC'

# add BC+OC emissions as a new pollutant
datEmi_all = rbind(datEmi_tmp,datEmi_BC_OC)
rm(datEmi_tmp,datEmi_BC_OC)

# consider only some pollutants
datEmi_all[datEmi_all == "Sulfur"] = 'SO2'
datEmi_all = datEmi_all[pollutant %in% c('BC+OC','NOx','SO2','VOC')]

# save it to do tests and plots
datEmi = datEmi_all
rm(datEmi_all)
save(datEmi,file=file.path(data_path,'RDA/','econ','emi_engage.RData'))
