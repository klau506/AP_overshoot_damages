# CONCENTRATIONS data preprocess: create columns from the info extracted from the
# scen_model_complete name.
# NOTICE: it can be created a RData with the concentrations' average per model_scen,
# or either with all the GIS data, that is, for each grid cell.
# Data: concentrations computed by FASST
library(data.table)
library(tidyverse)
library(stringr)
library(strex)
library(gridExtra)
library(desiderata)

#########################################################################################
#                                       IMPORT FILES                                    #
#########################################################################################
load(file.path(data_path,'RDA/','econ','mask3618.RData'))

folder = 'Conc_Data'

# apply the mask using all the data
apply_mask_bypx = function(qconc) {
  r10conc=data.table()
  for(rr in nmap$nn){
    print(rr)
    maks=apply(SMALL_MASK, 2, rev)
    maks[maks!=rr]=0
    maks[maks==rr]=1
    mask_applied=qconc$tot*maks
    r10conct=data.table(value=(mask_applied[mask_applied>0]),n=nmap[nn==rr]) # if all data
    r10conc = rbind(r10conc,r10conct)
  }
  names(r10conc) <- c('value','nn','region')
  return(r10conc)
}

# apply the mask considering the average
apply_mask_mean = function(qconc) {
  r10conc=data.table()
  for(rr in nmap$nn){
    print(rr)
    maks=apply(SMALL_MASK, 2, rev)
    maks[maks!=rr]=0
    maks[maks==rr]=1
    mask_applied=qconc$tot*maks
    r10conct=data.table(value=mean(mask_applied[mask_applied>0]),n=nmap[nn==rr]) # if average
    r10conc = rbind(r10conc,r10conct)
  }
  names(r10conc) <- c('value','nn','region')
  return(r10conc)
}


preprocess = function(model_scen) {
  
  # extract policy
  policy <- str_extract(model_scen,'INDC|NPi|NoPolicy')
  
  # extract scenario
  temp = str_after_nth(str_before_first(model_scen,'-'),'_', 2)
  temp = temp |> replace_na('REF')
  scenario = as.character(temp)
  scenario <- ifelse(endsWith(scenario, "f"), 'EoC',
                     ifelse(endsWith(scenario, "0") & !endsWith(scenario, "2100"), "NZ",'REF'))
  
  # extract model
  model = str_after_first(model_scen, '-')
  
  # extract carbon budget
  temp = as.numeric(gsub("([0-9]+).*$", "\\1", temp))
  temp = temp |> replace_na(5000)
  carbon_budget = temp
  
  # group carbon budgets
  cb_group <- ifelse(carbon_budget < 1000, "<1000",
                     ifelse(carbon_budget %in% (1000:2000), "[1000,2000]",
                            ifelse(carbon_budget %in% (2500:5000), ">2000")))
  
  concfield = data.frame(model_scen, policy,scenario,model,carbon_budget,cb_group)
  concfield = data.table(concfield)
  
  return(concfield)
}

importConc = function(file,withAv) {
  load(file.path('Data/',folder,file))
  ms = str_extract_all(file,"(?<=concfield_).+(?=.RData)")
  ms = as.character(ms[1])
  
  # restrict the plot to the pollutants PM25 and O3
  conc_susbset_o3 <- subset(concfield, aaqe == 'DO3')
  conc_susbset_o3 = data.frame(conc_susbset_o3)
  conc_susbset_o3$aaqe = 'O3'
  names(conc_susbset_o3) <- c("nb","aaqe","year","tot")
  
  c_pm = concfield %>%
    pivot_wider(names_from = aaqe, values_from = tot)
  c_pm = data.table(c_pm)
  c_pm$TOT_PM25 = c_pm$DNH4 + c_pm$DNO3_A + c_pm$DSO4 + c_pm$DBC +   #bc_enh
    c_pm$DPOM +                                        #pom_enh
    c_pm$dupm25 + c_pm$sspm25 +                        #natural pm
    c_pm$pmsec +                                       #secondary pm
    c_pm$h2o_35                                        #athmosphere impact
  conc_susbset_pm25 = data.frame(c_pm$nb, c_pm$t, c_pm$TOT_PM25)
  names(conc_susbset_pm25) <- c("nb","year","tot")
  conc_susbset_pm25$aaqe = 'PM25'
  
  for (y in c(2030,2050)) {
    if (withAv) {
      qconc_pm25 <- apply_mask_mean(subset(conc_susbset_pm25, year == y))
      qconc_pm25$pollutant = 'PM25'
      
      qconc_o3 <- apply_mask_mean(subset(conc_susbset_o3, year == y))
      qconc_o3$pollutant = 'O3'
    } else {
      qconc_pm25 <- apply_mask_bypx(subset(conc_susbset_pm25, year == y))
      qconc_pm25$pollutant = 'PM25'
      
      qconc_o3 <- apply_mask_bypx(subset(conc_susbset_o3, year == y))
      qconc_o3$pollutant = 'O3'
    }
    
    qconc = rbind(qconc_pm25,qconc_o3)
    qconc$year = y
    
    kl = preprocess(ms)
    datkl = cbind(kl, qconc)
    datkl$nn = NULL
    
    # account for the world emissions (only r10, not shipping+aviation)
    datkl_world = datkl[, list(value=mean(value,na.rm=TRUE)),
                        by=c('year','policy','scenario','carbon_budget',
                             'cb_group','model_scen','pollutant','model')]
    datkl_world$region = 'WORLD'
    
    # add world emissions as a new region
    datklall = rbind(datkl,datkl_world)
    
    datConc = rbind(datConc,datklall)
  }
  
  return(datConc)
}

###############################################################################

list_files = as.list(list.files(path = paste('Data',folder,sep='/'),
                                pattern = "concfield_", recursive = TRUE))
t0 = Sys.time()
datConc = data.frame()
for (f in list_files[1:length(list_files)]) {
  print(f)
  datConc = importConc(f,F)
}

datConc = data.table(datConc)
print_log(Sys.time()-t0)
save(datConc,file=file.path('Data','conc_fasstr_bypx.RData'))

t0 = Sys.time()
datConc = data.frame()
for (f in list_files[1:length(list_files)]) {
  print(f)
  datConc = importConc(f,T)
}

datConc = data.table(datConc)
print_log(Sys.time()-t0)
save(datConc,file=file.path(data_path,'RDA/','econ','conc_fasstr_av.RData'))