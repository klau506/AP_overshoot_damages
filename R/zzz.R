shortpal.scen = c('EoC'='#EF2424',
                  'NZ'='#2465EF')
regions.colors = c('#c2b280','#29AB87','#FF7F50','#FFD700','#228B22','#4682B4','#191970',
                   '#8FBC8F','#000000','#800000','#87CEEB')
longpal_impfun_colors = c('EoC.hi'='#040445',
                          'NZ.hi'='#128043',
                          'EoC.med'='#000080',
                          'NZ.med'='#43CD80',
                          'EoC.lo'='#7676C8',
                          'NZ.lo'='#77F7B0')
longpal_impfun_colors_th = c('EoC.95th'='#040445',
                         'NZ.95th'='#128043',
                         'EoC.50th'='#000080',#F85757',
                         'NZ.50th'='#43CD80',#3E6DD8',EF2424
                         'EoC.5th'='#7676C8',#F38858,FF9200
                         'NZ.5th'='#77F7B0')#3ED8D7,0BD5CB

longlabs.impfunCI = c('EoC.95th' = 'Eoc, 95% CI','NZ.95th' = 'NZ, 95% CI',
                      'EoC.50th' = 'EoC, 50% CI','NZ.50th' = 'NZ, 50% CI',
                      'EoC.5th' = 'EoC, 5% CI','NZ.5th' = 'NZ, 5% CI')
breaks.impfunCI = c('EoC.95th','NZ.95th','EoC.50th','NZ.50th','EoC.5th','NZ.5th')

longpal_linetype = c('EoC.hi'='dashed',
                      'NZ.hi'='dashed',
                      'EoC.med'='solid',#F85757',
                      'NZ.med'='solid',#3E6DD8',EF2424
                      'EoC.lo'='dotted',#F38858,FF9200
                      'NZ.lo'='dotted')#3ED8D7,0BD5CB
longpal_alpha_colors = c('EoC.hi'='#040445',
                         'NZ.hi'='#128043',
                         'EoC.med'='#000080',#F85757',
                         'NZ.med'='#43CD80',#3E6DD8',EF2424
                         'EoC.lo'='#7676C8',#F38858,FF9200
                         'NZ.lo'='#77F7B0')#3ED8D7,0BD5CB
longlabs.alphaCI = c(expression('EoC, high elasticity'),expression('NZ, high elasticity'),
                     expression('EoC, medium elasticity'),expression('NZ, medium elasticity'),
                     expression('EoC, low elasticity'),expression('NZ, low elasticity'))
# longlabs.alphaCI = c(expression('EoC,' ~ alpha ~ 'high'),expression('NZ, ' ~ alpha ~ 'high'),
#                      expression('EoC,' ~ alpha ~ 'medium'),expression('NZ, ' ~ alpha ~ 'medium'),
#                      expression('EoC,' ~ alpha ~ 'low'),expression('NZ, ' ~ alpha ~ 'low'))
shortpal_linetype = c('5th %CI'='dotted',
                      '50th %CI'='solid',
                      '95th %CI'='44',
                      'No ZCF\nuncertainty'='3313')
shortpal_linetype.labs = c('5th %CI','50th %CI','95th %CI','No ZCF\nuncertainty')
                      
alpha.linetype = c('hi'='dashed',
                   'med'='solid',
                   'lo'='dotted')
# alpha.labs = c(expression(alpha ~ ' high'),expression(alpha ~ ' medium'),expression(alpha ~ ' low'))
alpha.labs = c('high','medium','low')
names(alpha.labs) <- c("hi", "med", "lo")

quantile.labs = c('5th %CI','50th %CI','95th %CI')
names(quantile.labs) <- c("v05", "vmed", "v95")

scen.colors = c('EoC'='#C3423F',
                 'NZ'='#5BA1EB')
scen.labs = c('EoC','NZ')
scenario.colors = c('EoC'='navy',
                    'NZ'='seagreen3')
scenario.labs = c('EoC','NZ')

method_av.labs <- c("Dong et al. \n (2021)", "Dechezlepretre \n et al. (2019)", "VSL \n", 'HCL\n')
names(method_av.labs) <- c("dong_damage_avoided", "dech_damage_avoided", "vsl_damage_avoided", 'hcl_damage_avoided')

impact_function_group.labs <- c("GBD (high) (2015)", "GBD (low) (2015)", "GBD (medium) (2015)",
                                "GBD (2015)", "Jerrett et al. (2009)", "Burnett et al. (2014)",
                                "Krewski et al. (2009)", "Cohen et al. (2005)", "Burnett et al.\n(without) (2018)",
                                "Burnett et al.\n(with) (2018)")
names(impact_function_group.labs) <- c("PM25MORT_GBD2016_HI", "PM25MORT_GBD2016_LO", "PM25MORT_GBD2016_MED",
                                       "O3MORT_GBD2015_UNI", "O3MORT_JERRET2009_UNI", "PM25MORT_BURNETT2014_UNI",
                                       "PM25MORT_KREWSKI2009_UNI", "PM25MORT_OSTRO2004_UNI", "PM25MORT_BRUNETT2018_OUT",
                                       "PM25MORT_BRUNETT2018_WITH")
impact_function_group_2lines.labs <- c("GBD(high)\n(2015)", "GBD(low)\n(2015)", "GBD(medium)\n(2015)",
                                "GBD(2015)", "Jerrett et\nal. (2009)", "Burnett et\nal. (2014)",
                                "Krewski et\nal. (2009)", "Cohen et\nal. (2005)", "Burnett et al.\n(out)(2018)",
                                "Burnett et al.\n(with)(2018)")
names(impact_function_group_2lines.labs) <- c("PM25MORT_GBD2016_HI", "PM25MORT_GBD2016_LO", "PM25MORT_GBD2016_MED",
                                       "O3MORT_GBD2015_UNI", "O3MORT_JERRET2009_UNI", "PM25MORT_BURNETT2014_UNI",
                                       "PM25MORT_KREWSKI2009_UNI", "PM25MORT_OSTRO2004_UNI", "PM25MORT_BRUNETT2018_OUT",
                                       "PM25MORT_BRUNETT2018_WITH")
quantile.labs = c('5th','Median','95th')
names(quantile.labs) <- c("v05", "vmed", "v95")
quantile.linestyle = c('v05'='dotted',
                       'vmed'='solid',
                       'v95'='dashed')

ci_z_level.labs = c('5th','50th','95th')
# ci_z_level.labs = c('5th param\n95th CF','50th param\n50th CF','95th param\n5th CF')
names(ci_z_level.labs) <- c('5th','50th','95th')
ci_z_level.linestyle = c('5th'='dotted',
                       '50th'='solid',
                       '95th'='dashed')

av_mort.long.colors = c("#DDE9DD","#A8E980","#57CB0F","#2A6306")

av_damages.long.colors = c("#1C1ED3", "#0B88CA", "#26DCDC", "#D5D5E7", "#66D31C", "#35740A",
                           "#FF97F1", "#FF40E5", "#FF001E", "#AB071A", "#F1971E", "#FBFA18", "#3E2503")

regions.colors = c('#FFFF00','#FF4500','#8A2BE2','#1E90FF','#32CD32','#FFA500','#FF1493',
                                '#00FA9A','#EE82EE','#FF7F50','#7b857e')



do_rename_imp_fun_etal = function(dat, type = FALSE) {
  dat = data.table(dat)
  if (!type) {
    dat[impact_function_group %in% c("PM25MORT_OSTRO2004_UNI","OSTRO2004_gUNI"),       imp_fun_label := 'Cohen et al. (2005)']
    dat[impact_function_group %in% c("PM25MORT_KREWSKI2009_UNI","KREWSKI2009_gUNI"),   imp_fun_label := 'Krewski et al. (2009)']
    dat[impact_function_group %in% c("PM25MORT_BURNETT2014_UNI","BURNETT2014_gUNI"),   imp_fun_label := 'Burnett et al. (2014)']
    dat[impact_function_group %in% c("PM25MORT_GBD2016_LO","GBD2016_gLO"),             imp_fun_label := 'GBD (low) (2015)']
    dat[impact_function_group %in% c("PM25MORT_GBD2016_MED","GBD2016_gMED"),           imp_fun_label := 'GBD (medium) (2015)']
    dat[impact_function_group %in% c("PM25MORT_GBD2016_HI","GBD2016_gHI"),             imp_fun_label := 'GBD (high) (2015)']
    dat[impact_function_group %in% c("PM25MORT_BRUNETT2018_WITH","BRUNETT2018_gWITH"), imp_fun_label := 'Burnett et al.\n(with) (2018)']
    dat[impact_function_group %in% c("PM25MORT_BRUNETT2018_OUT","BRUNETT2018_gOUT"),   imp_fun_label := 'Burnett et al.\n(without) (2018)']
    dat[impact_function_group %in% c("O3MORT_GBD2015_UNI","GBD2015_gUNI"),             imp_fun_label := 'GBD (2015)']
    dat[impact_function_group %in% c("O3MORT_JERRET2009_UNI","JERRET2009_gUNI"),       imp_fun_label := 'Jerrett et al. (2009)']
  } else {
    dat[impact_function_group %in% c("PM25MORT_OSTRO2004_UNI","OSTRO2004_gUNI"),       imp_fun_label := 'Cohen et al. (2005)\n(Rational)']
    dat[impact_function_group %in% c("PM25MORT_KREWSKI2009_UNI","KREWSKI2009_gUNI"),   imp_fun_label := 'Krewski et al. (2009)\n(LL)']
    dat[impact_function_group %in% c("PM25MORT_BURNETT2014_UNI","BURNETT2014_gUNI"),   imp_fun_label := 'Burnett et al. (2014)\n(IER)']
    dat[impact_function_group %in% c("PM25MORT_GBD2016_LO","GBD2016_gLO"),             imp_fun_label := 'GBD (low) (2015)\n(IER)']
    dat[impact_function_group %in% c("PM25MORT_GBD2016_MED","GBD2016_gMED"),           imp_fun_label := 'GBD (medium) (2015)\n(IER)']
    dat[impact_function_group %in% c("PM25MORT_GBD2016_HI","GBD2016_gHI"),             imp_fun_label := 'GBD (high) (2015)\n(IER)']
    dat[impact_function_group %in% c("PM25MORT_BRUNETT2018_WITH","BRUNETT2018_gWITH"), imp_fun_label := 'Burnett et al.\n(with) (2018)\n(GEMM)']
    dat[impact_function_group %in% c("PM25MORT_BRUNETT2018_OUT","BRUNETT2018_gOUT"),   imp_fun_label := 'Burnett et al.\n(without) (2018)\n(GEMM)']
    dat[impact_function_group %in% c("O3MORT_GBD2015_UNI","GBD2015_gUNI"),             imp_fun_label := 'GBD (2015)\n(LL)']
    dat[impact_function_group %in% c("O3MORT_JERRET2009_UNI","JERRET2009_gUNI"),       imp_fun_label := 'Jerrett et al. (2009)\n(LL)']
  }
  return(dat)
}

do_rename_meth_etal = function(dat) {
  dat = data.table(dat)
  dat[method %in% c("dong_damage_avoided"),  meth_label := 'Dong et al.\n(2021)']
  dat[method %in% c("dech_damage_avoided"),  meth_label := 'Dechezlepretre\net al. (2019)']
  dat[method %in% c("vsl_damage_avoided"),   meth_label := 'VSL']
  dat[method %in% c("hcl_damage_avoided"),   meth_label := 'HCL']
  return(dat)
}

do_rename_regions_string = function(txt) {
  if (txt == 'R10AFRICA') {
    txt = 'Africa'
  } else if (txt == 'R10CHINA+') {
    txt = 'China'
  } else if (txt == 'R10EUROPE') {
    txt = 'Europe'
  } else if (txt == 'R10INDIA+') {
    txt = 'India'
  } else if (txt == 'R10LATIN_AM') {
    txt = 'Latin-America'
  } else if (txt == 'R10MIDDLE_EAST') {
    txt = 'Middle-East'
  } else if (txt == 'R10NORTH_AM') {
    txt = 'North-America'
  } else if (txt == 'R10PAC_OECD') {
    txt = 'Pacific-OECD'
  } else if (txt == 'R10REF_ECON') {
    txt = 'Reference economies'
  } else if (txt == 'R10REST_ASIA') {
    txt = 'Rest-Asia'
  } else if (txt %in% c('WORLD','R10WORLD')) {
    txt = 'World'
  }
  return(invisible(txt))
}

GDP_av = function(GDP_dt, reg, pop_by_reg) {
  GDP_CHINA_av = GDP_dt %>%
    dplyr::filter(region == reg) %>%
    dplyr::group_by(Model,scenario,policy,carbon_budget) %>%
    dplyr::mutate(Val2010 = as.numeric(mean(`2010`, na.rm = TRUE))) %>%
    dplyr::mutate(Val2015 = mean(`2015`, na.rm = TRUE)) %>%
    dplyr::select(Model,scenario,policy,carbon_budget,Val2010,Val2015)
  GDP_CHINA_av = pivot_wider(GDP_CHINA_av, names_from = Model, values_from = c(Val2010,Val2015))
  GDP_CHINA_av$ValMean2010 = rowMeans(GDP_CHINA_av[, c('Val2010_AIM_CGE_V2_2', 'Val2010_IMAGE_3_0', 'Val2010_MESSAGEix-GLOBIOM_1_1', 'Val2010_POLES-JRC_ENGAGE', 'Val2010_REMIND-MAgPIE_2_1-4_2', 'Val2010_WITCH_5_0')], na.rm = TRUE)
  GDP_CHINA_av$ValMean2015 = rowMeans(GDP_CHINA_av[, c('Val2015_AIM_CGE_V2_2', 'Val2015_IMAGE_3_0', 'Val2015_MESSAGEix-GLOBIOM_1_1', 'Val2015_POLES-JRC_ENGAGE', 'Val2015_REMIND-MAgPIE_2_1-4_2', 'Val2015_WITCH_5_0')], na.rm = TRUE)
  GDP_CHINA_av = GDP_CHINA_av %>%
    dplyr::mutate(Val2010_AIM_CGE_V2_2 = coalesce(Val2010_AIM_CGE_V2_2,ValMean2010)) %>%
    dplyr::mutate(Val2010_IMAGE_3_0 = coalesce(Val2010_IMAGE_3_0,ValMean2010)) %>%
    dplyr::mutate(`Val2010_MESSAGEix-GLOBIOM_1_1` = coalesce(`Val2010_MESSAGEix-GLOBIOM_1_1`,ValMean2010)) %>%
    dplyr::mutate(`Val2010_POLES-JRC_ENGAGE` = coalesce(`Val2010_POLES-JRC_ENGAGE`,ValMean2010)) %>%
    dplyr::mutate(`Val2010_REMIND-MAgPIE_2_1-4_2` = coalesce(`Val2010_REMIND-MAgPIE_2_1-4_2`,ValMean2010)) %>%
    dplyr::mutate(Val2010_WITCH_5_0 = coalesce(Val2010_WITCH_5_0,ValMean2010)) %>%
    dplyr::mutate(Val2015_AIM_CGE_V2_2 = coalesce(Val2015_AIM_CGE_V2_2,ValMean2015)) %>%
    dplyr::mutate(Val2015_IMAGE_3_0 = coalesce(Val2015_IMAGE_3_0,ValMean2015)) %>%
    dplyr::mutate(`Val2015_MESSAGEix-GLOBIOM_1_1` = coalesce(`Val2015_MESSAGEix-GLOBIOM_1_1`,ValMean2015)) %>%
    dplyr::mutate(`Val2015_POLES-JRC_ENGAGE` = coalesce(`Val2015_POLES-JRC_ENGAGE`,ValMean2015)) %>%
    dplyr::mutate(`Val2015_REMIND-MAgPIE_2_1-4_2` = coalesce(`Val2015_REMIND-MAgPIE_2_1-4_2`,ValMean2015)) %>%
    dplyr::mutate(Val2015_WITCH_5_0 = coalesce(Val2015_WITCH_5_0,ValMean2015))
  GDP_CHINA_av2010 = pivot_longer(GDP_CHINA_av, cols = c('Val2010_AIM_CGE_V2_2', 'Val2010_IMAGE_3_0', 'Val2010_MESSAGEix-GLOBIOM_1_1', 'Val2010_POLES-JRC_ENGAGE', 'Val2010_REMIND-MAgPIE_2_1-4_2', 'Val2010_WITCH_5_0'),
                                  names_to = 'Model', values_to = 'Val2010') %>%
    dplyr::select(scenario,policy,carbon_budget,ValMean2010,Model,Val2010)
  GDP_CHINA_av2010$Model = str_remove(GDP_CHINA_av2010$Model, 'Val2010_')
  GDP_CHINA_av2015 = pivot_longer(GDP_CHINA_av, cols = c('Val2015_AIM_CGE_V2_2', 'Val2015_IMAGE_3_0', 'Val2015_MESSAGEix-GLOBIOM_1_1', 'Val2015_POLES-JRC_ENGAGE', 'Val2015_REMIND-MAgPIE_2_1-4_2', 'Val2015_WITCH_5_0'),
                                  names_to = 'Model', values_to = 'Val2015') %>%
    dplyr::select(scenario,policy,carbon_budget,ValMean2015,Model,Val2015)
  GDP_CHINA_av2015$Model = str_remove(GDP_CHINA_av2015$Model, 'Val2015_')
  
  GDP_CHINA_av = merge(GDP_CHINA_av2010, GDP_CHINA_av2015, by = c('scenario','policy','carbon_budget','Model'))
  GDP_CHINA_av$Val = rowMeans(GDP_CHINA_av[, c('Val2010','Val2015')], na.rm = TRUE)
  GDP_CHINA_av$pop = pop_by_reg$pop[pop_by_reg$t == 1 & pop_by_reg$region == reg]*0.66 + pop_by_reg$pop[pop_by_reg$t == 2 & pop_by_reg$region == reg]*0.33
  
  return(GDP_CHINA_av)
}

return_year = function(t) {
  yy = ifelse(t == 1, 2020,
              ifelse(t == 2, 2030, 
                     ifelse(t == 3, 2050, NA)))
  return(yy)
}


# rename impact functions and order them by year of publication
do_rename_imp_fun = function(dat,poll_names,short_names) {
  dat = data.table(dat)
  
  if (poll_names & !short_names) {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'PM25 OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'PM25 KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'PM25 BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'PM25 GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'PM25 GBD2016 MEDIUM']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'PM25 GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'PM25 BRUNETT2018 WITH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'PM25 BRUNETT2018 WITHOUT']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'O3 GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'O3 JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('PM25 OSTRO2004',
                                                                    'PM25 KREWSKI2009',
                                                                    'PM25 BURNETT2014',
                                                                    'PM25 GBD2016 LOW',
                                                                    'PM25 GBD2016 MEDIUM',
                                                                    'PM25 GBD2016 HIGH',
                                                                    'PM25 BRUNETT2018 WITH',
                                                                    'PM25 BRUNETT2018 WITHOUT',
                                                                    'O3 JERRET2009',
                                                                    'O3 GBD2015'))]
  } else if (!poll_names & !short_names) {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'GBD2016 MEDIUM']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'BRUNETT2018 WITH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'BRUNETT2018 WITHOUT']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('OSTRO2004',
                                                                    'KREWSKI2009',
                                                                    'BURNETT2014',
                                                                    'GBD2016 LOW',
                                                                    'GBD2016 MEDIUM',
                                                                    'GBD2016 HIGH',
                                                                    'BRUNETT2018 WITH',
                                                                    'BRUNETT2018 WITHOUT',
                                                                    'JERRET2009',
                                                                    'GBD2015'))]
  } else if (!poll_names & short_names) {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'GBD2016 MED']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'BRUNETT18 W']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'BRUNETT18 O']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('OSTRO2004',
                                                                    'KREWSKI2009',
                                                                    'BURNETT2014',
                                                                    'GBD2016 LOW',
                                                                    'GBD2016 MED',
                                                                    'GBD2016 HIGH',
                                                                    'BRUNETT18 W',
                                                                    'BRUNETT18 O',
                                                                    'JERRET2009',
                                                                    'GBD2015'))]
  } else {
    dat[impact_function_group == "PM25MORT_OSTRO2004_UNI", imp_fun_label := 'PM25 OSTRO2004']
    dat[impact_function_group == "PM25MORT_KREWSKI2009_UNI", imp_fun_label := 'PM25 KREWSKI2009']
    dat[impact_function_group == "PM25MORT_BURNETT2014_UNI", imp_fun_label := 'PM25 BURNETT2014']
    dat[impact_function_group == "PM25MORT_GBD2016_LO", imp_fun_label := 'PM25 GBD2016 LOW']
    dat[impact_function_group == "PM25MORT_GBD2016_MED", imp_fun_label := 'PM25 GBD2016 MED']
    dat[impact_function_group == "PM25MORT_GBD2016_HI", imp_fun_label := 'PM25 GBD2016 HIGH']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_WITH", imp_fun_label := 'PM25 BRUNETT18 W']
    dat[impact_function_group == "PM25MORT_BRUNETT2018_OUT", imp_fun_label := 'PM25 BRUNETT18 O']
    dat[impact_function_group == "O3MORT_GBD2015_UNI", imp_fun_label := 'O3 GBD2015']
    dat[impact_function_group == "O3MORT_JERRET2009_UNI", imp_fun_label := 'O3 JERRET2009']
    dat[, impact_function_group := factor(imp_fun_label, levels = c('PM25 OSTRO2004',
                                                                    'PM25 KREWSKI2009',
                                                                    'PM25 BURNETT2014',
                                                                    'PM25 GBD2016 LOW',
                                                                    'PM25 GBD2016 MED',
                                                                    'PM25 GBD2016 HIGH',
                                                                    'PM25 BRUNETT18 W',
                                                                    'PM25 BRUNETT18 O',
                                                                    'O3 JERRET2009',
                                                                    'O3 GBD2015'))]
  }
  return(dat)
}

# rename models and order them alphabetically
do_rename_models = function(dat) {
  dat = data.table(dat)
  
  dat[model == "AIM_CGE_V2_2", model_label := 'AIM CGE']
  dat[model == "IMAGE_3_0", model_label := 'IMAGE']
  dat[model %in% c("MESSAGEix-GLOBIOM_1_1","MESSAGEix.GLOBIOM_1_1"),
      model_label := 'MESSAGEix-GLOBIOM']
  dat[model %in% c("POLES-JRC_ENGAGE","POLES.JRC_ENGAGE"),
      model_label := 'POLES-JRC']
  dat[model %in% c("REMIND-MAgPIE_2_1-4_2","REMIND.MAgPIE_2_1.4_2"),
      model_label := 'REMIND-MAgPIE']
  dat[model == "WITCH_5_0", model_label := 'WITCH']
  dat[, model := factor(model_label, levels = c('AIM CGE',
                                                'IMAGE',
                                                'MESSAGEix-GLOBIOM',
                                                'POLES-JRC',
                                                'REMIND-MAgPIE',
                                                'WITCH'))]
  
  return(dat)
}

custom_y_labels <- function(x) {
  c(round(mean(x),round_num), round(max(x),round_num))
}
