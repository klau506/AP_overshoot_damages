require(foreach)
library(ggplot2)
library(data.table)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(countrycode)
library(viridis)
library(ggpubr)
library(plyr)
library(ggh4x)
library(desiderata)
library(stringr)
library(ggsci)
library(cowplot)

data_path = '../fasstr_data'
outD = 'econ'
source('zzz.R')
source('plt_figures_functions.R')
source('plt_figures_kstest.R')

#################################################################################################
#                                           PRE-PROCESS                                         #
#################################################################################################
## load the dfs
df_av = load_df_av()
df_vsl = load_df_vsl()
df_mort = load_df_mort()
df_mort_raw = load_raw_df_mort()
df_mort_w_raw = load_raw_df_w_mort()

################################################################################
#                       FIG 1: GLOBAL HEALTH CO-BENEFITS                       #
################################################################################
dir.create(file.path('paper_figures/fig1'), showWarnings = FALSE)

## -- ks test
source('si_m_kstest.R')
ks_plt = doM_FIGURE_ks_test(df_mort_raw %>%
                              dplyr::filter(year %in% c(2030,2050)) %>%
                              dplyr::rename('Regions' = 'n') %>%
                              dplyr::rename('scen' = 'scenario') %>%
                              dplyr::mutate('t' = NULL) %>%
                              dplyr::rename('t' = 'year') %>%
                              dplyr::mutate('region' = NULL) %>%
                              as.data.table(), SI = FALSE)
ks_plt = ks_plt + 
  theme(strip.text = element_text(color = 'black', size = 12),
        strip.background = element_rect(fill = '#e8e8e8')) + 
  labs(title = '')

### == extended figure
## -- prob distrib & cumulative functions
datt = data.table(df_mort)[df_mort$scenario != 'REF',] %>%
  dplyr::mutate(impact_function_group = factor(impact_function_group,
                                               levels = c("PM25MORT_OSTRO2004_UNI", "PM25MORT_KREWSKI2009_UNI", "PM25MORT_BURNETT2014_UNI", "PM25MORT_GBD2016_LO", "O3MORT_JERRET2009_UNI",
                                                          "PM25MORT_GBD2016_MED", "PM25MORT_GBD2016_HI", "PM25MORT_BRUNETT2018_WITH", "PM25MORT_BRUNETT2018_OUT", "O3MORT_GBD2015_UNI")))
# consider only EoC-NZ cb pairs
datt = keep_only_scen_paired(datt) %>%
  as.data.table()

dmd = prob_distrib_mort(datt[datt$cb_group == '<1000' & datt$region == 11,],2030,remove_xfacet = FALSE,reg = 'WORLD',legend=FALSE)
cmd = cum_fun_mort(datt[datt$cb_group == '<1000' & datt$region == 11,],2030,remove_xfacet = FALSE,'WORLD')

## maps
# difference between climate policies
dattdiff = datt %>%
  dplyr::select(year,ci_z_level,cb_group,region,n,model,policy,value,scenario,impact_function_group) %>%
  dplyr::group_by(year,ci_z_level,cb_group,region,n,scenario,model,impact_function_group) %>%
  dplyr::summarise(median = median(value)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year,ci_z_level,cb_group,region,n,scenario,impact_function_group) %>%
  dplyr::mutate(median = median(median)) %>%
  dplyr::select(!"model") %>%
  dplyr::distinct(., .keep_all = FALSE)

dattdiff = pivot_wider(dattdiff, names_from = c(scenario, impact_function_group), values_from = median) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(PM25MORT_GBD2016_HI       = EoC_PM25MORT_GBD2016_HI       - NZ_PM25MORT_GBD2016_HI) %>%
  dplyr::mutate(PM25MORT_GBD2016_LO       = EoC_PM25MORT_GBD2016_LO       - NZ_PM25MORT_GBD2016_LO) %>%
  dplyr::mutate(PM25MORT_GBD2016_MED      = EoC_PM25MORT_GBD2016_MED      - NZ_PM25MORT_GBD2016_MED) %>%
  dplyr::mutate(O3MORT_GBD2015_UNI        = EoC_O3MORT_GBD2015_UNI        - NZ_O3MORT_GBD2015_UNI) %>%
  dplyr::mutate(O3MORT_JERRET2009_UNI     = EoC_O3MORT_JERRET2009_UNI     - NZ_O3MORT_JERRET2009_UNI) %>%
  dplyr::mutate(PM25MORT_BURNETT2014_UNI  = EoC_PM25MORT_BURNETT2014_UNI  - NZ_PM25MORT_BURNETT2014_UNI) %>%
  dplyr::mutate(PM25MORT_KREWSKI2009_UNI  = EoC_PM25MORT_KREWSKI2009_UNI  - NZ_PM25MORT_KREWSKI2009_UNI) %>%
  dplyr::mutate(PM25MORT_OSTRO2004_UNI    = EoC_PM25MORT_OSTRO2004_UNI    - NZ_PM25MORT_OSTRO2004_UNI) %>%
  dplyr::mutate(PM25MORT_BRUNETT2018_OUT  = EoC_PM25MORT_BRUNETT2018_OUT  - NZ_PM25MORT_BRUNETT2018_OUT) %>%
  dplyr::mutate(PM25MORT_BRUNETT2018_WITH = EoC_PM25MORT_BRUNETT2018_WITH - NZ_PM25MORT_BRUNETT2018_WITH) %>%
  dplyr::select(year,ci_z_level,cb_group,region,n,
                PM25MORT_GBD2016_HI,PM25MORT_GBD2016_LO,PM25MORT_GBD2016_MED,O3MORT_GBD2015_UNI,O3MORT_JERRET2009_UNI,
                PM25MORT_BURNETT2014_UNI,PM25MORT_KREWSKI2009_UNI,PM25MORT_OSTRO2004_UNI,PM25MORT_BRUNETT2018_OUT,PM25MORT_BRUNETT2018_WITH) %>%
  dplyr::distinct(., .keep_all = FALSE)

dattdiff = pivot_longer(dattdiff, cols = c(PM25MORT_GBD2016_HI,PM25MORT_GBD2016_LO,PM25MORT_GBD2016_MED,O3MORT_GBD2015_UNI,O3MORT_JERRET2009_UNI,
                                           PM25MORT_BURNETT2014_UNI,PM25MORT_KREWSKI2009_UNI,PM25MORT_OSTRO2004_UNI,PM25MORT_BRUNETT2018_OUT,PM25MORT_BRUNETT2018_WITH),
                        names_to = 'impact_function', values_to = 'value') %>%
  dplyr::mutate(impact_function = factor(impact_function,
                                         levels = c("PM25MORT_OSTRO2004_UNI", "PM25MORT_KREWSKI2009_UNI", "PM25MORT_BURNETT2014_UNI", "PM25MORT_GBD2016_LO", "O3MORT_JERRET2009_UNI",
                                                    "PM25MORT_GBD2016_MED", "PM25MORT_GBD2016_HI", "PM25MORT_BRUNETT2018_WITH", "PM25MORT_BRUNETT2018_OUT", "O3MORT_GBD2015_UNI")))
# Consider only median discount rate and 2030
dattdiff = dattdiff |> filter(ci_z_level == '50th', cb_group == '<1000')
y = 2030

m_pl = map_plot(dattdiff,save = FALSE,'av_mort_diff',y) 
# ggsave(file=paste0('paper_figures/fig1/map_av_mort_diff_',paste(y, collapse = '-'),'.png'), width = 300, height = 100, units = 'mm', plot = m_pl)
ggsave(file=paste0('paper_figures/fig1/map_av_mort_diff_',paste(y, collapse = '-'),'.pdf'), width = 300, height = 100, units = 'mm', plot = m_pl)

## whole figure
# vertical line to separate PM25 and O3 graphs
pl_vline = ggplot() +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  theme_void() + theme(plot.background = element_rect(fill = 'transparent', color = NA)) +
  xlim(-1,1) + ylim(0,10)


# without title
pl = ggdraw() +
  draw_plot(m_pl, x = 0.01, y = 0.65, width = 0.9, height = 0.35) +
  draw_plot(pl_vline, x = 0.235, y = 0.675, width = 1, height = 0.3) +
  draw_plot(dmd, x = 0.01, y = 0.34, width = 0.9, height = 0.35) +
  draw_plot(pl_vline, x = 0.2225, y = 0.4, width = 1, height = 0.26) +
  draw_plot(cmd, x = 0.01, y = 0, width = 0.9, height = 0.35) +
  draw_plot(pl_vline, x = 0.235, y = 0.105, width = 1, height = 0.22) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0), y = c(0.98, 0.68, 0.34))
ggsave(file=file.path(paste0('paper_figures/fig1/extended_heatlh_cobenefits_notitle_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 400, height = 337.5, unit = 'mm')

# with title
pl = ggdraw() +
  draw_plot(m_pl, x = 0.01, y = 0.635, width = 0.9, height = 0.34) +
  draw_plot(pl_vline, x = 0.235, y = 0.685, width = 1, height = 0.25) +
  draw_plot(dmd, x = 0.01, y = 0.32, width = 0.9, height = 0.34) +
  draw_plot(pl_vline, x = 0.2225, y = 0.365, width = 1, height = 0.27) +
  draw_plot(cmd, x = 0.01, y = -0.01, width = 0.9, height = 0.34) +
  draw_plot(pl_vline, x = 0.235, y = 0.0875, width = 1, height = 0.22) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0), y = c(0.95, 0.66, 0.32)) +
  draw_plot_label(label = c("Extended Data Fig1. Global helath co-benefits of reduced overshoot from EoC to NZ climate policy"), size = 23,
                  x = -0.475, y = 1)
ggsave(file=file.path(paste0('paper_figures/fig1/extended_heatlh_cobenefits_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 400, height = 375, unit = 'mm')

# K-S figure for SI
dir.create(file.path('paper_figures/SI/ks'), showWarnings = FALSE)
ggsave(file=file.path(paste0('paper_figures/SI/ks/ks_mort_2030_2050.pdf')), plot = ks_plt, width = 400, height = 225, unit = 'mm')

### == main fig1
dmd_main = prob_distrib_mort_main(datt[datt$cb_group == '<1000' & datt$region == 11,],2030,remove_xfacet = FALSE,reg = 'WORLD',legend=TRUE)
cmd_main = cum_fun_mort_main(datt[datt$cb_group == '<1000' & pollutant == 'PM25' & datt$region == 11,],2030,remove_xfacet = FALSE,'WORLD') 

# difference between climate policies
# consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
dattdiff = datt %>%
  dplyr::group_by(cb_group,region,n,year,scenario,pollutant,ci_z_level,model) %>%
  dplyr::summarise(x = median(value)) %>%
  dplyr::ungroup() %>%
  # consider the overall deaths: O3 + PM25 deaths
  dplyr::group_by(cb_group,region,n,year,scenario,ci_z_level,model) %>%
  dplyr::summarise(val_sum = sum(x)) %>%
  dplyr::ungroup() %>%
  # consider the overall deaths across all models
  dplyr::group_by(cb_group,region,n,year,scenario,ci_z_level) %>%
  dplyr::summarise(final_val = median(val_sum)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(., .keep_all = FALSE) %>%
  # compute the 95% CI
  dplyr::group_by(cb_group,region,n,year,scenario) %>%
  dplyr::reframe(enframe(quantile(final_val, c(0.05, 0.5, 0.95)), "quantile", "final_val")) %>%
  dplyr::ungroup() %>%
  as.data.frame() %>%
  # rename CI
  dplyr::mutate(across('quantile', \(x) str_replace(x, '50%', 'vmed'))) %>%
  dplyr::mutate(across('quantile', \(x) str_replace(x, '95%', 'v95'))) %>%
  dplyr::mutate(across('quantile', \(x) str_replace(x, '5%', 'v05'))) %>%
  dplyr::mutate(final_val = round(final_val, digits = 0)) %>%
  # compute diff between scen
  tidyr::pivot_wider(names_from = scenario, values_from = final_val) %>%
  mutate(value = EoC - NZ)


m_pl_main = map_plot(dattdiff %>%
                  filter(cb_group == '<1000', year == 2030),save = FALSE,'av_mort_diff_main',y) +
  theme(strip.text = element_text(face = "bold", size = 12))
ggsave(file=paste0('paper_figures/fig1/map_av_mort_diff_main_',paste(y, collapse = '-'),'.pdf'), width = 300, height = 100, units = 'mm', plot = m_pl_main)


# without title
blank_p <- patchwork::plot_spacer() + theme_void()
legend = ggpubr::get_legend(cmd_main +
                              theme(legend.direction = 'vertical'))

pl = ggdraw() +
  draw_plot(m_pl_main, x = 0.01, y = 0.46, width = 0.95, height = 0.6) +
  draw_plot(dmd_main + 
              theme(legend.position = 'none'), x = 0.02, y = 0.11, width = 0.35, height = 0.4) +
  draw_plot(cmd_main + 
              theme(legend.position = 'none'), x = 0.59, y = 0.11, width = 0.35, height = 0.4) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.15, width = 1, height = 1) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0.56), y = c(0.98, 0.53, 0.53))
ggsave(file=file.path(paste0('paper_figures/fig1/heatlh_cobenefits_notitle_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 400, height = 200, unit = 'mm')


# with title
pl = ggdraw() +
  draw_plot(m_pl_main, x = 0.01, y = 0.42, width = 0.95, height = 0.6) +
  draw_plot(dmd_main + 
              theme(legend.position = 'none'), x = 0.02, y = 0.07, width = 0.35, height = 0.4) +
  draw_plot(cmd_main + 
              theme(legend.position = 'none'), x = 0.59, y = 0.07, width = 0.35, height = 0.4) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.19, width = 1, height = 1) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0.56), y = c(0.94, 0.5, 0.5)) +
  draw_plot_label(label = c("Fig1. Global helath co-benefits of reduced overshoot from EoC to NZ climate policy"), size = 19,
                  x = -0.3295, y = 1)
ggsave(file=file.path(paste0('paper_figures/fig1/heatlh_cobenefits_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 400, height = 200, unit = 'mm')



################################################################################
#                           FIG 2: HEALTH UNCERTAINTY                          #
################################################################################
dir.create(file.path('paper_figures/fig2'), showWarnings = FALSE)

dat = data.table(df_mort_raw)[df_mort_raw$scenario != 'REF' & df_mort_raw$cb_group == '<1000'
                                & df_mort_raw$year == 2030 & df_mort_raw$pollutant == 'PM25' & df_mort_raw$scenario == 'NZ',] 
dat_w = data.table(df_mort_w_raw)[df_mort_w_raw$scenario != 'REF' & df_mort_w_raw$cb_group == '<1000'
                                & df_mort_w_raw$year == 2030 & df_mort_w_raw$pollutant == 'PM25' & df_mort_w_raw$scenario == 'NZ',] 

## -- Health parameters & cf sensitivity
pl_sens_china = m_sensitivity_plot(dat,'R10CHINA+','PM25')
name = 'paper_figures/fig2/param_zcf_sensitivity_mort_CHINA'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_sens_china, limitsize = FALSE)
pl_sens_northam = m_sensitivity_plot(dat,'R10NORTH_AM','PM25')
name = 'paper_figures/fig2/param_zcf_sensitivity_mort_NORTHAM'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_sens_northam, limitsize = FALSE)


## -- Health iams vs rr fun
source('si_m_iamsVSrrfun.R')
pl_vs = doM_iams_rrfun(dat_w %>% filter(pollutant == 'PM25'), legend = FALSE)
name = 'paper_figures/fig2/iams_vs_impfun_mort_WORLD'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_vs, limitsize = FALSE)



## -- whole figure
blank_p <- patchwork::plot_spacer() + theme_void()
legend = ggpubr::get_legend(pl_sens_china)
  
# without title
pl = ggdraw() +
  draw_plot(pl_sens_northam +
              theme(legend.position = 'none'), x = 0.01, y = 0.56, width = 0.4, height = 0.42) +
  draw_plot(pl_sens_china +
              theme(legend.position = 'none') +
              labs(y = ''), x = 0.5, y = 0.56, width = 0.4, height = 0.42) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = 0.27, width = 1, height = 1) +
  draw_plot(pl_iams + labs(title = '') + theme(strip.text.x = element_blank(), legend.position = 'none',
                                               axis.text.x = element_text(size = 10)),
            x = 0.01, y = 0, width = 0.45, height = 0.57) +
  draw_plot(pl_impfun + labs(title = '') + theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
                                                 legend.position = 'none',
                                                 axis.text.x = element_text(size = 10)),
            x = 0.5325, y = 0, width = 0.45, height = 0.57) +
  draw_plot_label(label = c("a1", "a2", "b1", "b2"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(0.98, 0.98, 0.57, 0.57))
ggsave(file=file.path(paste0('paper_figures/fig2/heatlh_uncertainty_notitle_2030.pdf')), plot = pl, width = 400, height = 200, unit = 'mm')

# with title
pl = ggdraw() +
  draw_plot(pl_sens_northam +
              theme(legend.position = 'none'), x = 0.01, y = 0.54, width = 0.4, height = 0.41) +
  draw_plot(pl_sens_china +
              theme(legend.position = 'none') +
              labs(y = ''), x = 0.5, y = 0.54, width = 0.4, height = 0.41) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = 0.24, width = 1, height = 1) +
  draw_plot(pl_iams + labs(title = '') + theme(strip.text.x = element_blank(), legend.position = 'none',
                                               axis.text.x = element_text(size = 10)),
            x = 0.01, y = 0, width = 0.45, height = 0.55) +
  draw_plot(pl_impfun + labs(title = '') + theme(strip.text.x = element_blank(), axis.title.y = element_blank(),
                                                 legend.position = 'none',
                                                 axis.text.x = element_text(size = 10)),
            x = 0.5325, y = 0, width = 0.45, height = 0.55) +
  draw_plot_label(label = c("a1", "a2", "b1", "b2"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(0.95, 0.95, 0.55, 0.55)) +
  draw_plot_label(label = c("Fig2. Health co-benefits uncertainty"), size = 20,
                  x = -0.1375, y = 1)
ggsave(file=file.path(paste0('paper_figures/fig2/heatlh_uncertainty_2030.pdf')), plot = pl, width = 420, height = 200, unit = 'mm')


################################################################################
#                      FIG 3: GLOBAL ECONOMIC CO-BENEFITS                      #
################################################################################
dir.create(file.path('paper_figures/fig3'), showWarnings = FALSE)

dat1 = df_av[df_av$scenario != 'REF',]
dat2 = df_vsl[df_vsl$scenario != 'REF',]

datt = merge(dat1, dat2) %>%
  dplyr::mutate(vsl_damage_avoided = -vsl_damage_avoided) %>%
  dplyr::mutate(dong_damage_avoided = -dong_damage_avoided) %>%
  dplyr::rename('alpha_original' = 'alpha') %>%
  dplyr::mutate(alpha_original = factor(alpha_original, levels = c("hi", "med", "lo")))
datt = pivot_longer(datt, cols = c('vsl_damage_avoided','hcl_damage_avoided','dong_damage_avoided','dech_damage_avoided'), names_to ='method',
                    values_to = 'value')

# Consider only EoC-NZ pairs with the same cb; erase the extra ones
datt = keep_only_scen_paired(datt)

# Fix alpha behavior in cumulative plots
datt$alpha = datt$alpha_original  
datt$alpha[datt$method %in% c('vsl_damage_avoided') & datt$alpha_original == 'lo'] = 'hi'
datt$alpha[datt$method %in% c('vsl_damage_avoided') & datt$alpha_original == 'hi'] = 'lo'
datt$alpha[datt$method %in% c('hcl_damage_avoided') & datt$alpha_original == 'lo'] = 'hi'
datt$alpha[datt$method %in% c('hcl_damage_avoided') & datt$alpha_original == 'hi'] = 'lo'

# Set discount rate levels as factors
datt = datt %>%
  dplyr::mutate(alpha = factor(alpha, levels = c("hi", "med", "lo")))

### === extended figure
## -- prob distrib & cumulative functions
dpd = prob_distrib_econ(datt %>% filter(cb_group == '<1000'),2030,remove_xfacet = FALSE,reg = 'WORLD',legend = FALSE)
cpd = cum_fun_econ(datt %>% filter(cb_group == '<1000'),2030,remove_xfacet = FALSE,'WORLD')

## -- map
dat11 = dat1 %>%
  dplyr::filter(cb_group == '<1000') %>%
  dplyr::select(-c(dech_damage_avoided))
dat11 = pivot_wider(dat11, names_from = scenario, values_from = dong_damage_avoided) %>%
  drop_na() %>%
  dplyr::group_by(region, year, alpha, cb_group, n, model) %>%
  dplyr::summarise(EoC_median = median(EoC),
                NZ_median = median(NZ)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = median(EoC_median),
                   NZ_median = median(NZ_median)) %>%
  dplyr::ungroup() %>%
  as.data.frame()
dat11$dong_damage_avoided = dat11$EoC_median - dat11$NZ_median
  
dat12 = dat1 %>%
  dplyr::filter(cb_group == '<1000') %>%
  dplyr::select(-c(dong_damage_avoided))
dat12 = pivot_wider(dat12, names_from = scenario, values_from = dech_damage_avoided) %>%
  drop_na() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = -median(EoC),
                   NZ_median = -median(NZ)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = median(EoC_median),
                   NZ_median = median(NZ_median)) %>%
  dplyr::ungroup() %>%
  as.data.frame()
dat12$dech_damage_avoided = dat12$EoC_median - dat12$NZ_median
  
dat21 = dat2 %>%
  dplyr::filter(cb_group == '<1000') %>%
  dplyr::select(-c(hcl_damage_avoided))
dat21 = pivot_wider(dat21, names_from = scenario, values_from = vsl_damage_avoided) %>%
  drop_na() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = median(EoC),
                   NZ_median = median(NZ)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = median(EoC_median),
                   NZ_median = median(NZ_median)) %>%
  dplyr::ungroup() %>%
  as.data.frame()
dat21$vsl_damage_avoided = dat21$EoC_median - dat21$NZ_median

dat22 = dat2 %>%
  dplyr::filter(cb_group == '<1000') %>%
  dplyr::select(-c(vsl_damage_avoided))
dat22 = pivot_wider(dat22, names_from = scenario, values_from = hcl_damage_avoided) %>%
  drop_na() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = median(EoC),
                   NZ_median = median(NZ)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, year, alpha, cb_group, n) %>%
  dplyr::summarise(EoC_median = median(EoC_median),
                   NZ_median = median(NZ_median)) %>%
  dplyr::ungroup() %>%
  as.data.frame()
dat22$hcl_damage_avoided = -(dat22$EoC_median - dat22$NZ_median)

dattm1 = merge(dat11, dat12, by = c('region', 'year', 'alpha', 'cb_group', 'n'))
dattm2 = merge(dat21, dat22, by = c('region', 'year', 'alpha', 'cb_group', 'n'))
dattm = merge(dattm1, dattm2, by = c('region', 'year', 'alpha', 'cb_group', 'n'))
dattm = dattm %>%
  dplyr::mutate(alpha = factor(alpha, levels = c("hi", "med", "lo"))) %>%
  dplyr::select(c(region, year, alpha, cb_group, n,
                  vsl_damage_avoided, dong_damage_avoided, dech_damage_avoided, hcl_damage_avoided)) %>%
  dplyr::distinct(.keep_all = TRUE)

dattm = pivot_longer(dattm, cols = c('vsl_damage_avoided','dong_damage_avoided','dech_damage_avoided', 'hcl_damage_avoided'), names_to ='method',
                     values_to = 'value')

# Consider only median discount rate and 2030
dattm = dattm |> filter(alpha == 'med')
y = 2030

pl_map = map_plot(dattm,save = FALSE,'av_damage_diff',y,facet_title = TRUE)
name = paste0('paper_figures/fig3/map_av_damage_diff_',paste(y, collapse = '-'),'.png')
ggsave(file=file.path(name), width = 550, height = 175, units = 'mm', plot = pl_map)


## -- ks test
source('si_econ_kstest.R')
pl_ks = doEcon_ks_test_process(datt %>% dplyr::filter(n != 'WORLD' & scenario != 'REF' & year %in% c(2030,2050)) %>%
                                 dplyr::mutate(region = NULL) %>%
                                 dplyr::rename('region' = 'n') %>%
                                 dplyr::select(year,alpha,scenario,policy,pollutant,carbon_budget,model,cb_group,impact_function_global_level,
                                               method, value, region) %>%
                                 as.data.table())
pl_ks = pl_ks + 
  theme(strip.text = element_text(color = 'black', size = 12),
        strip.background = element_rect(fill = '#e8e8e8')) + 
  labs(title = '')


## -- whole figure
# without title
pl = ggdraw() +
  draw_plot(pl_map, x = 0.01, y = 0.65, width = 0.9, height = 0.40) +
  draw_plot(dpd, x = 0.01, y = 0.38, width = 0.9, height = 0.34) +
  draw_plot(cpd, x = 0.01, y = 0, width = 0.9, height = 0.39) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0), y = c(0.99, 0.72, 0.39))
ggsave(file=file.path(paste0('paper_figures/fig3/extendend_econ_cobenefits_notitle_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 350, height = 250, unit = 'mm')

# with title
p1 = ggdraw() +
  draw_plot(pl_map, x = 0.01, y = 0.63, width = 0.9, height = 0.40) +
  draw_plot(dpd, x = 0.01, y = 0.38, width = 0.9, height = 0.34) +
  draw_plot(cpd, x = 0.01, y = 0, width = 0.9, height = 0.39) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0), y = c(0.95, 0.72, 0.39)) +
  draw_plot_label(label = c("Extended Fig3. Global economic co-benefits of reduced overshoot from EoC to NZ climate policy"), size = 20,
                  x = -0.465, y = 1)
ggsave(file=file.path(paste0('paper_figures/fig3/extended_econ_cobenefits_',paste(y, collapse = '-'),'.pdf')), plot = p1, width = 350, height = 275, unit = 'mm')

# K-S figure for SI
dir.create(file.path('paper_figures/SI/ks'), showWarnings = FALSE)
ggsave(file=file.path(paste0('paper_figures/SI/ks/ks_econ_2030_2050.pdf')), plot = pl_ks, width = 400, height = 225, unit = 'mm')


### === main figure
## -- prob distrib & cumulative functions
dpd_main = prob_distrib_econ_main(datt %>% filter(cb_group == '<1000'),2030,remove_xfacet = FALSE,reg = 'WORLD')
cpd_main = cum_fun_econ_main(datt %>% filter(cb_group == '<1000'),2030,remove_xfacet = FALSE,'WORLD')
## map
dattdiff = datt %>%
  dplyr::group_by(cb_group,region,n,year,scenario,alpha,model,method) %>%
  dplyr::summarise(x = median(value)) %>%
  dplyr::ungroup() %>%
  # consider the overall deaths across all models
  dplyr::group_by(cb_group,region,n,year,scenario,alpha,method) %>%
  dplyr::summarise(val = median(x)) %>%
  dplyr::ungroup() %>%
  # consider the overall deaths across all methods
  dplyr::group_by(cb_group,region,n,year,scenario,alpha) %>%
  dplyr::summarise(final_val = median(val)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(., .keep_all = FALSE) %>%
  # compute the 95% CI
  dplyr::group_by(cb_group,region,n,year,scenario) %>%
  dplyr::reframe(enframe(quantile(final_val, c(0.05, 0.5, 0.95)), "quantile", "final_val")) %>%
  dplyr::ungroup() %>%
  as.data.frame() %>%
  # rename CI
  dplyr::mutate(across('quantile', \(x) str_replace(x, '50%', 'vmed'))) %>%
  dplyr::mutate(across('quantile', \(x) str_replace(x, '95%', 'v95'))) %>%
  dplyr::mutate(across('quantile', \(x) str_replace(x, '5%', 'v05'))) %>%
  dplyr::mutate(final_val = round(final_val, digits = 2)) %>%
  # compute diff between scen
  tidyr::pivot_wider(names_from = scenario, values_from = final_val) %>%
  mutate(value = NZ - EoC) %>%
  filter(complete.cases(.))

econ_pl_main = map_plot(dattdiff %>%
                       filter(cb_group == '<1000', year == 2030),save = FALSE,'av_damage_diff_main',y) +
  theme(strip.text = element_text(face = "bold", size = 12))
ggsave(file=paste0('paper_figures/fig3/map_av_damage_diff_main_',paste(y, collapse = '-'),'.pdf'), width = 300, height = 100, units = 'mm', plot = econ_pl_main)


# without title
blank_p <- patchwork::plot_spacer() + theme_void()
legend = ggpubr::get_legend(cpd_main +
                              theme(legend.direction = 'vertical'))

pl = ggdraw() +
  draw_plot(econ_pl_main, x = 0.01, y = 0.46, width = 0.95, height = 0.6) +
  draw_plot(dpd_main + 
              theme(legend.position = 'none'), x = 0.02, y = 0.11, width = 0.35, height = 0.4) +
  draw_plot(cpd_main + 
              theme(legend.position = 'none'), x = 0.59, y = 0.11, width = 0.35, height = 0.4) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.15, width = 1, height = 1) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0.56), y = c(0.98, 0.53, 0.53))
ggsave(file=file.path(paste0('paper_figures/fig3/econ_cobenefits_notitle_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 400, height = 200, unit = 'mm')


# with title
pl = ggdraw() +
  draw_plot(econ_pl_main, x = 0.01, y = 0.42, width = 0.95, height = 0.6) +
  draw_plot(dpd_main + 
              theme(legend.position = 'none'), x = 0.02, y = 0.07, width = 0.35, height = 0.4) +
  draw_plot(cpd_main + 
              theme(legend.position = 'none'), x = 0.59, y = 0.07, width = 0.35, height = 0.4) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.19, width = 1, height = 1) +
  draw_plot_label(label = c("a", "b", "c"), size = 15,
                  x = c(0, 0, 0.56), y = c(0.94, 0.5, 0.5)) +
  draw_plot_label(label = c("Fig3. Global economic co-benefits of reduced overshoot from EoC to NZ climate policy"), size = 19,
                  x = -0.344, y = 1)
ggsave(file=file.path(paste0('paper_figures/fig3/econ_cobenefits_',paste(y, collapse = '-'),'.pdf')), plot = pl, width = 400, height = 200, unit = 'mm')

################################################################################
#                          FIG 4: ECONOMY UNCERTAINTY                          #
################################################################################
dir.create(file.path('paper_figures/fig4'), showWarnings = FALSE)

datt = merge(df_av, df_vsl) %>%
  dplyr::mutate(vsl_damage_avoided = -vsl_damage_avoided) %>%
  dplyr::mutate(dong_damage_avoided = -dong_damage_avoided) %>%
  dplyr::rename('alpha_original' = 'alpha') %>%
  dplyr::mutate(alpha_original = factor(alpha_original, levels = c("hi", "med", "lo")))
datt = pivot_longer(datt, cols = c('vsl_damage_avoided','hcl_damage_avoided','dong_damage_avoided','dech_damage_avoided'), names_to ='method',
                    values_to = 'value')

# Consider only EoC-NZ pairs with the same cb; erase the extra ones
datt = keep_only_scen_paired(datt)

# Fix alpha behavior in cumulative plots
datt$alpha = datt$alpha_original  
datt$alpha[datt$method %in% c('vsl_damage_avoided') & datt$alpha_original == 'lo'] = 'hi'
datt$alpha[datt$method %in% c('vsl_damage_avoided') & datt$alpha_original == 'hi'] = 'lo'
datt$alpha[datt$method %in% c('hcl_damage_avoided') & datt$alpha_original == 'lo'] = 'hi'
datt$alpha[datt$method %in% c('hcl_damage_avoided') & datt$alpha_original == 'hi'] = 'lo'

# Set discount rate levels as factors
datt = datt %>%
  dplyr::mutate(alpha = factor(alpha, levels = c("hi", "med", "lo")))


## -- Discount rate and econ meth sensitivity
dattw = data.table(datt)[datt$scenario == 'NZ' & datt$cb_group == '<1000' & year == 2030 & region == 11,] 
dattreg = data.table(datt)[datt$scenario == 'NZ' & datt$cb_group == '<1000' & year == 2030 & region != 11,] 


## -- Health parameters & cf sensitivity
pl_sens_northam = econ_sensitivity_plot(dattreg,'R10NORTH_AM','PM25')
name = 'paper_figures/fig4/alpha_rrfun_sensitivity_mort_NORTHAM'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_sens_northam, limitsize = FALSE)
pl_sens_europe = econ_sensitivity_plot(dattreg,'R10EUROPE','PM25')
name = 'paper_figures/fig4/alpha_rrfun_sensitivity_mort_EUROPE'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_sens_europe, limitsize = FALSE)
pl_sens_india = econ_sensitivity_plot(dattreg,'R10INDIA+','PM25')
name = 'paper_figures/fig4/alpha_rrfun_sensitivity_mort_INDIA'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_sens_india, limitsize = FALSE)
pl_sens_latinam = econ_sensitivity_plot(dattreg,'R10LATIN_AM','PM25')
name = 'paper_figures/fig4/alpha_rrfun_sensitivity_mort_LATINAM'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_sens_latinam, limitsize = FALSE)

dir.create(file.path('paper_figures/fig4/alpha_rrfun_sens'), showWarnings = FALSE)
for (reg in unique(dattreg$n)) {
  pl_sens = econ_sensitivity_plot(dattreg,reg,'PM25')
  name = paste0('paper_figures/fig4/alpha_rrfun_sens/alpha_rrfun_sensitivity_mort_',reg)
  print(name)
  ggsave(file=paste0(name,'.png'), width = 220, height = 150, units = "mm",plot = pl_sens, limitsize = FALSE)
}

## -- Econ iams vs meth
source('si_econ_iamsVSmeth.R')
pl_vs = doEcon_iams_meth(dattw %>% filter(scenario == 'NZ', n == 'WORLD', cb_group == '<1000', year == 2030), legend = FALSE)
name = 'paper_figures/fig4/iams_vs_meth_WORLD'
print(name)
ggsave(file=paste0(name,'.pdf'), width = 220, height = 150, units = "mm",plot = pl_vs, limitsize = FALSE)


## -- whole figure
blank_p <- patchwork::plot_spacer() + theme_void()
legend = ggpubr::get_legend(pl_sens_india +
                              theme(
                                legend.direction = "horizontal", legend.box = "horizontal",
                                legend.box.just = "right", legend.margin = margin(t = 0, r = 10, b = 0, l = 0)  # Adjust legend margin
                              ) +
                              guides(
                                color = guide_legend(title.position = "top"),
                                linetype = guide_legend(title.position = "top")
                              ))

# without title
pl = ggdraw() +
  draw_plot(pl_sens_india +
              theme(legend.position = 'none'), x = 0.01, y = 0.55, width = 0.3, height = 0.45) +
  draw_plot(pl_sens_europe +
              theme(legend.position = 'none'), x = 0.01, y = 0.1, width = 0.3, height = 0.45) +
  draw_plot(pl_sens_latinam +
              theme(legend.position = 'none') +
              labs(y = ''), x = 0.33, y = 0.55, width = 0.3, height = 0.45) +
  draw_plot(pl_sens_northam +
              theme(legend.position = 'none') +
              labs(y = ''), x = 0.33, y = 0.1, width = 0.3, height = 0.45) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.1, y = -0.44, width = 1, height = 1) +
  draw_plot(pl_iams + labs(title = '') + theme(strip.text.x = element_blank(), legend.position = 'none'),
            x = 0.66, y = 0.53, width = 0.34, height = 0.465) +
  draw_plot(pl_meth + labs(title = '') + theme(strip.text.x = element_blank(), legend.position = 'none'),
            x = 0.66, y = 0.05, width = 0.305, height = 0.465) +
  draw_plot_label(label = c("a1 (India)", "a2 (Europe)", "a3 (North Am)", "a4 (Latin Am)", "b1", "b2"), size = 15,
                  x = c(-0.01, 0.305, -0.025, 0.3, 0.65, 0.65), y = c(0.98, 0.98, 0.53, 0.53, 0.98, 0.53))
ggsave(file=file.path(paste0('paper_figures/fig4/econ_uncertainty_notitle_2030.pdf')), plot = pl, width = 500, height = 250, unit = 'mm')

# with title
pl = ggdraw() +
  draw_plot(pl_sens_india +
            theme(legend.position = 'none'), x = 0.01, y = 0.53, width = 0.3, height = 0.44) +
  draw_plot(pl_sens_europe +
              theme(legend.position = 'none'), x = 0.01, y = 0.08, width = 0.3, height = 0.44) +
  draw_plot(pl_sens_latinam +
              theme(legend.position = 'none') +
              labs(y = ''), x = 0.33, y = 0.53, width = 0.3, height = 0.44) +
  draw_plot(pl_sens_northam +
              theme(legend.position = 'none') +
              labs(y = ''), x = 0.33, y = 0.08, width = 0.3, height = 0.44) +
  draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.1, y = -0.45, width = 1, height = 1) +
  draw_plot(pl_iams + labs(title = '') + theme(strip.text.x = element_blank(), legend.position = 'none'),
            x = 0.66, y = 0.52, width = 0.34, height = 0.46) +
  draw_plot(pl_meth + labs(title = '') + theme(strip.text.x = element_blank(), legend.position = 'none'),
            x = 0.66, y = 0.065, width = 0.305, height = 0.46) +
  draw_plot_label(label = c("a1 (India)", "a2 (Europe)", "a3 (North Am)", "a4 (Latin Am)", "b1", "b2"), size = 15,
                  x = c(-0.0105, 0.305, -0.025, 0.3, 0.65, 0.65), y = c(0.96, 0.96, 0.51, 0.51, 0.96, 0.51)) +
  draw_plot_label(label = c("Fig4. Economic co-benefits uncertainty"), size = 20,
                  x = -0.125, y = 1)
ggsave(file=file.path(paste0('paper_figures/fig4/econ_uncertainty_2030.pdf')), plot = pl, width = 500, height = 275, unit = 'mm')

