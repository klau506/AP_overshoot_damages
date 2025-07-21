
source('plt_figures_functions.R')
datMort_raw = load_raw_df_mort()
datMort_w_raw = load_raw_df_w_mort()
datMort = load_df_mort()
datMort = keep_only_scen_paired(datMort)
datMort = datMort%>%
  dplyr::rename('Regions' = 'n') %>%
  dplyr::rename('scen' = 'scenario') %>%
  dplyr::mutate('t' = NULL) %>%
  dplyr::rename('t' = 'year') %>%
  dplyr::mutate('region' = NULL) %>%
  as.data.table() %>%   dplyr::filter(!impact_function_group %in% c('PM25MORT_KREWSKI2009_UNI','PM25MORT_OSTRO2004_UNI'))

datMort_raw = bind_rows(datMort_raw, datMort_w_raw) %>%
  dplyr::rename('Regions' = 'n') %>%
  dplyr::rename('scen' = 'scenario') %>%
  dplyr::mutate('t' = NULL) %>%
  dplyr::rename('t' = 'year') %>%
  dplyr::mutate('region' = NULL) %>%
  as.data.table() %>% 
  dplyr::filter(!impact_function_group %in% c('PM25MORT_KREWSKI2009_UNI','PM25MORT_OSTRO2004_UNI'))



#################################################################################
#                            DISTRIB & CUMULATIVE GRAPHS                        #
#################################################################################

source('si_m_distrib_cum.R')
do_m_distrib_cum_folder()
doM_FIGURE_distrib_cum(datMort)

################################################################################
#                                    KS TEST                                   #
################################################################################

source('si_m_kstest.R')
doM_FIGURE_ks_test(datMort_raw)

#################################################################################
#                              CI and ZCF SCENSITIVITY                          #
#################################################################################

source('si_m_sensitivity.R')

# CI
doM_FIGURE_CI_sens_by_region_pollutant(datMort)
doM_FIGURE_CI_sens_by_region(datMort)

# ZCF
doM_FIGURE_ZCF_sens_by_region_pollutant(datMort)
doM_FIGURE_ZCF_sens_by_region(datMort)

# ZCF and CI
doM_FIGURE_ZCF_CI_sens_by_region_pollutant(datMort)
doM_FIGURE_ZCF_CI_sens_by_region(datMort_raw)

###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################

source('si_m_numdeaths.R')
do_numdeaths_folder()
doM_FIGURE_num_deaths(datMort[Regions != 'WORLD'])
source('si_m_avdeaths.R')
doM_AvDeaths_folder()
doM_FIGURE_av_deaths_table(datMort[Regions != 'WORLD']) # (NZ or EoC) - REF
doM_FIGURE_av_deaths_map(datMort[Regions != 'WORLD']) # (NZ or EoC) - REF
doM_FIGURE_av_deaths_nz_minus_eoc_map(datMort[Regions != 'WORLD']) # NZ values - EoC values
doM_FIGURE_av_deaths_map_by_year(datMort[Regions != 'WORLD']) # (NZ or EoC) - REF
doM_FIGURE_av_deaths_map_by_climate_policy_year(datMort[Regions != 'WORLD']) # (NZ or EoC) - REF

###############################################################################
#                                BAD TAILS PROB                               #
###############################################################################

source('si_m_badtails.R')
th = 0.9
do_m_badtails_folder(th)
tmp = doM_compute_bad_tails_prob(datMort,th)
doM_plot_bad_tails_prob(tmp,th)

###############################################################################
#                         GLOBAL SENSITIVITY ANALYSIS                         #
###############################################################################

source('si_m_gsa.R')
do_m_gsa_folder()
doM_gsa(datMort_w_raw)

###############################################################################
#                      IAMS VS RR FUN SENSITIVITY ANALYSIS                    #
###############################################################################

source('si_m_iamsVSrrfun.R')
doM_iams_rrfun(datMort_w_raw)
