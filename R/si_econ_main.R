source('plt_figures_functions.R')
df_av = load_df_av()
df_vsl = load_df_vsl()

datt = merge(df_av, df_vsl) %>%
  dplyr::mutate(vsl_damage_avoided = -vsl_damage_avoided) %>%
  dplyr::mutate(dong_damage_avoided = -dong_damage_avoided) %>%
  dplyr::rename('alpha_original' = 'alpha') %>%
  dplyr::mutate(alpha_original = factor(alpha_original, levels = c("hi", "med", "lo"))) %>%
  dplyr::select(-region) %>%
  dplyr::rename(region = n)
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

# SI ks-test figures
source('si_econ_kstest.R')
doEcon_KsTest_folder()
doEcon_ks_test_process(datt %>% dplyr::filter(region != 'WORLD' & scenario != 'REF') %>%
                         dplyr::select(year,alpha,scenario,policy,pollutant,carbon_budget,model,cb_group,impact_function_global_level,
                              method, value, region) %>%
                         as.data.table())

# SI cum-distrib figures
source('si_econ_distrib_cum.R')
doEcon_DistribCum_folder()
doEcon_FIGURE_distrib_cum(datt %>% dplyr::filter(scenario != 'REF'), slides = FALSE)


# SI map econ co-benefits
source('si_econ_cobenefits.R')
doEcon_CoBenefits_folder()
doEcon_FIGURE_co_benefits_table(datt %>% filter(region != 'WORLD')) # (NZ or EoC) - REF
doEcon_FIGURE_co_benefits_table(datt %>% filter(region == 'WORLD')) # (NZ or EoC) - REF
doEcon_FIGURE_co_benefits_map(datt %>% filter(region != 'WORLD')) # (NZ or EoC) - REF
doEcon_FIGURE_co_benefits_nz_minus_eoc_map(datt %>% filter(region != 'WORLD')) # NZ values - EoC values
doEcon_FIGURE_co_benefits_map_by_year(datt %>% filter(region != 'WORLD')) # (NZ or EoC) - REF
doEcon_FIGURE_co_benefits_map_by_climate_policy_year(datt %>% filter(region != 'WORLD')) # (NZ or EoC) - REF


# SI bad tails prob
source('si_econ_badtails.R')
th = 0.9
do_econ_badtails_folder(th)
tmp = doECON_compute_bad_tails_prob(datt,th)
doECON_plot_bad_tails_prob(tmp,th)


# SI gsa
source('si_econ_gsa.R')
do_econ_gsa_folder()
doEcon_gsa(datt %>% filter(region != 'WORLD'))

# SI iams vs meth
source('si_econ_iamsVSmeth.R')
doEcon_iams_meth(datt %>% filter(region == 'WORLD'))

