dir.create(file.path('paper_figures/SI/uncert'), showWarnings = FALSE)
source('si_uncert_functions.R')

df_av = load_df_av()
df_vsl = load_df_vsl()
df_mort_raw = load_raw_df_mort() %>% as.data.table()

# Damage uncertainty
dat1 = df_av[df_av$scenario != 'REF' & df_av$cb_group == '<1000',]
dat2 = df_vsl[df_vsl$scenario != 'REF' & df_vsl$cb_group == '<1000',]
dat3 = df_mort_raw[df_mort_raw$scenario != 'REF' & df_mort_raw$cb_group == '<1000' & year %in% c(2030,2050),]

dat_econ = merge(dat1, dat2) %>%
  dplyr::filter(pollutant == 'PM25' & year != 2020) %>%
  dplyr::mutate(vsl_damage_avoided = -vsl_damage_avoided) %>%
  dplyr::mutate(dong_damage_avoided = -dong_damage_avoided) %>%
  dplyr::rename('alpha_original' = 'alpha') %>%
  dplyr::mutate(alpha_original = factor(alpha_original, levels = c("hi", "med", "lo")))
dat_econ = pivot_longer(dat_econ, cols = c('vsl_damage_avoided','hcl_damage_avoided','dong_damage_avoided','dech_damage_avoided',), names_to ='method',
                        values_to = 'value') %>%
  as.data.table() 

# Consider only EoC-NZ pairs with the same cb; erase the extra ones
dat_econ = keep_only_scen_paired(dat_econ)

# Fix alpha behavior in cumulative plots
dat_econ$alpha = dat_econ$alpha_original  
dat_econ$alpha[dat_econ$method %in% c('vsl_damage_avoided') & dat_econ$alpha_original == 'lo'] = 'hi'
dat_econ$alpha[dat_econ$method %in% c('vsl_damage_avoided') & dat_econ$alpha_original == 'hi'] = 'lo'
dat_econ$alpha[dat_econ$method %in% c('hcl_damage_avoided') & dat_econ$alpha_original == 'lo'] = 'hi'
dat_econ$alpha[dat_econ$method %in% c('hcl_damage_avoided') & dat_econ$alpha_original == 'hi'] = 'lo'


for (y in c(2030, 2050)) {
  pl_econ_indiv = do_econ_uncert.individual(dat_econ[dat_econ$n != 'WORLD' & dat_econ$year == y,],'allreg') + 
    labs(title = '') +
    scale_x_discrete(guide = guide_axis(n.dodge=1)) +
    theme(strip.text = element_blank())
  pl_econ_prop = do_econ_uncert.propagation(dat_econ[dat_econ$n != 'WORLD' & dat_econ$year == y,],'allreg') + 
    labs(title = '', y = '') +
    scale_x_discrete(guide = guide_axis(n.dodge=1)) +
    theme(strip.text = element_blank())
  
  # Mortality uncertainty
  pl_mort_indiv = do_mort_uncert.individual(dat3[dat3$region != 'WORLD' & dat3$year == y,],'allreg') + 
    labs(title = '') +
    scale_x_discrete(guide = guide_axis(n.dodge=1)) +
    theme(strip.text = element_blank())
  pl_mort_prop = do_mort_uncert.propagation(dat3[dat3$region != 'WORLD' & dat3$year == y,],'allreg') + 
    labs(title = '', y = '') +
    scale_x_discrete(guide = guide_axis(n.dodge=1)) +
    theme(strip.text = element_blank())
  
  ## whole figure
  pl = ggdraw() +
    draw_plot(pl_mort_indiv, x = 0.01, y = 0.5, width = 0.5, height = 0.5) +
    draw_plot(pl_mort_prop, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
    draw_plot(pl_econ_indiv, x = 0.01, y = 0, width = 0.5, height = 0.5) +
    draw_plot(pl_econ_prop, x = 0.5, y = 0, width = 0.5, height = 0.5) +
    draw_plot_label(label = c("a", "b", "c", "d"), size = 15,
                    x = c(0.01, 0.5, 0.01, 0.5), y = c(0.99, 0.99, 0.49, 0.49))
  
  ggsave(file=file.path(paste0('paper_figures/SI/uncert/uncert_',y,'.pdf')), plot = pl, width = 400, height = 300, unit = 'mm')
}
  