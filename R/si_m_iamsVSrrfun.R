do_iams_vs_impfun_pre = function(datIni, poll) {
  dat_to_plot = datIni |> filter(year %in% c(2030,2050)
                                 & pollutant == poll & scenario != 'REF'
                                 & ci_level == 'ciMED' & z_level %in% c('zMED','zUNI'))
  
  dat_medi_iams <- data.table(dat_to_plot)[, .(medi_iams = quantile(value, 0.5)), #median
                              by=c('scenario','cb_group','year','model')]
  dat_medi_impfun <- data.table(dat_to_plot)[, .(medi_impfun = quantile(value, 0.5)), #median
                                by=c('scenario','cb_group','year','impact_function_group')]
  dat = merge(dat_to_plot, dat_medi_iams, by=c('scenario','cb_group','year','model'))
  datall = merge(dat, dat_medi_impfun, by=c('scenario','cb_group','year','impact_function_group'))

  datall = data.table(datall)
  datall = do_rename_models(datall)
  datall = do_rename_imp_fun_etal(datall)
  if (poll == 'PM25') {
    datall$imp_fun_label = (factor(datall$imp_fun_label, levels = 
                                            c('Cohen et al. (2005)','Krewski et al. (2009)',
                                              'Burnett et al. (2014)','GBD (low) (2015)','GBD (medium) (2015)',
                                              'GBD (high) (2015)','Burnett et al.\n(with) (2018)',
                                              'Burnett et al.\n(without) (2018)')))
  } else {
    datall$imp_fun_label = (factor(datall$imp_fun_label, levels = 
                                            c('Jerrett et al. (2009)','GBD (2015)')))
  }
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))

  return(datall)
}

do_iams_plot = function(datIni, poll, save) {
  pl_iams <- ggplot(datIni) +
    geom_density(aes(x = value, group = scenario,
                     color = scenario,
                     fill = scenario), alpha = 0.5) +
    geom_vline(aes(color = scenario, xintercept = medi_iams),
               linetype="dashed", linewidth = 1) +
    facet_grid(model ~ year, scales='free') +
    theme_light() +
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom", strip.text = element_text(color = "black"),
          strip.text.y = element_text(size = 13, angle = 0, hjust = 0), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(), plot.background = element_rect(fill = 'white'), 
          axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),
          strip.background = element_rect(fill = 'white', color = 'white')) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    scale_fill_manual(values = scenario.colors,
                      name = 'Climate\npolicy') +
    scale_color_manual(values = scenario.colors,
                       name = 'Climate\npolicy') +
    labs(title='', x = 'Premature deaths [million people/year]', y = "Probability density\n\n\n")
  
  if(save) {
    h = as.integer(length(unique(dat_to_plot$impact_function_group)))*37.5
    name_file_iams = ifelse(cb == '<1000', paste0('Results/Mort/IAMsVSImpFun/iams/iams_cbL_',poll),
                              ifelse(cb == '[1000,2000]', paste0('Results/Mort/IAMsVSImpFun/iams/iams_cbM_',poll),
                                     paste0('Results/Mort/IAMsVSImpFun/iams/iams_cbH_',poll)))
    print(name_file_iams)
    ggsave(file=paste0(name_file_iams,'.png'), width = 300, height = h, units = "mm",plot = pl_iams, limitsize = FALSE)
    ggsave(file=paste0(name_file_iams,'.pdf'), width = 300, height = h, units = "mm",plot = pl_iams, limitsize = FALSE)
  }
  return(pl_iams)
}

do_impfun_plot = function(datIni, poll, save) {
  pl_impfun <- ggplot(datIni) +
    geom_density(aes(x = value, group = scenario,
                     color = scenario,
                     fill = scenario), alpha = 0.5) +
    geom_vline(aes(color = scenario, xintercept = medi_impfun),
               linetype="dashed", linewidth = 1) +
    facet_grid(imp_fun_label ~ year, scales='free')+
    theme_light() +
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom", strip.text = element_text(color = "black"),
          strip.text.y = element_text(size = 13, angle = 0, hjust = 0), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(), plot.background = element_rect(fill = 'white'), 
          axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),
          strip.background = element_rect(fill = 'white', color = 'white')) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    scale_fill_manual(values = scenario.colors,
                      name = 'Climate\npolicy')+
    scale_color_manual(values = scenario.colors,
                       name = 'Climate\npolicy')+
    labs(title='', x = 'Premature deaths [million people/year]', y = "Probability density\n")
  
  if(save) {
    h = as.integer(length(unique(dat_to_plot$impact_function_group)))*37.5
    name_file_impfun = ifelse(cb == '<1000', paste0('Results/Mort/IAMsVSImpFun/rr/rr_cbL_',poll),
                     ifelse(cb == '[1000,2000]', paste0('Results/Mort/IAMsVSImpFun/rr/rr_cbM_',poll),
                            paste0('Results/Mort/IAMsVSImpFun/rr/rr_cbH_',poll)))
    print(name_file_impfun)
    ggsave(file=paste0(name_file_impfun,'.png'), width = 300, height = h, units = "mm",plot = pl_impfun, limitsize = FALSE)
    ggsave(file=paste0(name_file_impfun,'.pdf'), width = 300, height = h, units = "mm",plot = pl_impfun, limitsize = FALSE)
  }
  return(pl_impfun)
}


# do figure by region
doM_iams_rrfun = function(datIni, legend = TRUE) {
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  for (poll in unique(datIni$pollutant)) {
    dat_to_plot = do_iams_vs_impfun_pre(datIni, poll)
    str(dat_to_plot)
    
    # CI of medi_iams (median across IAMs)
    ci_medi_iams <- dat_to_plot %>% 
      dplyr::select(scenario, cb_group, year, pollutant, model_label, medi_iams) %>% 
      unique()
    ci_medi_iams_quantiles <- c(min(ci_medi_iams$medi_iams), median(ci_medi_iams$medi_iams), max(ci_medi_iams$medi_iams))
    print("The min-max for the medians accross IAMs is ")
    print(ci_medi_iams_quantiles)

    # CI of medi_impfun (median across RR function)
    ci_medi_impfun <- dat_to_plot %>% 
      dplyr::select(scenario, cb_group, year, pollutant, imp_fun_label, medi_impfun) %>% 
      unique()
    ci_medi_impfun_quantiles <- c(min(ci_medi_impfun$medi_impfun), median(ci_medi_impfun$medi_impfun), max(ci_medi_impfun$medi_impfun))
    print("The min-max for the medians accross RR functions is ")
    print(ci_medi_impfun_quantiles)
    
    for (cb in unique(dat_to_plot$cb_group)) {
      pl_impfun <<- do_impfun_plot(dat_to_plot %>% filter(cb_group == cb), poll, save = TRUE)
      pl_iams <<- do_iams_plot(dat_to_plot %>% filter(cb_group == cb), poll, save = TRUE)
      
      if (legend) {
        pl_joint = ggpubr::ggarrange(pl_iams + labs(title = 'a1'), pl_impfun + labs(title = 'a2'),
                                     ncol=2, common.legend = T,legend = "bottom")
      } else {
        pl_joint = ggpubr::ggarrange(pl_iams + labs(title = 'a1'), pl_impfun + labs(title = 'a2'),
                                     ncol=2, common.legend = T,legend = "none")
      }
      
      pl = cowplot::ggdraw() +
        theme(plot.background = element_rect(fill="white")) +
        cowplot::draw_plot(pl_joint, x = 0.01, y = 0, width = 0.99, height = 0.95) +
        cowplot::draw_plot_label(label = "IAMs VS RR function global uncertainty", size = 15,
                                 x = -0.175, y = 0.99)
      
      pl_name = ifelse(cb == '<1000', paste0('Results/Mort/IAMsVSImpFun/iamsVSrr_cbL_',poll),
                       ifelse(cb == '[1000,2000]', paste0('Results/Mort/IAMsVSImpFun/iamsVSrr_cbM_',poll),
                              paste0('Results/Mort/IAMsVSImpFun/iamsVSrr_cbH_',poll)))
      print(pl_name)
      ggsave(file=paste0(pl_name,'.png'), width = 300, height = 180, units = "mm",plot = pl, limitsize = FALSE)
      ggsave(file=paste0(pl_name,'.pdf'), width = 250, height = 180, units = "mm",plot = pl, limitsize = FALSE)
      
    }
  }
  return(pl)
}
