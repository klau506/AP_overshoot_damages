do_iams_vs_meth_pre = function(datIni) {
  dat_to_plot = datIni |> filter(year %in% c(2030,2050) & scenario != 'REF',
                                 alpha == 'med', parameter == 'ciMED', cf %in% c('zMED','zUNI'))
  
  dat_medi_iams <- data.table(dat_to_plot)[, .(medi_iams = quantile(value, 0.5)), #median
                                           by=c('scenario','cb_group','year','model')]
  dat_medi_meth <- data.table(dat_to_plot)[, .(medi_meth = quantile(value, 0.5)), #median
                                             by=c('scenario','cb_group','year','method')]
  dat = merge(dat_to_plot, dat_medi_iams, by=c('scenario','cb_group','year','model'))
  datall = merge(dat, dat_medi_meth, by=c('scenario','cb_group','year','method'))
  
  datall = data.table(datall)
  datall = do_rename_models(datall)
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  return(datall)
}

do_iams_plot = function(datIni, save) {
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
          axis.text = element_text(size = 12),
          strip.background = element_rect(fill = 'white', color = 'white')) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_manual(values = scenario.colors,
                      name = 'Climate\npolicy') +
    scale_color_manual(values = scenario.colors,
                       name = 'Climate\npolicy') +
    labs(title='', x = 'US Billion', y = "Probability density\n")
  
  if(save) {
    h = as.integer(length(unique(datIni$impact_function_group)))*37.5
    name_file_iams = ifelse(cb == '<1000', 'Results/Econ/IAMsVSMeth/iams/iams_cbL',
                            ifelse(cb == '[1000,2000]', 'Results/Econ/IAMsVSMeth/iams/iams_cbM',
                                   'Results/Econ/IAMsVSMeth/iams/iams_cbH'))
    print(name_file_iams)
    ggsave(file=paste0(name_file_iams,'.png'), width = 300, height = h, units = "mm",plot = pl_iams, limitsize = FALSE)
    ggsave(file=paste0(name_file_iams,'.pdf'), width = 300, height = h, units = "mm",plot = pl_iams, limitsize = FALSE)
  }
  return(pl_iams)
}

do_meth_plot = function(datIni, save) {
  pl_meth <- ggplot(datIni) +
    geom_density(aes(x = value, group = scenario,
                     color = scenario,
                     fill = scenario), alpha = 0.5) +
    geom_vline(aes(color = scenario, xintercept = medi_meth),
               linetype="dashed", linewidth = 1) +
    facet_grid(method ~ year, scales='free',
               labeller = labeller(method = method_av.labs)) +
    theme_light() +
    theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.position = "bottom", strip.text = element_text(color = "black"),
          strip.text.y = element_text(size = 13, angle = 0, hjust = 0), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(), plot.background = element_rect(fill = 'white'), 
          axis.text = element_text(size = 12),
          strip.background = element_rect(fill = 'white', color = 'white')) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_manual(values = scenario.colors,
                      name = 'Climate\npolicy')+
    scale_color_manual(values = scenario.colors,
                       name = 'Climate\npolicy')+
    labs(title='', x = 'US Billion', y = "Probability density\n")
  
  if(save) {
    h = as.integer(length(unique(datIni$impact_function_group)))*37.5
    name_file_meth = ifelse(cb == '<1000', 'Results/Econ/IAMsVSMeth/meth/meth_cbL',
                              ifelse(cb == '[1000,2000]', 'Results/Econ/IAMsVSMeth/meth/meth_cbM',
                                     'Results/Econ/IAMsVSMeth/meth/meth_cbH'))
    print(name_file_meth)
    ggsave(file=paste0(name_file_meth,'.png'), width = 300, height = h, units = "mm",plot = pl_meth, limitsize = FALSE)
    ggsave(file=paste0(name_file_meth,'.pdf'), width = 300, height = h, units = "mm",plot = pl_meth, limitsize = FALSE)
  }
  return(pl_meth)
}


# do figure by region
doEcon_iams_meth = function(datIni, legend = TRUE) {
  dat_to_plot = do_iams_vs_meth_pre(datIni)
  
  # CI of medi_iams (median across IAMs)
  ci_medi_iams <- dat_to_plot %>% 
    dplyr::select(scenario, cb_group, year, model_label, medi_iams) %>% 
    unique()
  ci_medi_iams_quantiles <- c(min(ci_medi_iams$medi_iams), median(ci_medi_iams$medi_iams), max(ci_medi_iams$medi_iams))
  print("The min-max for the medians accross IAMs is ")
  print(ci_medi_iams_quantiles)
  
  # CI of medi_meth (median across damage functions)
  ci_medi_meth <- dat_to_plot %>% 
    dplyr::select(scenario, cb_group, year, method, medi_meth) %>% 
    unique()
  ci_medi_meth_quantiles <- c(min(ci_medi_meth$medi_meth), median(ci_medi_meth$medi_meth), max(ci_medi_meth$medi_meth))
  print("The min-max for the medians accross damage functions is ")
  print(ci_medi_meth_quantiles)
  
  
  for (cb in unique(as.character(dat_to_plot$cb_group))) {
    pl_meth <<- do_meth_plot(dat_to_plot %>% filter(cb_group == cb), save = TRUE)
    pl_iams <<- do_iams_plot(dat_to_plot %>% filter(cb_group == cb), save = TRUE)
    
    if (legend) {
      pl_joint = ggpubr::ggarrange(pl_iams + labs(title = 'a1'), pl_meth + labs(title = 'a2'),
                                   ncol=2, common.legend = T,legend = "bottom")
    } else {
      pl_joint = ggpubr::ggarrange(pl_iams + labs(title = 'a1'), pl_meth + labs(title = 'a2'),
                                   ncol=2, common.legend = T,legend = "none")
    }
    
    pl = cowplot::ggdraw() +
      theme(plot.background = element_rect(fill="white")) +
      cowplot::draw_plot(pl_joint, x = 0.01, y = 0, width = 0.99, height = 0.95) +
      cowplot::draw_plot_label(label = "IAMs VS meth function global uncertainty", size = 15,
                               x = -0.175, y = 0.99)
    
    pl_name = ifelse(cb == '<1000', 'Results/Econ/IAMsVSMeth/iamsVSmeth_cbL',
                     ifelse(cb == '[1000,2000]', 'Results/Econ/IAMsVSMeth/iamsVSmeth_cbM',
                            'Results/Econ/IAMsVSMeth/iamsVSmeth_cbH'))
    print(pl_name)
    ggsave(file=paste0(pl_name,'.png'), width = 300, height = 180, units = "mm",plot = pl, limitsize = FALSE)
    ggsave(file=paste0(pl_name,'.pdf'), width = 250, height = 180, units = "mm",plot = pl, limitsize = FALSE)
    
    return(pl)
  }
}
