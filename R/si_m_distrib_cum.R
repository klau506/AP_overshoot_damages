
################################################################################
#                                DISTRIB PLOT                                  #
################################################################################

#datIni = data to plot: datEmi or datConc
#reg = R10 region
#poll = pollutant
#ylab = TRUE if the years should be displayed, FALSE otherwise
#cblab = TRUE if the cb_groups should be displayed, FALSE otherwise
#xxbb = OX label
doM_distrib_plot = function(datIni,reg,poll,ylab,cblab,xxbb) {
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  dat_to_plot = datIni |> filter(t %in% c(2030,2050) & Regions == reg &
                                   pollutant == poll & scen != 'REF') %>%
    as.data.table()
  
  dat_kl = dat_to_plot[, .(medi = quantile(value, 0.5)), #median
                       by=c('scen','cb_group','pollutant','t',
                            'impact_function_group','ci_z_level')]
  datall = merge(dat_kl, dat_to_plot, by=c('scen','cb_group','pollutant','t',
                                           'impact_function_group','ci_z_level'))
  
  datall = data.table(datall)
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  datall$impact_function_group <- factor(datall$impact_function_group, 
                                         levels = c("PM25MORT_BURNETT2014_UNI",
                                                    "PM25MORT_GBD2016_LO",
                                                    "PM25MORT_GBD2016_MED",
                                                    "PM25MORT_GBD2016_HI",
                                                    "PM25MORT_BRUNETT2018_OUT",
                                                    "PM25MORT_BRUNETT2018_WITH"
                                         ))
  
  pl_title = paste(poll,reg,sep='_')
  
  round_num <<- 9
  pl <- ggplot(datall) +
    geom_density(aes(x = value, group = interaction(scen,ci_z_level),
                     color = interaction(scen,ci_z_level),
                     fill = interaction(scen,ci_z_level)), alpha = 0.5) +
    geom_vline(data = datall[ci_z_level == '50th'], aes(color = interaction(scen,ci_z_level), xintercept = medi),
               linetype="dashed", linewidth = 1) +
    facet_grid(cb_group+impact_function_group ~ t, scales='free',
               labeller = labeller(impact_function_group = impact_function_group_2lines.nospace.labs))+
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +
    scale_fill_manual(values = longpal_impfun_colors_th,
                      name = 'Climate policy design and percentile ranges for\nparameter-counterfactual value estimates',
                      labels = longlabs.impfunCI) +
    scale_color_manual(values = longpal_impfun_colors_th,
                       name = 'Climate policy design and percentile ranges for\nparameter-counterfactual value estimates',
                       labels = longlabs.impfunCI) +
    labs(title='', x = xxbb, y = "Probability density") +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black'), strip.background = element_rect(fill = '#e8e8e8'),
          strip.text.y = element_text(size = 7))
  
  pl
  
  if(!ylab) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!cblab) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  return(pl)
}


################################################################################
#                                CUM FREQ PLOT                                 #
################################################################################
#datIni = data to plot: datEmi or datConc
#reg = R10 region
#poll = pollutant
#ylab = TRUE if the years should be displayed, FALSE otherwise
#cblab = TRUE if the cb_groups should be displayed, FALSE otherwise
#xxbb = OX label
doM_cumm_plot = function(datIni,reg,poll,ylab,cblab,xxbb) {
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  dat_to_plot = datIni |> filter(t %in% c(2030,2050) & Regions == reg
                                 & pollutant == poll & scen != 'REF')
  
  dat_kl2 <- ddply(dat_to_plot, .(scen,cb_group,pollutant,t,impact_function_group,ci_z_level), summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))
  
  dat_kl2 = data.table(dat_kl2)
  names(dat_kl2) = tolower(names(dat_kl2))
  dat_kl2$cb_group <- factor(dat_kl2$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  pl_title = paste(reg,poll,sep = '_')
  
  round_num <<- 1
  pl <- ggplot(dat_kl2, aes(x = value)) +
    geom_line(aes(y = ecdf, color = interaction(scen, ci_z_level)), na.rm = T) +
    facet_grid(cb_group+impact_function_group ~ t, scales='free',
               labeller = labeller(impact_function_group = impact_function_group_2lines.nospace.labs))+
    scale_fill_manual(values = longpal_impfun_colors_th,
                      name = 'Climate policy design and percentile ranges for\nparameter-counterfactual value estimates',
                      labels = longlabs.impfunCI)+
    scale_color_manual(values = longpal_impfun_colors_th,
                       name = 'Climate policy design and percentile ranges for\nparameter-counterfactual value estimates',
                       labels = longlabs.impfunCI)+
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5) +
    scale_y_continuous(breaks = custom_y_labels) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +
    labs(title='', x = xxbb, y = "Cumulative frequency") +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black'), strip.background = element_rect(fill = '#e8e8e8'),
          strip.text.y = element_text(size = 7))
  
  pl
  
  if(!ylab) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!cblab) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  return(pl)
}


##############################################################################
#                                WHOLE FIGURE                                #
##############################################################################

M_cum_distrib_plot = function(dat,reg) {
  dat = dat |> filter(t %in% c(2030,2050) & Regions == reg)
  
  pl_pm25_d = doM_distrib_plot(dat,reg,'PM25',T,T,'Premature deaths [million people/year]')
  pl_o3_d = doM_distrib_plot(dat,reg,'O3',T,T,'Premature deaths [million people/year]')
  pl_pm25_c = doM_cumm_plot(dat,reg,'PM25',T,T,expression(paste('Premature deaths [million people/year]')))
  pl_o3_c = doM_cumm_plot(dat,reg,'O3',T,T,expression(paste('Premature deaths [million people/year]')))
  
  fig_pm25 = ggarrange(pl_pm25_d + font("title", size = 8),
                       pl_pm25_c + font("title", size = 8),
                       labels = c('PM25',''),
                       ncol = 2, nrow = 1,
                       common.legend = TRUE, legend="bottom",
                       widths = c(1,1))
  
  fig_pm25 = annotate_figure(fig_pm25,
                             top = text_grob(do_rename_regions_string(reg), color = "black", face = "bold", size = 14))

  if(!dir.exists(file.path('Results/Mort/CumAndDistrib'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Mort/'))){
      dir.create(file.path('Results/Mort/'))
    }
    dir.create(file.path('Results/Mort/CumAndDistrib'))
  }
  
  pl_title = paste('cum_freq_AND_distrib',reg,sep='_')
  name_file_M = paste("Results/Mort/CumAndDistrib/",paste(paste(pl_title,'PM25',sep='_'),'pdf',sep='.'),sep='/')
  ggsave(file=name_file_M, width = 400, height = 500, units = "mm", plot = fig_pm25, limitsize = FALSE)
  
  fig_o3 = ggarrange(pl_o3_d + font("title", size = 8),
                     pl_o3_c + font("title", size = 8),
                     labels = c('O3',''),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend="bottom",
                     widths = c(1,1.1))
  
  fig_o3 = annotate_figure(fig_o3,
                           top = text_grob(do_rename_regions_string(reg), color = "black", face = "bold", size = 14))

  pl_title = paste('cum_freq_AND_distrib',reg,sep='_')
  name_file_M = paste("Results/Mort/CumAndDistrib/",paste(paste(pl_title,'O3',sep='_'),'pdf',sep='.'),sep='/')
  ggsave(file=name_file_M, width = 400, height = 150, units = "mm",plot = fig_o3, limitsize = FALSE)
}

doM_FIGURE_distrib_cum = function(dat) {
  for(r in unique(dat$Regions)) {
    # restrict to NZ and EoC
    dat = dat |> filter(scen != 'REF')
    
    print(r)
    M_cum_distrib_plot(dat,r)
  }
}

do_m_distrib_cum_folder = function() {
  if(!dir.exists(file.path('Results/Mort/CumAndDistrib'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Mort/'))){
      dir.create(file.path('Results/Mort/'))
    }
    dir.create(file.path('Results/Mort/CumAndDistrib'))
  }
}