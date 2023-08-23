source('si_e_c_functions.R')

###########################################################################################
#                               DISTRIB & CUMULATIVE PLOT                                 #
###########################################################################################

C_cum_distrib_plot_single_poll = function(dat,reg,poll,save) {
  xx = ifelse(poll == 'PM25', expression(paste('Concentration [',mu,g,m^-3,']')), 'Concentration [ppb]')
  pl_c = do_cumm_plot(dat,reg,poll,T,T,xx,F) + theme(axis.text.x = element_text(size = 8))
  pl_d = do_distrib_plot(dat,reg,poll,T,F,xx,F) + theme(axis.text.x = element_text(size = 8))
  
  fig = ggarrange(pl_d + font("title", size = 8),
                  pl_c + font("title", size = 8),
                  labels = ifelse(save, c(poll,''), ''),
                  ncol = 2, nrow = 1,
                  common.legend = TRUE, legend=ifelse(save | poll %in% c('VOC','O3'),"bottom","none"),
                  widths = c(1,1.15))
  
  if(save) {
    if(!dir.exists(file.path('Results/Conc/CumAndDistrib',version))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Conc/'))){
        dir.create(file.path('Results/Conc/'))
      }
      if(!dir.exists(file.path('Results/Conc/CumAndDistrib/'))){
        dir.create(file.path('Results/Conc/CumAndDistrib/'))
      }
      dir.create(file.path('Results/Conc/CumAndDistrib',version))
    }
    
    fig = annotate_figure(fig,
                          top = text_grob(reg, color = "black", face = "bold", size = 14))
                          # top = text_grob(paste0("Concentrations probability distribution and cumulative frequency of ",reg), color = "black", face = "bold", size = 14))
    
    pl_title = paste('cum_freq_AND_distrib',paste(reg,poll,sep='_'),sep='_')
    name_file_M = paste("Results/Conc/CumAndDistrib/",version,paste(paste(pl_title,'PM25',sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 200, height = 100, units = "mm",plot = fig_pm25, limitsize = FALSE)
  }
  else {
    return(fig)
  }
}


doC_FIGURE_cum_distrib = function(dat) {
  dat = dat[year %in% c(2030,2050) & scenario != 'REF' ]#& cb_group != 'REF']
  for(r in unique(dat$region)) {
    print(r)
    pl_pm25 = C_cum_distrib_plot_single_poll(dat,r,'PM25',F)
    pl_o3 = C_cum_distrib_plot_single_poll(dat,r,'O3',F)
    
    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend="bottom")
    fig = annotate_figure(fig,
                          top = text_grob(do_rename_regions_string(r), color = "black", face = "bold", size = 14))
                          # top = text_grob(paste0("Concentrations probability distribution and cumulative frequency of ",r), color = "black", face = "bold", size = 14))
    
    if(!dir.exists(file.path('Results/Conc/CumAndDistrib',version))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Conc/'))){
        dir.create(file.path('Results/Conc/'))
      }
      if(!dir.exists(file.path('Results/Conc/CumAndDistrib/'))){
        dir.create(file.path('Results/Conc/CumAndDistrib/'))
      }
      dir.create(file.path('Results/Conc/CumAndDistrib',version))
    }
    
    pl_title = paste('cum_freq_AND_distrib_4poll',r,sep='_')
    name_file = paste("Results/Conc/CumAndDistrib",version,paste(paste0(pl_title,'_alltogether'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file, width = 250, height = 175, units = "mm",plot = fig, limitsize = FALSE)
  }
}

###########################################################################################
#                                       P-VALUE PLOT                                      #
###########################################################################################

doC_ks_test_plot = function(dat) {
  dat = dat[year %in% c(2030,2050) & scenario != 'REF' ]
  ks_C_test = ks_testt(dat)
  ks_C_test$cb_group <- factor(ks_C_test$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  ks_C_test$pollutant <- factor(ks_C_test$pollutant, levels = c("PM25", "O3"))
  ks_C_test = ks_C_test[!region %in% c('WORLD','R10WORLD')]
  ks_C_test = ks_C_test %>%
    dplyr::mutate(region = sapply(ks_C_test$region, do_rename_regions_string))
  pl_C = ggplot(data = ks_C_test, aes(x = cb_group, y = p.value, group = pollutant,
                                      color = region)) +
    geom_point( aes(shape = pollutant), position=position_dodge(width = 0.3), alpha = 0.8, size = 3) +
    facet_grid(. ~ year) +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black', size = 12), strip.background = element_rect(fill = '#e8e8e8')) +
    geom_hline(yintercept = 0.05, color = 'red', linetype = 'dashed', alpha = 0.5) +
    scale_shape_manual(values = c(22,16),
                       name = 'Pollutant') +
    scale_color_manual(values = regions.colors,
                       name = 'Regions') +
    labs(x = 'Carbon budget', y = "p-value") +
    guides(color = guide_legend(nrow = 2)) +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))
  
  if(!dir.exists(file.path('Results/Conc/KS_test',version))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Conc/'))){
      dir.create(file.path('Results/Conc/'))
    }
    if(!dir.exists(file.path('Results/Conc/KS_test/'))){
      dir.create(file.path('Results/Conc/KS_test/'))
    }
    dir.create(file.path('Results/Conc/KS_test',version))
  }
  name_file_C = paste("Results/Conc/KS_test",version,paste('ks_test','pdf',sep='.'),sep='/')
  ggsave(file=name_file_C, width = 300, height = 180, units = "mm",plot = pl_C, limitsize = FALSE)
  
  # K-S figure for SI
  dir.create(file.path('paper_figures/SI/ks'), showWarnings = FALSE)
  ggsave(file=file.path(paste0('paper_figures/SI/ks/ks_conc_2030_2050.pdf')), plot = pl_C, width = 400, height = 225, unit = 'mm')
}