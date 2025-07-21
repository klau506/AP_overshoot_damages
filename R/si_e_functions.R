source('si_e_c_functions.R')

doEmiss_CumDistrib_folder = function() {
  if(!dir.exists(file.path('Results/Emiss/CumAndDistrib'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Emiss/'))){
      dir.create(file.path('Results/Emiss/'))
    }
    dir.create(file.path('Results/Emiss/CumAndDistrib'))
  }
}

###########################################################################################
#                               DISTRIB & CUMULATIVE PLOT                                 #
###########################################################################################

E_cum_distrib_plot_single_poll = function(datt,reg,poll,save) {
  pl_c = do_cumm_plot(datt,reg,poll,T,T,expression(paste('Emissions [kg/year]')),T)
  pl_d = do_distrib_plot(datt,reg,poll,T,F,expression(paste('Emissions [kg/year]')),T)
  
  fig = ggarrange(pl_d + font("title", size = 8),
                  pl_c + font("title", size = 8),
                  labels = ifelse(save, c(poll,''), ''),
                  ncol = 2, nrow = 1,
                  common.legend = TRUE, legend=ifelse(save | poll == 'VOC',"bottom","none"),
                  widths = c(1,1.15))
  
  if(save) {
    fig = annotate_figure(fig,
                          top = text_grob(paste0("Emissions probability distribution and cumulative frequency of ",reg), color = "black", face = "bold", size = 14))
    
    if(!dir.exists(file.path('Results/Emiss/CumAndDistrib'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Emiss/'))){
        dir.create(file.path('Results/Emiss/'))
      }
      dir.create(file.path('Results/Emiss/CumAndDistrib'))
    }
    
    pl_title = paste('cum_freq_AND_distrib',paste(reg,poll,sep='_'),sep='_')
    name_file = paste("Results/Emiss/CumAndDistrib",paste(pl_title,'pdf',sep='.'),sep='/')
    ggsave(file=name_file, width = 200, height = 100, units = "mm",plot = fig, limitsize = FALSE)
  }
  else {
    return(fig)
  }
}

doE_FIGURE_cum_distrib = function(dat) {
  doEmiss_CumDistrib_folder()
  for(r in unique(dat$region)) {
    print(r)
    dat = dat[year %in% c(2030,2050) & scenario != 'REF']
    
    pl_nox = E_cum_distrib_plot_single_poll(dat,r,'NOx',F)
    pl_so2 = E_cum_distrib_plot_single_poll(dat,r,'SO2',F)
    pl_bcoc = E_cum_distrib_plot_single_poll(dat,r,'BC+OC',F)
    pl_voc = E_cum_distrib_plot_single_poll(dat,r,'VOC',F)
    
    fig = ggarrange(pl_nox + rremove("ylab") + font("title", size = 8),
                    pl_bcoc + rremove("ylab") + font("title", size = 8),
                    pl_so2 + rremove("ylab") + font("title", size = 8),
                    pl_voc + rremove("ylab") + rremove("xlab") + font("title", size = 8),
                    heights = c(1,1,1,1.165),
                    labels = c("NOx", 'BC+OC', "SO2", "VOC"),
                    hjust = c(-0.5,-0.25,-0.5,-0.5),
                    ncol = 1, nrow = 4,
                    common.legend = TRUE, legend="bottom")
    
    fig = annotate_figure(fig,
                          top = text_grob(do_rename_regions_string(r), color = "black", face = "bold", size = 14))

    pl_title = paste('cum_freq_AND_distrib_alltogehter',r,sep='_')
    name_file = paste("Results/Emiss/CumAndDistrib",paste(pl_title,'pdf',sep='.'),sep='/')
    ggsave(file=name_file, width = 200, height = 310, units = "mm",plot = fig, limitsize = FALSE)
  }
}

###########################################################################################
#                                       P-VALUE PLOT                                      #
###########################################################################################

doE_ks_test_plot = function(dat) {
  dat = dat[year %in% c(2030,2050) & scenario != 'REF' ]
  ks_E_test = ks_testt(dat)
  ks_E_test = ks_E_test[!region %in% c('WORLD','R10WORLD')]
  ks_E_test = ks_E_test %>%
    dplyr::mutate(region = sapply(ks_E_test$region, do_rename_regions_string))
  ks_E_test$cb_group <- factor(ks_E_test$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  pl_E = ggplot(data = ks_E_test, aes(x = cb_group, y = p.value, group = pollutant,
                                      color = region)) +
    geom_point( aes(shape = pollutant), position=position_dodge(width = .5), alpha = 0.8, size = 3) +
    geom_hline(yintercept = 0.05, color = 'red', linetype = 'dashed', alpha = 0.5) +
    facet_grid(. ~ year) +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black', size = 12), strip.background = element_rect(fill = '#e8e8e8')) +
    scale_shape_manual(values = c(15,16,17,18),
                       name = 'Pollutant') +
    scale_color_manual(values = regions.colors,
                       name = 'Regions') +
    labs(x = 'Carbon budget', y = "p-value") +
    guides(color = guide_legend(nrow = 2, order = 1), shape = guide_legend(order = 2))
  
           
  if(!dir.exists(file.path('Results/Emiss/KS_test'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Emiss/'))){
      dir.create(file.path('Results/Emiss/'))
    }
    dir.create(file.path('Results/Emiss/KS_test'))
  }
  
  name_file_E = paste("Results/Emiss/KS_test",paste('ks_test','pdf',sep='.'),sep='/')
  ggsave(file=name_file_E, width = 250, height = 180, units = "mm",plot = pl_E, limitsize = FALSE)
  
  # K-S figure for SI
  dir.create(file.path('paper_figures/SI/ks'), showWarnings = FALSE)
  ggsave(file=file.path(paste0('paper_figures/SI/ks/ks_emiss_2030_2050.pdf')), plot = pl_E, width = 400, height = 225, unit = 'mm')
  
}