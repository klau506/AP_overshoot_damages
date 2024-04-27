################################################################################
#                                DISTRIB PLOT                                  #
################################################################################

#datIni = data to plot
#reg = region
#yrem = remove OY facet titles if TRUE
doEcon_distrib_plot = function(datIni,reg,yrem) {
  datIni = data.table(datIni)
  dat_tmp = datIni[, .(medi = quantile(value, 0.5)), #median
                       by=c('scenario','cb_group','pollutant','year',
                            'method','alpha')]
  datall = merge(dat_tmp, datIni, by=c('scenario','cb_group','pollutant','year',
                                           'method','alpha'))
  
  datall = data.table(datall)
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  pl_title = reg
  
  round_num <<- 1
  pl <- ggplot(datall) +
    geom_density(aes(x = value, group = interaction(scenario,alpha), color = interaction(scenario,alpha),
                     fill = interaction(scenario,alpha)), alpha = 0.5) +
    geom_vline(data = datall[alpha == 'med'], aes(color = interaction(scenario,alpha), xintercept = medi),
               linetype="dashed", linewidth = 1) +
    facet_grid(cb_group+method ~ year, scales='free')+
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black', size = 12), strip.background = element_rect(fill = '#e8e8e8'),
          strip.text.y = element_text(size = 10)) +
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_manual(values = longpal_alpha_colors,
                      name = 'Climate policy design\nand elasticity',
                      labels = longlabs.alphaCI)+
    scale_color_manual(values = longpal_alpha_colors,
                       name = 'Climate policy design\nand elasticity',
                       labels = longlabs.alphaCI)+
    labs(title='', x = 'US Billion', y = "Probability density")
  pl
  if (yrem) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  
  return(pl)
}


################################################################################
#                                CUM FREQ PLOT                                 #
################################################################################
#datIni = data to plot
#reg = region
#yrem = remove OY facet titles if TRUE
doEcon_cumm_plot = function(datIni,reg,yrem) {
  dat_tmp <- plyr::ddply(datIni, .(scenario,cb_group,method,alpha,year), summarize,
                         value = unique(value),
                         ecdf = ecdf(value)(unique(value)))
  
  # dat_tmp = data.table(dat_tmp)
  dat_tmp$cb_group <- factor(dat_tmp$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  # 
  pl_title = reg
  
  round_num <<- 1
  pl <- ggplot(dat_tmp, aes(value, ecdf, color = interaction(scenario,alpha), fill = interaction(scenario,alpha))) +
    geom_line(linewidth = 1.5) +
    ggh4x::facet_grid2(cb_group + method ~ year, scales = "free", independent = "all",
                       labeller = labeller(method = method_av.labs)) +
    scale_fill_manual(values = longpal_alpha_colors,
                      name = 'Climate policy design\nand elasticity',
                      labels = longlabs.alphaCI)+
    scale_color_manual(values = longpal_alpha_colors,
                       name = 'Climate policy design\nand elasticity',
                       labels = longlabs.alphaCI)+
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black', size = 12), strip.background = element_rect(fill = '#e8e8e8'),
          strip.text.y = element_text(size = 10)) +
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5) +
    scale_x_continuous(labels = scales::comma) +
    labs(title='', x = 'US Billion', y = "Cumulative frequency")
  pl
  
  if (yrem) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  return(pl)
}


##############################################################################
#                                WHOLE FIGURE                                #
##############################################################################

Econ_cum_distrib_plot = function(datIni,reg,slides) {
  datIni = datIni |> filter(year %in% c(2030,2050) & region == reg)
  
  pl_d = doEcon_distrib_plot(datIni,reg,yrem = TRUE)
  pl_c = doEcon_cumm_plot(datIni,reg,yrem = FALSE) + 
    rotate_y_facet_text(angle = 270, align = 0.5, valign = 0.5)

  fig = ggarrange(pl_d + font("title", size = 8),
                  pl_c + font("title", size = 8),
                  ncol = 2, nrow = 1,
                  common.legend = TRUE, legend="bottom",
                  widths = c(1,1.1))
  
  if (!slides) {
    fig = annotate_figure(fig,
                          top = text_grob(do_rename_regions_string(reg), color = "black", face = "bold", size = 14))
  }
  
  if (!slides) {
    pl_title = paste('cum_freq_AND_distrib',reg,sep='_')
    name_file_E = paste0("Results/Econ/CumAndDistrib/",pl_title,'.pdf')
    ggsave(file=name_file_E, width = 400, height = 500, units = "mm",plot = fig, limitsize = FALSE)
    name_file_E = paste0("Results/Econ/CumAndDistrib/",pl_title,'.png')
    ggsave(file=name_file_E, width = 400, height = 500, units = "mm",plot = fig, limitsize = FALSE)
  } else {
    pl_title = paste('Econ_CumAndDistrib_cum_freq_AND_distrib',reg,sep='_')
    name_file_E = paste0("Results/Slides/",pl_title,'.pdf')
    ggsave(file=name_file_E, width = 400, height = 250, units = "mm",plot = fig, limitsize = FALSE)
    name_file_E = paste0("Results/Slides/",pl_title,'.png')
    ggsave(file=name_file_E, width = 400, height = 250, units = "mm",plot = fig, limitsize = FALSE)
  }
  
}

doEcon_FIGURE_distrib_cum = function(datIni,slides) {
  for(r in unique(datIni$region)) {
    # restrict to NZ and EoC
    datIni = datIni |> filter(scenario != 'REF')
    
    print(r)
    Econ_cum_distrib_plot(datIni,r,slides)
  }
}

doEcon_DistribCum_folder = function() {
  if(!dir.exists(file.path('Results/Econ/CumAndDistrib'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Econ/'))){
      dir.create(file.path('Results/Econ/'))
    }
    dir.create(file.path('Results/Econ/CumAndDistrib'))
  }
}