
################################################################################
#                             CI LEVEL SENSITIVITY                             #
################################################################################

doM_ci_sens_plot_by_reg = function(datIni,reg,poll,xx,yy,save) {
  
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  pl_title = paste(poll,reg,sep='_')
  tt = ''
  
  dat <- datIni[t %in% c(2030,2050) & Regions == reg & pollutant == poll
                & z_level %in% c('zUNI','zMED'),
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('t','Regions','impact_function_group','cb_group','scen','ci_level')]
  
  dat[ci_level == "ciLO", ci_label := "5th"]
  dat[ci_level == "ciMED", ci_label := "median"]
  dat[ci_level == "ciHI", ci_label := "95th"]
  dat[, ci_level := factor(ci_label, levels = c("5th",
                                                "median",
                                                "95th"))]
  dat = do_rename_imp_fun(dat,poll_names = F,short_names = F)
  dat$impact_function_group = fct_rev(dat$impact_function_group)
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = impact_function_group,
                      group = interaction(scen,ci_level),
                      color = ci_level),
                  size = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.4)) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,ci_level),
                   shape = scen),
               position = position_dodge(width = 0.4),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,ci_level),
                   color = ci_level,
                   shape = scen),
               position = position_dodge(width = 0.4),
               size = 1.5) +
    facet_grid(cb_group ~ t, scales='free')+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 8)) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    guides(x =  guide_axis(n.dodge = 2)) +
    scale_shape_manual(values = c(16,17),
                       name = 'Scenario')+
    scale_color_brewer(palette = "Set1",
                       name = 'Impact function\npercentile') +
    labs(title=tt, x = 'Deaths', y = "Impact functions")
  
  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if(poll == 'PM25'){
    pl = pl + guides(x =  guide_axis(n.dodge = 2))
  }
  
  if(save) {
    if(!dir.exists(file.path('Results/Mort/CI/ByPoll'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Mort/'))){
        dir.create(file.path('Results/Mort/'))
      }
      if(!dir.exists(file.path('Results/Mort/CI/'))){
        dir.create(file.path('Results/Mort/CI/'))
      }
      dir.create(file.path('Results/Mort/CI/ByPoll'))
    }
    h = as.integer(length(unique(dat$impact_function_group)))*37.5
    name_file_M = paste("Results/Mort/CI/ByPoll",paste(paste('Medians',pl_title,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 400, height = h, units = "mm",plot = pl, limitsize = FALSE)
  }
  else {
    return(pl)
  }
}


# by region and poll
doM_FIGURE_CI_sens_by_region_pollutant = function(dat) {
  for (reg in unique(dat$Regions)) {
    for (poll in unique(dat$pollutant)) {
      # restric to NZ and EoC
      dat = dat |> filter(scen != 'REF')
      
      doM_ci_sens_plot_by_reg(dat,reg,poll,T,T,T)
    }
  }
}

# by region
doM_FIGURE_CI_sens_by_region = function(dat) {
  for (r in unique(dat$Regions)) {
    # restric to NZ and EoC
    dat = dat |> filter(scen != 'REF')
    
    print(r)
    pl_pm25 = doM_ci_sens_plot_by_reg(dat,r,'PM25',T,T,F)
    pl_o3 = doM_ci_sens_plot_by_reg(dat,r,'O3',T,T,F)
    
    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    heights = c(1,0.3),
                    common.legend = TRUE, legend="right")
    
    fig = annotate_figure(fig,
                          top = text_grob(do_rename_regions_string(r), color = "black", face = "bold", size = 14))
    
    if(!dir.exists(file.path('Results/Mort/CI/ByRegion'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Mort/'))){
        dir.create(file.path('Results/Mort/'))
      }
      if(!dir.exists(file.path('Results/Mort/CI/'))){
        dir.create(file.path('Results/Mort/CI/'))
      }
      dir.create(file.path('Results/Mort/CI/ByRegion'))
    }
    name_file_M = paste("Results/Mort/CI/ByRegion",paste(paste('Medians',r,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 325, units = "mm",plot = fig, limitsize = FALSE)
  }
}

################################################################################
#                                  ZCF SENSITIVITY                             #
################################################################################

doM_zcf_sens_plot_by_reg = function(datIni,reg,poll,xx,yy,save) {
  
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  pl_title = paste(poll,reg,sep='_')
  tt = ''
  
  dat <- datIni[t %in% c(2030,2050) & Regions == reg & pollutant == poll
                & ci_level %in% c('ciMED','ciUNI') & z_level != 'zUNI',
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('t','Regions','impact_function_group','cb_group',
                       'scen','z_level')]
  
  dat[z_level == "zLO", z_label := '5th']
  dat[z_level == "zMED", z_label := 'mean']
  dat[z_level == "zHI", z_label := '95th']
  dat[, z_level := factor(z_label, levels = c('5th','mean','95th'))]
  
  dat = do_rename_imp_fun(dat,poll_names = F,short_names = F)
  dat$impact_function_group = fct_rev(dat$impact_function_group)
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = impact_function_group,
                      group = interaction(scen,z_level),
                      color = z_level),
                  size = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,z_level),
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = impact_function_group,
                   group = interaction(scen,z_level),
                   color = z_level,
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 1.5) +
    facet_grid(cb_group ~ t, scales='free')+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 8)) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    guides(x = guide_axis(n.dodge = 2)) +
    scale_shape_manual(values = c(16,17),
                       name = 'Scenario')+
    scale_color_brewer(palette = "Set2",
                       name = 'TMREL\npercentile') +
    labs(title=tt, x = 'Deaths', y = "Impact functions")
  
  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if(poll == 'PM25'){
    pl = pl + guides(x =  guide_axis(n.dodge = 2))
  }
  
  
  if(save) {
    if(!dir.exists(file.path('Results/Mort/ZCF/ByPoll'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Mort/'))){
        dir.create(file.path('Results/Mort/'))
      }
      if(!dir.exists(file.path('Results/Mort/ZCF/'))){
        dir.create(file.path('Results/Mort/ZCF/'))
      }
      dir.create(file.path('Results/Mort/ZCF/ByPoll'))
    }
    h = as.integer(length(unique(dat$impact_function_group)))*37.5
    name_file_M = paste("Results/Mort/ZCF/ByPoll",paste(paste('Medians',pl_title,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 250, height = h, units = "mm",plot = pl, limitsize = FALSE)
  }
  else {
    return(pl)
  }
}

# by region and poll
doM_FIGURE_ZCF_sens_by_region_pollutant = function(dat) {
  for (reg in unique(dat$Regions)) {
    for (poll in unique(dat$pollutant)) {
      # restric to NZ and EoC
      dat = dat |> filter(scen != 'REF')
      
      doM_zcf_sens_plot_by_reg(dat,reg,poll,T,T,T)
    }
  }
}

# by region
doM_FIGURE_ZCF_sens_by_region = function(dat) {
  for (r in unique(dat$Regions)) {
    # restric to NZ and EoC
    dat = dat |> filter(scen != 'REF')
    
    print(r)
    pl_pm25 = doM_zcf_sens_plot_by_reg(dat,r,'PM25',T,T,F)
    pl_o3 = doM_zcf_sens_plot_by_reg(dat,r,'O3',T,T,F)
    
    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    heights = c(1,0.3),
                    common.legend = TRUE, legend="right")
    fig = annotate_figure(fig,
                          top = text_grob(do_rename_regions_string(r), color = "black", face = "bold", size = 14))
    
    if(!dir.exists(file.path('Results/Mort/ZCF/ByRegion'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Mort/'))){
        dir.create(file.path('Results/Mort/'))
      }
      if(!dir.exists(file.path('Results/Mort/ZCF/'))){
        dir.create(file.path('Results/Mort/ZCF/'))
      }
      dir.create(file.path('Results/Mort/ZCF/ByRegion'))
    }
    name_file_M = paste("Results/Mort/ZCF/ByRegion",paste(paste('Medians',r,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 325, units = "mm",plot = fig, limitsize = FALSE)
  }
}

################################################################################
#                               ZCF & CI SENSITIVITY                           #
################################################################################

doM_zcf_ci_sens_plot_by_reg = function(datIni,reg,poll,xx,yy,save) {
  
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  pl_title = paste(poll,reg,sep='_')
  tt = ''
  
  dat <- datIni[t %in% c(2030,2050) & Regions == reg & pollutant == poll,
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('t','Regions','impact_function_group','cb_group',
                       'scen','ci_level','z_level')]
  
  dat[ci_level == "ciLO", ci_label := "2.5th"]
  dat[ci_level == "ciMED", ci_label := "50th"]
  dat[ci_level == "ciHI", ci_label := "97.5th"]
  dat[, ci_level := factor(ci_label, levels = c("2.5th",
                                                "50th",
                                                "97.5th"))]
  dat[z_level == "zLO", z_label := '2.5th']
  dat[z_level == "zMED", z_label := '50th']
  dat[z_level == "zHI", z_label := '97.5th']
  dat[z_level == "zUNI", z_label := 'No ZCF\nuncertainty']
  dat[, z_level := factor(z_label, levels = c('2.5th','50th','97.5th','No ZCF\nuncertainty'))]
  
  dat = do_rename_imp_fun_etal(dat)
  if (poll == 'PM25') {
    dat$imp_fun_label = fct_rev(factor(dat$imp_fun_label, levels = 
                                         c("Cohen et al. (2005) [51]", "Krewski et al. (2009) [55]",
                                           "Burnett et al. (2014) [50]",  "GBD (low) (2015) [56]", 
                                           "GBD (medium) (2015) [56]","GBD (high) (2015) [56]", 
                                           "Burnett et al.\n(with) (2018) [49]", "Burnett et al.\n(without) (2018) [49]")))
  } else {
    dat$imp_fun_label = fct_rev(factor(dat$imp_fun_label, levels = 
                                         c('Jerrett et al. (2009) [41]','GBD (2015) [56]')))
  }
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = imp_fun_label,
                      group = interaction(scen,ci_level,z_level),
                      color = ci_level,
                      linetype = z_level),
                  size = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(x = cmed,
                   y = imp_fun_label,
                   group = interaction(scen,ci_level,z_level),
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = imp_fun_label,
                   group = interaction(scen,ci_level,z_level),
                   color = ci_level,
                   shape = scen),
               position = position_dodge(width = 0.5),
               size = 1.5) +
    facet_grid(cb_group ~ t, scales='free')+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 8)) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +
    scale_shape_manual(values = c(16,17),
                       name = 'Climate policy\ndesign')+
    scale_color_brewer(palette = "Set1",
                       name = 'Parameter\npercentile') +
    scale_linetype_manual(values = c('dotted','solid','44','3313'),
                          name = 'ZCF\npercentile') +
    labs(title=tt, x = 'Premature deaths [million people/year]', y = "")
  
  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }

  if(save) {
    if(!dir.exists(file.path('Results/Mort/ZCF_CI/ByPoll'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Mort/'))){
        dir.create(file.path('Results/Mort/'))
      }
      if(!dir.exists(file.path('Results/Mort/ZCF_CI/'))){
        dir.create(file.path('Results/Mort/ZCF_CI/'))
      }
      dir.create(file.path('Results/Mort/ZCF_CI/ByPoll'))
    }
    h = as.integer(length(unique(dat$impact_function_group)))*37.5
    name_file_M = paste("Results/Mort/ZCF_CI/ByPoll",paste(paste('Medians',pl_title,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 300, height = h, units = "mm",plot = pl, limitsize = FALSE)
  }
  else {
    return(pl)
  }
}

# by region and poll
doM_FIGURE_ZCF_CI_sens_by_region_pollutant = function(dat) {
  for (reg in unique(dat$Regions)) {
    for (poll in unique(dat$pollutant)) {
      # restric to NZ and EoC
      dat = dat |> filter(scen != 'REF')
      
      doM_zcf_ci_sens_plot_by_reg(dat,reg,poll,T,T,T)
    }
  }
}

# by region
doM_FIGURE_ZCF_CI_sens_by_region = function(dat) {
  for (r in unique(dat$Regions)) {
    # restric to NZ and EoC
    dat = dat |> filter(scen != 'REF')
    
    print(r)
    pl_pm25 = doM_zcf_ci_sens_plot_by_reg(dat,r,'PM25',T,T,F)
    pl_o3 = doM_zcf_ci_sens_plot_by_reg(dat,r,'O3',T,T,F)
    
    fig = ggarrange(pl_pm25 + rremove("ylab") + font("title", size = 8),
                    pl_o3 + rremove("ylab") + font("title", size = 8),
                    labels = c("PM25", 'O3'),
                    ncol = 1, nrow = 2,
                    heights = c(1,0.3),
                    common.legend = TRUE, legend="right")
    fig = annotate_figure(fig,
                          top = text_grob(do_rename_regions_string(r), color = "black", face = "bold", size = 14))
    
    if(!dir.exists(file.path('Results/Mort/ZCF_CI/ByRegion'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Mort/'))){
        dir.create(file.path('Results/Mort/'))
      }
      if(!dir.exists(file.path('Results/Mort/ZCF_CI/'))){
        dir.create(file.path('Results/Mort/ZCF_CI/'))
      }
      dir.create(file.path('Results/Mort/ZCF_CI/ByRegion'))
    }
    name_file_M = paste("Results/Mort/ZCF_CI/ByRegion",paste(paste('Medians',r,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 375, units = "mm",plot = fig, limitsize = FALSE)
  }
}
