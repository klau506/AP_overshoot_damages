doEcon_elast_ci_sens_plot_by_reg = function(datIni,reg,poll,xx,yy) {
  
  pl_title = reg
  tt = ''
  
  dat <- data.table(datIni)[year %in% c(2030,2050) & region == reg & pollutant == poll,
                            .(cmed = quantile(value, 0.5),
                              c05  = quantile(value, 0.05),
                              c95  = quantile(value, 0.95),
                              mmax  = max(value),
                              mmin  = min(value),
                              mmean  = mean(value)),
                            by = c('year','region','impact_function_group','cb_group',
                                   'scenario', 'alpha','method')]
  
  dat[alpha == "lo", alpha_label := "low"]
  dat[alpha == "med", alpha_label := "medium"]
  dat[alpha == "hi", alpha_label := "high"]
  dat[, alpha := factor(alpha_label, levels = c("low", "medium", "high"))]
  
  # dat = do_rename_imp_fun_etal(dat)
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  dat = dat %>%
    mutate(impact_function_group = ifelse(method %in% c('dech_damage_avoided','dong_damage_avoided'),
                                          'Zero rr fun used', impact_function_group))
  dat = do_rename_meth_etal(dat)
  dat$meth_label = fct_rev(dat$meth_label)
  
  dat[impact_function_group == "BRUNETT2018_gWITH", imp_fun_label := "Brunett et al.\n(with) (2018) [49]"]
  dat[impact_function_group == "BRUNETT2018_gOUT", imp_fun_label := "Brunett et al.\n(without) (2018) [49]"]
  dat[impact_function_group == "BURNETT2014_gUNI", imp_fun_label := "Burnett et al. (2014) [50]"]
  dat[impact_function_group == "GBD2016_gLO", imp_fun_label := "GBD (low) (2016) [56]"]
  dat[impact_function_group == "GBD2016_gMED", imp_fun_label := "GBD (medium) (2016) [56]"]
  dat[impact_function_group == "GBD2016_gHI", imp_fun_label := "GBD (high) (2016) [56]"]
  dat[impact_function_group == "Zero rr fun used", imp_fun_label := "No RR function\nused"]
  dat[, impact_function_group := factor(imp_fun_label, levels = c("Brunett et al.\n(with) (2018) [49]",
                                                                  "Brunett et al.\n(without) (2018) [49]",
                                                                  "Burnett et al. (2014) [50]",
                                                                  "GBD (low) (2016) [56]",
                                                                  "GBD (medium) (2016) [56]",
                                                                  "GBD (high) (2016) [56]",
                                                                  "No RR function\nused"
  ))]
  
  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = meth_label,
                      group = interaction(scenario,alpha,impact_function_group),
                      linetype = alpha,
                      color = impact_function_group),
                  linewidth = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.75)) +
    geom_point(aes(x = cmed,
                   y = meth_label,
                   group = interaction(scenario,alpha,impact_function_group),
                   shape = scenario),
               position = position_dodge(width = 0.75),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = meth_label,
                   group = interaction(scenario,alpha,impact_function_group),
                   color = impact_function_group,
                   shape = scenario),
               position = position_dodge(width = 0.75),
               size = 1.5) +
    facet_grid(cb_group ~ year, scales='free')+
    theme_light() +
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 10)) +
    scale_x_continuous(labels = scales::comma) +
    scale_shape_manual(values = c(16,17),
                       name = 'Climate policy\ndesign')+
    scale_color_brewer(palette = "Set1",
                       name = 'RR function') +
    scale_linetype_manual(values = c('low' = 'dotted', 'medium' = 'solid', 'high' = 'dashed'),
                          name = 'Elasticity') +
    labs(title=tt, x = 'US Billion', y = "")
  
  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  return(pl)
}


# by region
doEcon_FIGURE_Elast_CI_sens_by_region = function(dat) {
  for (r in unique(dat$region)) {
    # restric to NZ and EoC
    dat = dat |> filter(scenario != 'REF')
    
    print(r)
    pl_pm25 = doEcon_elast_ci_sens_plot_by_reg(dat,r,'PM25',T,T)

    fig = annotate_figure(pl_pm25,
                          top = text_grob(do_rename_regions_string(r), color = "black", face = "bold", size = 14))
    
    if(!dir.exists(file.path('Results/Econ/Elast_CI/ByRegion'))){
      if(!dir.exists(file.path('Results/'))){
        dir.create(file.path('Results/'))
      }
      if(!dir.exists(file.path('Results/Econ/'))){
        dir.create(file.path('Results/Econ/'))
      }
      if(!dir.exists(file.path('Results/Econ/Elast_CI/'))){
        dir.create(file.path('Results/Econ/Elast_CI/'))
      }
      dir.create(file.path('Results/Econ/Elast_CI/ByRegion'))
    }
    name_file_M = paste("Results/Econ/Elast_CI/ByRegion",paste(paste('Medians',r,sep='_'),'pdf',sep='.'),sep='/')
    ggsave(file=name_file_M, width = 220, height = 350, units = "mm",plot = fig, limitsize = FALSE)
  }
}
