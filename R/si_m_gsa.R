
doM_gsa_preprocess = function(datIni) {
  
  data = datIni %>%
    dplyr::rename('impact_function' = impact_function_group) %>%
    dplyr::rename('parameter' = ci_level) %>%
    dplyr::rename('cf' = z_level) %>%
    dplyr::select(year,policy,carbon_budget,model,cb_group,region,
           impact_function,parameter,cf,scenario,
           value)

  items = c('model','impact_function','parameter','cf')
  
  sd_calc = NULL
  for (yr in unique(data$year)) {
    for (cb in unique(data$cb_group)) {
      for (it in items) {
        data_subset = data %>%
          dplyr::filter(year == yr, cb_group == cb) %>%
          as.data.table()
        
        if (it == 'model') {
          test = misty::ci.sd(data_subset$value, conf.level = 0.95, group = data_subset$model)
        } else if (it == 'impact_function') {
          test = misty::ci.sd(data_subset$value, conf.level = 0.95, group = data_subset$impact_function)
        } else if (it == 'parameter') {
          test = misty::ci.sd(data_subset$value, conf.level = 0.95, group = data_subset$parameter)
        } else {
          test = misty::ci.sd(data_subset$value, conf.level = 0.95, group = data_subset$cf)
        }
        test = test$result

        # save values
        res = data.table(year = yr,
                         cb_group = cb,
                         item = it,
                         sd_05 = median(test$low),
                         sd_50 = median(test$sd),
                         sd_95 = median(test$upp))
        sd_calc = rbindlist(list(sd_calc,res))
      }
    }
  }
  sd_calc = sd_calc %>%
    mutate(item = ifelse(item == 'impact_function', 'rr function', item))

  return(sd_calc)
}

doM_gsa_plot_SI = function(sd_calc) {
  pl = ggplot(sd_calc, aes(x = item, y = sd_50)) +
    geom_bar(stat = 'identity') +
    geom_errorbar(aes(ymin = sd_05, ymax = sd_95, width = 0.35),alpha=0.5) +
    facet_grid(cb_group ~ year) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    theme_pubr() + 
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 20), strip.background = element_blank(),
          axis.title = element_text(size = 20), axis.text = element_text(size = 15),
          legend.position = "none") +
    labs(y = 'SD', x = 'Uncertainty sources')
  
  name = 'Results/Mort/GSA/sd'
  print(name)
  ggsave(file=paste0(name,'.png'), width = 300, height = 200, units = 'mm', plot = pl)
  ggsave(file=paste0(name,'.pdf'), width = 300, height = 200, units = 'mm', plot = pl)
}

doM_gsa = function(datIni) {
  tmp = doM_gsa_preprocess(datIni)
  doM_gsa_plot_SI(tmp)
}

do_m_gsa_folder = function() {
  if(!dir.exists(file.path('Results/Mort/GSA'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Mort/'))){
      dir.create(file.path('Results/Mort/'))
    }
    dir.create(file.path('Results/Mort/GSA'))
  }
}

