
doEcon_ks_testt = function(datIni) {

  # ks test to check if the EoC and NZ scenarios are so different or not
  res_test = NULL
  # LOOP YEAR
  for (y in unique(datIni$year)) {
    #LOOP REGION
    for (reg in unique(datIni$region)) {
      # LOOP CARBON BUDGET
      for(cbg in unique(datIni$cb_group)) {
        # LOOP ECON-METHOD
        for(met in unique(datIni$method)) {

          dat = datIni |> filter(year == y & region == reg & cb_group == cbg
                                 & method == met)

          res = setDT(broom::glance(ks.test(subset(dat,scenario == "EoC")$value,
                                     subset(dat,scenario == "NZ")$value,
                                     alternative = 'two.sided')))
          res[,dataNZ := length(subset(dat,scenario == "NZ")$value)]
          res[,dataEoC := length(subset(dat,scenario == "EoC")$value)]
          res[,method := met]
          res[,cb_group := cbg]
          res[,region := reg]
          res[,year := y]
          res_test = rbindlist(list(res_test,res))
        }
      }
    }
  }

  return(res_test)
}


doEcon_ks_test_process = function(dat, slides = FALSE) {
  ks_Econ_test = doEcon_ks_testt(dat) %>%
    as.data.table()

  # order the p.value per regions-cbgroup-year of the p.value
  ks_Econ_test = ks_Econ_test[order(ks_Econ_test$p.value),]
  ks_Econ_test$cb_group <- factor(ks_Econ_test$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  ks_Econ_test = ks_Econ_test %>%
    dplyr::mutate(region = sapply(ks_Econ_test$region, do_rename_regions_string))
  ks_Econ_test = data.table(ks_Econ_test)
  pl = doEcon_ks_comp_plot(ks_Econ_test, slides)
  
  return(pl)
}


doEcon_ks_comp_plot = function(ks_all, slides = FALSE) {
  pl_all = ggplot(data = ks_all, aes(x = cb_group, y = p.value, group = method,
                                     color = region)) +
    geom_point( aes(shape = method), position=position_dodge(width = .5), alpha = 0.9, size = 3) +
    geom_hline(yintercept = 0.05, color = 'red', linetype = 'dashed', alpha = 0.9) +
    facet_grid(. ~ year) +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom") +
    scale_color_manual(values = regions.colors,
                       name = 'Regions') +
    scale_shape_manual(values = c(15,16,17,18),
                       name = 'Econ. methods',
                       labels = method_av.labs) +
    labs(title='Climate policy design dependency', x = 'Carbon budget', y = "p-value") +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))
  
  if (slides) {
    pl_all = pl_all + theme(legend.text = element_text(size = 12),
                    legend.title = element_text(size = 14),
                    strip.text = element_text(size = 14)) +
      labs(title = '')
  }

  
  name = dplyr::if_else(!slides, 'Results/Econ/ks_test/ks_test_econ',
                        'Results/Slides/ks_test_econ')
  print(name)
  w = dplyr::if_else(!slides, 400, 300)
  ggsave(file=paste0(name,'.png'), width = w, height = 180, units = "mm",plot = pl_all, limitsize = FALSE)
  ggsave(file=paste0(name,'.pdf'), width = w, height = 180, units = "mm",plot = pl_all, limitsize = FALSE)
  return(pl_all)
}

doEcon_KsTest_folder = function() {
  if(!dir.exists(file.path('Results/Econ/ks_test'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Econ/'))){
      dir.create(file.path('Results/Econ/'))
    }
    dir.create(file.path('Results/Econ/ks_test/'))
  }
}