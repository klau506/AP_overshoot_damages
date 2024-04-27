###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################
doM_FIGURE_num_deaths = function(dd) {
  dd = dd |> filter(t %in% c(2030,2050))

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_all_now = dd %>%
    dplyr::group_by(cb_group,Regions,t,scen,pollutant,ci_z_level,model) %>%
    dplyr::summarise(x = median(value)) %>%
    dplyr::ungroup() %>%
    # consider the overall deaths: O3 + PM25 deaths
    dplyr::group_by(cb_group,Regions,t,scen,ci_z_level,model) %>%
    dplyr::summarise(val_sum = sum(x)) %>%
    dplyr::ungroup() %>%
    # consider the overall deaths across all models
    dplyr::group_by(cb_group,Regions,t,scen,ci_z_level) %>%
    dplyr::summarise(final_val = median(val_sum)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(., .keep_all = FALSE)
  
  dd_all_w = dd_all_now %>% 
    # compute the World deaths by cb_group, t, scen, and ci_z_level
    dplyr::group_by(cb_group,t,scen,ci_z_level) %>% 
    dplyr::summarise(final_val = sum(final_val)) %>% 
    dplyr::mutate(Regions = 'WORLD') %>% 
    dplyr::ungroup()
  
  dd_all = bind_rows(dd_all_w, dd_all_now) %>% 
    # compute the 95% CI
    dplyr::group_by(cb_group,Regions,t,scen) %>%
    dplyr::reframe(enframe(quantile(final_val, c(0.05, 0.5, 0.95)), "quantile", "final_val")) %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    # rename CI
    dplyr::mutate(across('quantile', \(x) str_replace(x, '50%', 'vmed'))) %>%
    dplyr::mutate(across('quantile', \(x) str_replace(x, '95%', 'v95'))) %>%
    dplyr::mutate(across('quantile', \(x) str_replace(x, '5%', 'v05'))) %>%
    dplyr::mutate(final_val = round(final_val, digits = 0))
  
  dd_all$Regions = fct_rev(dd_all$Regions)
  dd_all = dd_all %>%
    dplyr::mutate(Regions = sapply(dd_all$Regions, do_rename_regions_string))
  
  dd_all = pivot_wider(dd_all, names_from = quantile, values_from = final_val)
  
  # figure by year
  for(year in c(2030,2050)) {
    pl_nz = do_num_deaths_plot(dd_all,'NZ',year,T)
    pl_eoc = do_num_deaths_plot(dd_all,'EoC',year,F)
    pl_ref = do_num_deaths_plot(dd_all,'REF',year,F)
    
    maxHeight = grid::unit.pmax(pl_nz$heights, pl_eoc$heights, pl_ref$heights)
    pl_nz$heights <- as.list(maxHeight)
    pl_eoc$heights <- as.list(maxHeight)
    pl_ref$heights <- as.list(pl_eoc$heights)
    
    fig = ggarrange(pl_nz + rremove('xlab') + rremove('ylab'),
                    pl_eoc + rremove('xlab') + rremove('ylab'),
                    pl_ref + rremove('xlab') + rremove('ylab'),
                    common.legend = TRUE, legend="right",
                    ncol = 3, nrow = 1,
                    widths = c(1,0.75,0.35),
                    heights = c(1,1,0.5))
    fig = annotate_figure(fig,
                          top = text_grob(paste0("Premature deaths in ",year), color = "black", face = "bold", size = 14),
                          left = text_grob("Regions", color = "black", size = 10, rot = 90),
                          bottom = text_grob("Carbon budget [GtCO2]", color = "black", size = 10))
    
    pl_name = ifelse(length(unique(dd$Regions)) == 1, 'num_deaths_world', 'num_deaths_table')
    name_file_numD = paste0("Results/Mort/NumDeaths/",pl_name,'_',year)
    print(name_file_numD)
    ggsave(file=paste(name_file_numD,'.png'), width = 350, height = 150, units = "mm",plot = fig, limitsize = FALSE)
    ggsave(file=paste(name_file_numD,'.pdf'),  width = 350, height = 150, units = "mm",plot = fig, limitsize = FALSE)
  }
  
  # whole figure
  pl_nz_2030 = do_num_deaths_plot(dd_all,'NZ',2030,T)
  pl_eoc_2030 = do_num_deaths_plot(dd_all,'EoC',2030,F)
  pl_ref_2030 = do_num_deaths_plot(dd_all,'REF',2030,F)
  
  pl_nz_2050 = do_num_deaths_plot(dd_all,'NZ',2050,T)
  pl_eoc_2050 = do_num_deaths_plot(dd_all,'EoC',2050,F)
  pl_ref_2050 = do_num_deaths_plot(dd_all,'REF',2050,F)
  
  fig = ggarrange(pl_nz_2030 + rremove('xlab') + rremove('ylab'),
                  pl_eoc_2030 + rremove('xlab') + rremove('ylab'),
                  pl_ref_2030 + rremove('xlab') + rremove('ylab'),
                  
                  pl_nz_2050 + rremove('xlab') + rremove('ylab'),
                  pl_eoc_2050 + rremove('xlab') + rremove('ylab'),
                  pl_ref_2050 + rremove('xlab') + rremove('ylab'),
                  
                  common.legend = TRUE, legend="right",
                  labels = c('2030','','','2050'),
                  ncol = 3, nrow = 2,
                  widths = c(1,0.75,0.35))
  
  fig = annotate_figure(fig,
                        top = text_grob(paste0("Premature deaths"), color = "black", face = "bold", size = 14),
                        left = text_grob("Regions", color = "black", size = 10, rot = 90),
                        bottom = text_grob("Carbon budget [GtCO2]", color = "black", size = 10))
  
  pl_name = ifelse(length(unique(dd$Regions)) == 1, 'num_deaths_world', 'num_deaths_table')
  name_file_numD = paste("Results/Mort/NumDeaths/",pl_name)
  print(name_file_numD)
  ggsave(file=paste0(name_file_numD,'.png'), width = 375, height = 300, units = "mm",plot = fig, limitsize = FALSE)
  ggsave(file=paste0(name_file_numD,'.pdf'), width = 375, height = 300, units = "mm",plot = fig, limitsize = FALSE)
}



do_num_deaths_plot = function(dat,sc,yr,ylab) {
  tt = paste(sc, 'scenario')
  
  pl = ggplot(data = dat |> filter(t == yr, scen == sc), aes(x = cb_group, y = Regions)) +
    geom_tile(aes(fill = vmed)) +
    geom_text(aes(label = paste0(round(vmed, 1),'\n','(',round(v05, 1),', ',round(v95, 1),')')), size = 3.2) +
    scale_fill_gradient("Deaths", low = "#e6d329", high = "#d6290b") +
    theme_bw() +
    labs(x = "\n Carbon budget", y = "Region",
         title = tt) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.title = element_text(size = 10)) +
    theme_pubr(legend = 'right') +
    labs_pubr(base_size = 10)
  
  if(!ylab) {
    pl = pl + scale_y_discrete(labels = NULL, breaks = NULL)
  }
  
  return(pl)
}


do_numdeaths_folder = function() {
  if(!dir.exists(file.path('Results/Mort/NumDeaths'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Mort/'))){
      dir.create(file.path('Results/Mort/'))
    }
    dir.create(file.path('Results/Mort/NumDeaths'))
  }
}
