load(file.path(data_path,'RDA','econ',"mask3618.RData"))
require(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################
doM_av_deaths_map_preprocess = function(datIni) {
  datIni = datIni |> filter(t %in% c(2030,2050))

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_all_now = datIni %>%
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
    dplyr::reframe(enframe(c(min(final_val),median(final_val),max(final_val)), "quantile", "final_val")) %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    # rename CI
    dplyr::mutate(across('quantile', \(x) str_replace(x, '2', 'vmed'))) %>%
    dplyr::mutate(across('quantile', \(x) str_replace(x, '3', 'vmax'))) %>%
    dplyr::mutate(across('quantile', \(x) str_replace(x, '1', 'vmin'))) %>%
    dplyr::mutate(final_val = round(final_val, digits = 0))
  
  # split CI values in different columns
  dd_all = pivot_wider(dd_all, values_from = final_val, names_from = c(quantile,scen))

  # copy the ref value for percentile for each Regions-year pair
  dd_all = data.table(dd_all)
  dd_all_full = data.frame()
  for (reg in unique(dd_all$Regions)) {
    for (yr in unique(dd_all$t)) {
      dd_all_tmp <- dd_all[Regions == reg & t == yr,
                           .(vmin_REF_full = unique(na.omit(dd_all[Regions == reg & t == yr & cb_group == '>2000']$vmin_REF)),
                             vmed_REF_full = unique(na.omit(dd_all[Regions == reg & t == yr & cb_group == '>2000']$vmed_REF)),
                             vmax_REF_full = unique(na.omit(dd_all[Regions == reg & t == yr & cb_group == '>2000']$vmax_REF))),
                           by = c('t','Regions')]
      
      dd_all_full = rbind(dd_all_full,dd_all_tmp)
    }
  }
  dd_all_full2 = merge(dd_all, dd_all_full, by = c('t','Regions'))
  
  return(dd_all_full2)
}

#' doM_av_deaths_map_nz_minus_eoc_preprocess
#' 
#' @param datIni: data
#' @param map: TRUE if the preprocessed data aims to be used to plot a map, FALSE otherwise
#' @return processed data with the avoided deaths using NZ instead of EoC climate policy
doM_av_deaths_map_nz_minus_eoc_preprocess = function(datIni, map) {
  datIni = datIni |> filter(t %in% c(2030,2050))

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_all = datIni %>%
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
    dplyr::distinct(., .keep_all = FALSE) %>%
    as.data.frame() %>%
    # rename CI
    dplyr::mutate(across('ci_z_level', \(x) str_replace(x, '2', 'vmed'))) %>%
    dplyr::mutate(across('ci_z_level', \(x) str_replace(x, '3', 'vmax'))) %>%
    dplyr::mutate(across('ci_z_level', \(x) str_replace(x, '1', 'vmin'))) 
  
  # split CI values in different columns
  dd_all = pivot_wider(dd_all, values_from = final_val, names_from = ci_z_level)
  
  # compute the difference between EoC and NZ
  dd_all = dd_all %>%
    dplyr::filter(scen %in% c('EoC','NZ')) %>%
    dplyr::select(cb_group,Regions,t,scen,vmed,vmin,vmax) %>%
    pivot_wider(names_from = scen, values_from = c(vmed,vmin,vmax))
  
  dd_all = dd_all %>%
    dplyr::group_by(cb_group,Regions,t) %>%
    dplyr::mutate(vmed = vmed_EoC - vmed_NZ) %>%
    dplyr::mutate(vmin = vmin_EoC - vmin_NZ) %>%
    dplyr::mutate(vmax = vmax_EoC - vmax_NZ) %>%
    as.data.table()
  
  dd_sel = dd_all %>%
    dplyr::select(cb_group, Regions, t, vmed, vmin, vmax) %>%
    dplyr::distinct(., .keep_all = FALSE)

  if(map) {
    dd = dd_sel %>%
      pivot_longer(c("vmin","vmed","vmax"), names_to = "per_comp", values_to = "val_comp")
    dat = data.table(dd)
    dat[per_comp == "vmin", per_label := 'min']
    dat[per_comp == "vmed", per_label := 'median']
    dat[per_comp == "vmax", per_label := 'max']
    dat[, per_comp := factor(per_label, levels = c('min','median','max'))]
    dat = as_tibble(dat)
  } else {
    dat = dd_sel
  }
  return(dat)
  
}

#' doM_av_deaths_map_select_climate_policy
#' 
#' @param dat: data
#' @param map: TRUE if the processed data is aimed to be used to plot a map
doM_av_deaths_map_select_climate_policy = function(dat,map) {
  
  dd_sel = dat %>%
    dplyr::mutate('vmed_ec' = vmed_REF_full - vmed_EoC,
                  'vmin_ec' = vmin_REF_full - vmin_EoC,
                  'vmax_ec' = vmax_REF_full - vmax_EoC,
                  'vmed_nz' = vmed_REF_full - vmed_NZ,
                  'vmin_nz' = vmin_REF_full - vmin_NZ,
                  'vmax_nz' = vmax_REF_full - vmax_NZ) %>%
    dplyr::select(t, Regions, cb_group, 
                  vmed_ec, vmin_ec, vmax_ec,
                  vmed_nz, vmin_nz, vmax_nz) %>%
    # replace NA values with non-NA values within each group
    group_by(t, Regions, cb_group) %>%
    fill(everything(), .direction = "downup") %>%
    filter(row_number() == 1) %>%
    as.data.frame()
  
  df_long <- dd_sel %>%
    pivot_longer(cols = starts_with("v"), 
                 names_to = c(".value", "scen"), 
                 names_pattern = "(vmed|vmin|vmax)_(\\w{2})") %>%
    mutate(scen = ifelse(scen == "nz", "NZ", 'EoC')) %>%
    as.data.frame()
  
  if(map) {
    dd = df_long %>%
      pivot_longer(c("vmin","vmed","vmax"), names_to = "per_comp", values_to = "val_comp")
    dat = data.table(dd)
    dat[per_comp == "vmin", per_label := 'min']
    dat[per_comp == "vmed", per_label := 'median']
    dat[per_comp == "vmax", per_label := 'max']
    dat[, per_comp := factor(per_label, levels = c('min','median','max'))]
    dat = as_tibble(dat)
  } else {
    dat = df_long
  }
  return(dat)
}


#' doM_dat_world_whole_plot
#' 
#' @param dat: data to be plotted. Already preprocessed
#' @param cp: climate policy: NZ, EoC, or diff
#' @param yr: 2030 or 2050
#' @param save: if TRUE, save generated figure. Return it otherwise
doM_dat_world_whole_plot = function(dat,cp,yr,save) {
  # obtain regions
  wreg <- read_csv(file.path(data_path,'RDA/','econ','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg |>
    mutate(Regions = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$Regions = 'Netherlands Antilles'
  
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
  world <- world|>mutate(adm0_a3=ifelse(sovereignt=='South Sudan','SSD',adm0_a3))
  
  # merge the regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  # merge with the emission data.frame
  r10_nmap <- merge(dat, nmap, by.x="Regions", by.y="n")
  world0 <- merge(world, r10_nmap, by.x = 'n', by.y='Regions', allow.cartesian=TRUE) %>%
    filter(t %in% yr)
  if (! 'diff' %in% cp) {
    world0 = world0 %>% filter(scen %in% cp)
  }
  
  # Remove small islands
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_transform(world0, crs = target_crs)
  
  tt = ifelse(length(unique(world1$t)) == 1, 
              ifelse(length(unique(world1$scen)) == 1 & cp != 'diff', paste('Avoided deaths,',yr,cp), paste('Avoided deaths,',yr)),
              ifelse(length(cp) == 1 && cp == 'diff','Avoided deaths (NZ - EoC)', 'Avoided deaths'))
  mm = min(world1 |> filter(t %in% yr) |> pull(val_comp))
  MM = max(world1 |> filter(t %in% yr) |> pull(val_comp))
  pl <- ggplot(data = world1 |> filter(t %in% yr)) +
    geom_sf(aes(fill = val_comp)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    labs(title = tt) +
    theme_void() +
    rotate_y_facet_text(angle = 270, align = 0.5, valign = 0.5)
  
  if (length(cp) == 1 && cp == 'diff') {
    pl = pl +
      facet_grid(cb_group ~ t+per_comp) +
      scale_fill_gradientn("Avoided deaths \n",
                           colours=c("white","white","#f7e665","#9ff57f","#68de3c","#43b319","#2b8a07"), 
                           values=scales::rescale(c(mm,0-.Machine$double.eps,0,0+.Machine$double.eps,MM/4,MM/2,MM))) +
      theme(legend.position = 'bottom',
            legend.key.width = unit(1, 'cm'),
            legend.key.height = unit(0.35, 'cm'),
            legend.direction = "horizontal",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
            strip.text.x = element_text(vjust = 1, size = 10),
            strip.text.y = element_text(size = 6),
            plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'),
            panel.spacing = unit(0.5, "lines"))
  } else {
    pl = pl +
      facet_grid(t+cb_group ~ scen+per_comp) +
      scale_fill_gradient("Avoided deaths \n", low = "#f7e665", high = "#2b8a07") +
      theme(legend.position = 'bottom',
            legend.key.width = unit(2, 'cm'),
            legend.key.height = unit(0.7, 'cm'),
            legend.direction = "horizontal",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16),
            strip.text.x = element_text(vjust = 1, size = 16),
            strip.text.y = element_text(size = 16),
            plot.title = element_text(hjust = 0.5, size = 22, face = 'bold'),
            panel.spacing = unit(0.5, "lines"))
  }
  
  if(save) {
    pl = pl + theme(plot.title = element_text(hjust = 0.5, vjust = 1.5,size = 16))
    name_file = paste('Results/Mort/AvDeaths/av_deaths',cp,yr,sep='_')
    print(name_file)
    ggsave(file=paste0(name_file,'.png'), width = 350, height = 200, units = "mm",plot = pl, limitsize = FALSE)
    ggsave(file=paste0(name_file,'.pdf'), width = 350, height = 200, units = "mm",plot = pl, limitsize = FALSE)
  } else {
    return(pl)
  }
}

doM_FIGURE_av_deaths_map_by_climate_policy_year = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)
  #plot and save single maps
  for (sc in c('NZ','EoC')) {
    for (yr in unique(dat0$t)) {
      dat2 = doM_av_deaths_map_select_climate_policy(dat1,T)
      doM_dat_world_whole_plot(dat2,sc,yr,T)
    }
  }
}

doM_FIGURE_av_deaths_map_by_year = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)
  # figure by year
  for (yr in unique(dat0$t)) {
    print(yr)
    
    dat2 = doM_av_deaths_map_select_climate_policy(dat1,T)
    pl = doM_dat_world_whole_plot(dat2,c('NZ','EoC'),yr,F)
    
    name_file_numD = paste0('Results/Mort/AvDeaths/av_deaths_',yr,'_map')
    print(name_file_numD)
    ggsave(file=paste0(name_file_numD,'.png'), width = 500, height = 150, units = "mm",plot = pl, limitsize = FALSE)
    ggsave(file=paste0(name_file_numD,'.pdf'), width = 500, height = 150, units = "mm",plot = pl, limitsize = FALSE)
  }
}

doM_FIGURE_av_deaths_map = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)
  
  # unique figure
  dat2 = doM_av_deaths_map_select_climate_policy(dat1,T)
  dat2 = data.table(dat2)
  
  pl = doM_dat_world_whole_plot(dat2,c('NZ','EoC'),c(2030,2050),F)

  name_file_whole = "Results/Mort/AvDeaths/av_deaths_whole_map"
  print(name_file_whole)
  ggsave(file=paste0(name_file_whole,'.png'), width = 500, height = 300, units = "mm",plot = pl, limitsize = FALSE)
  ggsave(file=paste0(name_file_whole,'.pdf'), width = 500, height = 300, units = "mm",plot = pl, limitsize = FALSE)
}

doM_FIGURE_av_deaths_nz_minus_eoc_map = function(dat0) {
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_nz_minus_eoc_preprocess(dat0, map = TRUE)
  
  # unique figure
  pl = doM_dat_world_whole_plot(dat1,'diff',c(2030,2050),F)

  name_file_whole = "Results/Mort/AvDeaths/av_deaths_whole_map_diff"
  print(name_file_whole)
  ggsave(file=paste0(name_file_whole,'.png'), width = 150, height = 75, units = "mm",plot = pl, limitsize = FALSE)
  ggsave(file=paste0(name_file_whole,'.pdf'), width = 150, height = 75, units = "mm",plot = pl, limitsize = FALSE)
}

doM_FIGURE_av_deaths_table = function(dat0, slides = FALSE) {
  uniqueYear = dplyr::if_else(length(unique(dat0$t)) == 1, TRUE, FALSE)
  
  dat0 = dat0[t %in% c(2030,2050)]
  dat1 = doM_av_deaths_map_preprocess(dat0)
  
  # whole figure
  dat2 = doM_av_deaths_map_select_climate_policy(dat1,map=F) %>%
    dplyr::mutate_if(is.numeric, round)
  dat2 = data.table(dat2)
  dat2 = dat2[order(dat2$Regions), ]
  dat2$Regions = forcats::fct_rev(dat2$Regions)
  dat2 = dat2 %>%
    dplyr::mutate(Regions = sapply(dat2$Regions, do_rename_regions_string))
  dat2 = data.table(dat2)
  pl_nz_2030 = do_av_deaths_table(dat2,'NZ',2030,T)
  pl_nz_2050 = do_av_deaths_table(dat2,'NZ',2050,T)
  
  dat2 = doM_av_deaths_map_select_climate_policy(dat1,map=F) %>%
    dplyr::mutate_if(is.numeric, round)
  dat2 = data.table(dat2)
  dat2 = dat2[order(dat2$Regions), ]
  dat2$Regions = forcats::fct_rev(dat2$Regions)
  dat2 = dat2 %>%
    dplyr::mutate(Regions = sapply(dat2$Regions, do_rename_regions_string))
  dat2 = data.table(dat2)
  pl_eoc_2030 = do_av_deaths_table(dat2,'EoC',2030,F)
  pl_eoc_2050 = do_av_deaths_table(dat2,'EoC',2050,F)
  
  if (!uniqueYear) {
    fig = ggarrange(pl_nz_2030 + rremove('xlab') + rremove('ylab'),
                    pl_eoc_2030 + rremove('xlab') + rremove('ylab'),
                    
                    pl_nz_2050 + rremove('xlab') + rremove('ylab'),
                    pl_eoc_2050 + rremove('xlab') + rremove('ylab'),
                    
                    common.legend = TRUE, legend="right",
                    labels = c('2030','','2050'),
                    hjust = c(-0.2,0,-0.2),
                    ncol = 2, nrow = 2,
                    widths = c(1,0.83))
  } else if (unique(dat0$t) == 2030) {
    fig = ggarrange(pl_nz_2030 + rremove('xlab') + rremove('ylab'),
                    pl_eoc_2030 + rremove('xlab') + rremove('ylab'),

                    common.legend = TRUE, legend="right",
                    labels = c('2030'),
                    hjust = c(-0.2),
                    ncol = 2, nrow = 1,
                    widths = c(1,0.83))
  } else {
    fig = ggarrange(pl_nz_2050 + rremove('xlab') + rremove('ylab'),
                    pl_eoc_2050 + rremove('xlab') + rremove('ylab'),
                    
                    common.legend = TRUE, legend="right",
                    labels = c('2050'),
                    hjust = c(-0.2),
                    ncol = 2, nrow = 1,
                    widths = c(1,0.83))
  }
  

  fig = annotate_figure(fig,
                        bottom = text_grob("Carbon budget [GtCO2]", color = "black", size = 10))
  # if (!slides) {
  #   fig = annotate_figure(fig,
  #                         top = text_grob("Avoided deaths"), color = "black", face = "bold", size = 14,
  #                         left = text_grob("Regions", color = "black", size = 10, rot = 90))
  # }
  
  name_file = ifelse(!slides, dplyr::if_else(unique(dat2$Regions) == 'WORLD',
                                             "Results/Mort/AvDeaths/mM_av_deaths_table_world",
                                             "Results/Mort/AvDeaths/mM_av_deaths_table"),
                             ifelse(uniqueYear, dplyr::if_else(unique(dat2$Regions) == 'WORLD',
                                                               paste0("Results/Slides/mM_Mort_av_deaths_table_world_",unique(dat0$t)),
                                                               paste0("Results/Slides/mM_Mort_av_deaths_table_",unique(dat0$t))),
                                    dplyr::if_else(unique(dat2$Regions) == 'WORLD',
                                                   "Results/Slides/mM_Mort_av_deaths_table_world",
                                                   "Results/Slides/mM_Mort_av_deaths_table")))
  print(name_file)
  h = dplyr::if_else(unique(dat2$Regions) == 'WORLD', 100,
                     dplyr::if_else(!slides, 300, 
                                    dplyr::if_else(uniqueYear, 125, 100)))
  ggsave(file=paste0(name_file,'.png'), width = 350, height = h, units = "mm",plot = fig, limitsize = FALSE)
  ggsave(file=paste0(name_file,'.pdf'), width = 350, height = h, units = "mm",plot = fig, limitsize = FALSE)
}

do_av_deaths_table = function(dat,cp,yr,ylab) {
  dat = dat |> filter(scen == cp & t == yr) %>%
    as.data.table()
  
  tt = paste(cp, 'climate policy')
  
  pl = ggplot(data = dat[t == yr], aes(x = cb_group, y = Regions)) +
    geom_tile(data = dat[t == yr & !is.na(dat$vmed)], aes(fill = vmed)) +
    geom_text(aes(label = paste0(round(vmed, 1),'\n','(',round(vmin, 1),', ',round(vmax, 1),')')), size = 3.2) +
    scale_fill_gradient("Deaths", low = "#f7e665", high = "#2b8a07") +
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

doM_AvDeaths_folder = function() {
  if(!dir.exists(file.path('Results/Mort/AvDeaths'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Mort/'))){
      dir.create(file.path('Results/Mort/'))
    }
    dir.create(file.path('Results/Mort/AvDeaths'))
  }
}
