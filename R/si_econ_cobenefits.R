load(file.path(data_path,'RDA/','econ',"mask3618.RData"))
require(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
###############################################################################
#                               NUMBER OF DEATHS                              #
###############################################################################
doEcon_co_benefits_map_preprocess = function(dd) {

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_all = dd %>%
    dplyr::group_by(cb_group,region,year,scenario,alpha,model,method) %>%
    dplyr::summarise(x = median(value)) %>%
    dplyr::ungroup() %>%
    # consider the overall deaths across all models
    dplyr::group_by(cb_group,region,year,scenario,alpha,method) %>%
    dplyr::summarise(val = median(x)) %>%
    dplyr::ungroup() %>%
    # consider the overall deaths across all methods
    dplyr::group_by(cb_group,region,year,scenario,alpha) %>%
    dplyr::summarise(final_val = median(val)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(., .keep_all = FALSE) %>%
    # compute the 95% CI
    dplyr::group_by(cb_group,region,year,scenario) %>%
    dplyr::reframe(enframe(quantile(final_val, c(0.05, 0.5, 0.95)), "quantile", "final_val")) %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    # rename CI
    dplyr::mutate(across('quantile', \(x) str_replace(x, '50%', 'vmed'))) %>%
    dplyr::mutate(across('quantile', \(x) str_replace(x, '95%', 'v95'))) %>%
    dplyr::mutate(across('quantile', \(x) str_replace(x, '5%', 'v05'))) %>%
    dplyr::mutate(final_val = round(final_val, digits = 2))
  
  # split CI values in different columns
  dd_all = pivot_wider(dd_all, values_from = final_val, names_from = c(quantile,scenario))

  # copy the ref value for percentile for each region-year pair
  dd_all = data.table(dd_all)
  dd_all_full = data.frame()
  for (reg in unique(dd_all$region)) {
    for (yr in unique(dd_all$year)) {
      dd_all_tmp <- dd_all[region == reg & year == yr,
                           .(v05_REF_full = unique(na.omit(dd_all[region == reg & year == yr & cb_group == '>2000']$v05_REF)),
                             vmed_REF_full = unique(na.omit(dd_all[region == reg & year == yr & cb_group == '>2000']$vmed_REF)),
                             v95_REF_full = unique(na.omit(dd_all[region == reg & year == yr & cb_group == '>2000']$v95_REF))),
                           by = c('year','region')]
      
      dd_all_full = rbind(dd_all_full,dd_all_tmp)
    }
  }
  dd_all_full2 = merge(dd_all, dd_all_full, by = c('year','region'))
  
  return(dd_all_full2)
}

#' doEcon_co_benefits_map_nz_minus_eoc_preprocess
#' 
#' @param dd: data
#' @param map: TRUE if the preprocessed data aims to be used to plot a map, FALSE otherwise
#' @return processed data with the avoided deaths using NZ instead of EoC climate policy
doEcon_co_benefits_map_nz_minus_eoc_preprocess = function(dd, map) {

  # consider the average deaths by cb, reg, year, climate policy, poll --> the impact function does not play a role any more
  dd_all = dd %>%
    dplyr::group_by(cb_group,region,year,scenario,alpha,model,method) %>%
    dplyr::summarise(x = mean(value)) %>%
    dplyr::ungroup() %>%
    # consider the overall deaths across all models
    dplyr::group_by(cb_group,region,year,scenario,alpha,method) %>%
    dplyr::summarise(val = mean(x)) %>%
    dplyr::ungroup() %>%
    # consider the overall deaths across all methods
    dplyr::group_by(cb_group,region,year,scenario,alpha) %>%
    dplyr::summarise(final_val = mean(val)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(., .keep_all = FALSE) %>%
    as.data.frame() %>%
    # rename CI
    dplyr::mutate(across('alpha', \(x) str_replace(x, 'med', 'vmed'))) %>%
    dplyr::mutate(across('alpha', \(x) str_replace(x, 'hi', 'v95'))) %>%
    dplyr::mutate(across('alpha', \(x) str_replace(x, 'lo', 'v05'))) 
  
  # split CI values in different columns
  dd_all = pivot_wider(dd_all, values_from = final_val, names_from = c(alpha,scenario))
  
  # compute the difference between EoC and NZ
  dd_all = dd_all %>%
    dplyr::group_by(cb_group,region,year) %>%
    dplyr::summarise(vmed = vmed_NZ - vmed_EoC,
                     v05 = v05_NZ - v05_EoC,
                     v95 = v95_NZ - v95_EoC) %>%
    as.data.table()

  if(map) {
    dd = dd_all %>%
      pivot_longer(c("v05","vmed","v95"), names_to = "per_comp", values_to = "val_comp")
    dat = data.table(dd)
    dat$per_label[dat$per_comp == "v05"] ='5th'
    dat$per_label[dat$per_comp == "vmed"] ='median'
    dat$per_label[dat$per_comp == "v95"] ='95th'
    dat[, per_comp := factor(per_label, levels = c('5th','median','95th'))]
    dat = as_tibble(dat)
  } else {
    dat = dd_sel
  }
  return(dat)
  
}

#' doEcon_co_benefits_map_select_climate_policy
#' 
#' @param dat: data
#' @param map: TRUE if the processed data is aimed to be used to plot a map
doEcon_co_benefits_map_select_climate_policy = function(dat,map) {
  
  dd_sel = dat %>%
    dplyr::mutate('vmed_ec' = vmed_EoC - vmed_REF_full,
                  'v05_ec' = v05_EoC - v05_REF_full,
                  'v95_ec' = v95_EoC - v95_REF_full,
                  'vmed_nz' = vmed_NZ - vmed_REF_full,
                  'v05_nz' = v05_NZ - v05_REF_full,
                  'v95_nz' = v95_NZ - v95_REF_full) %>%
    dplyr::select(year, region, cb_group, 
                  vmed_ec, v05_ec, v95_ec,
                  vmed_nz, v05_nz, v95_nz) %>%
    # replace NA values with non-NA values within each group
    group_by(year, region, cb_group) %>%
    fill(everything(), .direction = "downup") %>%
    filter(row_number() == 1) %>%
    as.data.frame()
  
  df_long <- dd_sel %>%
    pivot_longer(cols = starts_with("v"), 
                 names_to = c(".value", "scenario"), 
                 names_pattern = "(vmed|v05|v95)_(\\w{2})") %>%
    mutate(scenario = ifelse(scenario == "nz", "NZ", 'EoC')) %>%
    as.data.frame()


  if(map) {
    dd = df_long %>%
      pivot_longer(c("v05","vmed","v95"), names_to = "per_comp", values_to = "val_comp")
    dat = data.table(dd)
    dat[per_comp == "v05", per_label := '5th']
    dat[per_comp == "vmed", per_label := 'median']
    dat[per_comp == "v95", per_label := '95th']
    dat[, per_comp := factor(per_label, levels = c('5th','median','95th'))]
    dat = as_tibble(dat)
  } else {
    dat = df_long
  }
  return(dat)
}


#' doEcon_dat_world_whole_plot
#' 
#' @param dat: data to be plotted. Already preprocessed
#' @param cp: climate policy: NZ, EoC, or diff
#' @param yr: 2030 or 2050
#' @param save: if TRUE, save generated figure. Return it otherwise
doEcon_dat_world_whole_plot = function(dat,cp,yr,save) {
  # obtain regions
  wreg <- read_csv(file.path(data_path,'RDA/','econ','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg |>
    mutate(region = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$region = 'Netherlands Antilles'
  
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
  world <- world|>mutate(adm0_a3=ifelse(sovereignt=='South Sudan','SSD',adm0_a3))
  
  # merge the regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  # merge with the emission data.frame
  if ('R10AFRICA' %in% dat$region) {
    r10_nmap <- merge(dat, nmap, by.x="region", by.y="n") %>%
      dplyr::rename('n' = 'region')
  } else {
    r10_nmap <- merge(dat, nmap, by.x="region", by.y="nn")
  }
  world0 <- merge(world, r10_nmap, by.x = 'n', by.y='n', allow.cartesian=TRUE) %>%
    filter(year %in% yr)
  if (! 'diff' %in% cp) {
    world0 = world0 %>% filter(scenario %in% cp)
  }
  
  # Remove small islands
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_transform(world0, crs = target_crs)
  
  tt = ifelse(length(unique(world1$year)) == 1, 
              ifelse(length(unique(world1$scenario)) == 1 & cp != 'diff', paste('Economic co-benefits,',yr,cp), paste('Economic co-benefits,',yr)),
              ifelse(length(cp) == 1 && cp == 'diff','Economic co-benefits (NZ - EoC)', 'Economic co-benefits'))
  mm = min(world1 |> filter(year %in% yr) |> pull(val_comp))
  MM = max(world1 |> filter(year %in% yr) |> pull(val_comp))
  pl <- ggplot(data = world1 |> filter(year %in% yr)) +
    geom_sf(aes(fill = val_comp)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    rotate_y_facet_text(angle = 270, align = 0.5, valign = 0.5) +
    labs(title = tt) 
    
  if (length(cp) == 1 && cp == 'diff') {
    pl = pl +
      facet_grid(cb_group ~ year+per_comp) +
      scale_fill_gradientn("Economic co-benefits   \n[US Billion]",
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
      facet_grid(year+cb_group ~ scenario+per_comp) +
      scale_fill_gradient("Economic co-benefits   \n[US Billion]", low = "#f7e665", high = "#2b8a07") +
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
    name_file = paste('Results/Econ/CoBenefits/co_benefits',cp,'_',yr)
    ggsave(file=paste0(name_file,'.png'), width = 350, height = 200, units = "mm",plot = pl, limitsize = FALSE)
    ggsave(file=paste0(name_file,'.pdf'), width = 350, height = 200, units = "mm",plot = pl, limitsize = FALSE)
  } else {
    return(pl)
  }
}

doEcon_FIGURE_co_benefits_map_by_climate_policy_year = function(dat0) {
  dat0 = dat0 %>% filter(year %in% c(2030,2050))
  dat1 = doEcon_co_benefits_map_preprocess(dat0)
  
  #plot and save single maps
  for (sc in c('NZ','EoC')) {
    for (yr in unique(dat0$year)) {
      dat2 = doEcon_co_benefits_map_select_climate_policy(dat1,T)
      dat2$cb_group <- factor(dat2$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
      doEcon_dat_world_whole_plot(dat2,sc,yr,T)
    }
  }
}

doEcon_FIGURE_co_benefits_map_by_year = function(dat0) {
  dat0 = dat0 %>% filter(year %in% c(2030,2050))
  dat1 = doEcon_co_benefits_map_preprocess(dat0)
  
  # figure by year
  for (yr in unique(dat0$year)) {
    print(yr)
    
    dat2 = doEcon_co_benefits_map_select_climate_policy(dat1,T)
    dat2$cb_group <- factor(dat2$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
    pl = doEcon_dat_world_whole_plot(dat2,c('NZ','EoC'),yr,F)
    
    name_file_numD = paste('Results/Econ/CoBenefits/co_benefits_',yr,'_map')
    ggsave(file=paste0(name_file_numD,'.png'), width = 500, height = 150, units = "mm",plot = pl, limitsize = FALSE)
    ggsave(file=paste0(name_file_numD,'.pdf'), width = 500, height = 150, units = "mm",plot = pl, limitsize = FALSE)
  }
}

doEcon_FIGURE_co_benefits_map = function(dat0, slides = FALSE) {
  dat0 = dat0 %>% filter(year %in% c(2030,2050))
  dat1 = doEcon_co_benefits_map_preprocess(dat0)
  
  # unique figure
  dat2 = doEcon_co_benefits_map_select_climate_policy(dat1,T)
  dat2$cb_group <- factor(dat2$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  pl = doEcon_dat_world_whole_plot(dat2, c('NZ','EoC'), c(2030,2050), save = FALSE)

  if (!slides) {
    name_file_whole = "Results/Econ/CoBenefits/co_benefits_whole_map"
    ggsave(file=paste0(name_file_whole,'.png'), width = 500, height = 300, units = "mm",plot = pl, limitsize = FALSE)
    ggsave(file=paste0(name_file_whole,'.pdf'), width = 500, height = 300, units = "mm",plot = pl, limitsize = FALSE)
  } else {
    return(pl)
  }
}

doEcon_FIGURE_co_benefits_nz_minus_eoc_map = function(dat0) {
  dat0 = dat0 %>% filter(year %in% c(2030,2050))
  dat0 = dat0 %>%
    dplyr::filter(scenario != 'REF') 

  dat1 = doEcon_co_benefits_map_nz_minus_eoc_preprocess(dat0, map = TRUE)
  dat1$cb_group <- factor(dat1$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  # unique figure
  pl = doEcon_dat_world_whole_plot(dat1,'diff',c(2030,2050),F)

  name_file_whole = "Results/Econ/CoBenefits/co_benefits_whole_map_diff"
  ggsave(file=paste0(name_file_whole,'.png'), width = 150, height = 75, units = "mm",plot = pl, limitsize = FALSE)
  ggsave(file=paste0(name_file_whole,'.pdf'), width = 150, height = 75, units = "mm",plot = pl, limitsize = FALSE)
}

doEcon_FIGURE_co_benefits_table = function(dat0, slides = FALSE) {
  uniqueYear = dplyr::if_else(length(unique(dat0$year)) == 1, TRUE, FALSE)
  dat0 = dat0 %>% dplyr::filter(year %in% c(2030,2050))
  dat1 = doEcon_co_benefits_map_preprocess(dat0)

  # whole figure
  dat2 = doEcon_co_benefits_map_select_climate_policy(dat1,map=F)
  dat2 = data.table(dat2)
  dat2 = dat2[order(dat2$region), ]
  dat2$region = forcats::fct_rev(dat2$region)
  dat2 = dat2 %>%
    dplyr::mutate(region = sapply(dat2$region, do_rename_regions_string))
  dat2$cb_group <- factor(dat2$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  dat2 = data.table(dat2)
  pl_nz_2030 = doEcon_co_benefits_table(dat2,'NZ',2030,T)
  pl_nz_2050 = doEcon_co_benefits_table(dat2,'NZ',2050,T)
  
  dat2 = doEcon_co_benefits_map_select_climate_policy(dat1,map=F)
  dat2 = data.table(dat2)
  dat2 = dat2[order(dat2$region), ]
  dat2$region = forcats::fct_rev(dat2$region)
  dat2 = dat2 %>%
    dplyr::mutate(region = sapply(dat2$region, do_rename_regions_string))
  dat2$cb_group <- factor(dat2$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  dat2 = data.table(dat2)
  pl_eoc_2030 = doEcon_co_benefits_table(dat2,'EoC',2030,F)
  pl_eoc_2050 = doEcon_co_benefits_table(dat2,'EoC',2050,F)
  
  if (!uniqueYear) {
    fig = ggarrange(pl_nz_2030 + rremove('xlab') + rremove('ylab'),
                    pl_eoc_2030 + rremove('xlab') + rremove('ylab'),
                    
                    pl_nz_2050 + rremove('xlab') + rremove('ylab'),
                    pl_eoc_2050 + rremove('xlab') + rremove('ylab'),
                    
                    common.legend = TRUE, legend="right",
                    labels = c('2030','','2050'),
                    hjust = c(-0.2,0,-0.2),# hjust = c(0,0,0),vjust = c(0.2,0,0.2),
                    ncol = 2, nrow = 2,
                    widths = c(1,0.83))
  } else if (unique(dat0$year) == 2030) {
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
  if (!slides) {
    fig = annotate_figure(fig,
                          top = text_grob(paste0("Economic co-benefits"), color = "black", face = "bold", size = 14),
                          left = text_grob("region", color = "black", size = 10, rot = 90))
  }

  name_file = ifelse(!slides, dplyr::if_else(unique(dat2$region) == 'WORLD',
                                             "Results/Econ/CoBenefits/co_benefits_table_world",
                                             "Results/Econ/CoBenefits/co_benefits_table"),
                     ifelse(uniqueYear, dplyr::if_else(unique(dat2$region) == 'WORLD',
                                                       paste0("Results/Slides/Econ_CoBenefits_co_benefits_table_world_",unique(dat0$t)),
                                                       paste0("Results/Slides/Econ_CoBenefits_co_benefits_table_",unique(dat0$t))),
                            dplyr::if_else(unique(dat2$region) == 'WORLD',
                                           "Results/Slides/Econ_CoBenefits_co_benefits_table_world",
                                           "Results/Slides/Econ_CoBenefits_co_benefits_table")))
  print(name_file)
  h = dplyr::if_else(unique(dat2$region) == 'WORLD', 100,
                     dplyr::if_else(!slides, 300, 
                                    dplyr::if_else(uniqueYear, 125, 100)))[1]
  ggsave(file=paste0(name_file,'.png'), width = 350, height = h, units = "mm",plot = fig, limitsize = FALSE)
  ggsave(file=paste0(name_file,'.pdf'), width = 350, height = h, units = "mm",plot = fig, limitsize = FALSE)
}

doEcon_co_benefits_table = function(dat,cp,yr,ylab) {
  dat = dat |> filter(scenario == cp & year == yr) %>%
    as.data.table()
  tt = paste('       ', cp, 'climate policy')
  
  pl = ggplot(data = dat[year == yr], aes(x = cb_group, y = region)) +
    geom_tile(data = dat[year == yr & !is.na(dat$vmed)], aes(fill = vmed)) +
    geom_text(aes(label = paste(round(vmed, 2),'\n','(',round(v05, 2),',',round(v95, 2),')')), size = 3.2) +
    scale_fill_gradient("Economic co-benefits  \n[US Billion]", low = "#f7e665", high = "#2b8a07") +
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


doEcon_CoBenefits_folder = function() {
  if(!dir.exists(file.path('Results/Econ/CoBenefits'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Econ/'))){
      dir.create(file.path('Results/Econ/'))
    }
    dir.create(file.path('Results/Econ/CoBenefits'))
  }
}