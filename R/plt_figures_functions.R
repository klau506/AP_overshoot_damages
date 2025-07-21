#' keep_only_scen_paired
#' 
#' @param dat: dataset
#' @return dataset with only REF scenario and EoC-NZ that consider the same cb
keep_only_scen_paired = function(datt) {
  if ('REF' %in% unique(datt$scenario)) {
    datA = pivot_wider(datt, names_from = scenario, values_from = value)
    datA = data.table(datA)
    datB = na.omit(datA[, !"REF"])
    datC = pivot_longer(datB, cols = c('EoC','NZ'), names_to = 'scenario', values_to = 'value')
    datD = rbind(datC, datt[datt$scenario == 'REF',])
  } else {
    datA = pivot_wider(datt, names_from = scenario, values_from = value)
    datB = na.omit(datA)
    datD = pivot_longer(datB, cols = c('EoC','NZ'), names_to = 'scenario', values_to = 'value')
  }
  return(datD)
}

#' normalize_deaths 
#' 
#' @param data dataset containing 
#' n: regions' names
#' region: regions' numbers
#' year: numeric year: 2020, 2030, 2050
#' value: deaths by region in nº of people
#' @param factor by default 1e6 (million), but if specified, reports pop-normalized deaths by FACTOR people
#' @return deaths value population-normalized by million (1e6) people
normalize_deaths = function(data, factor = 1e6) {
  # merge with population data
  if ("n" %in% colnames(data) && "region" %in% colnames(data)) {
    data <- data %>% 
      left_join(df_pop, by = c('year', 'n', 'region'))
  } else if ("n" %in% colnames(data)) {
    data <- data %>% 
      left_join(df_pop, by = c('year', 'n'))
  } else if ("region" %in% colnames(data)) {
    data <- data %>% 
      left_join(df_pop, by = c('year', 'region'))
  } else {
    stop('insufient information to do a population-normalization')
  }

  # population-normalize deaths by FACTOR: (deaths / pop by region) * FACTOR
  data <- data %>% 
    dplyr::mutate(norm_value = factor * value / pop)
  
  # rename columns
  data <- data %>% 
    dplyr::select(-pop, -value) %>% 
    dplyr::rename(value = norm_value)
  
  return(data)
}


  
#' load_df_pop 
#' 
#' @return regional and total population dataframe for all R10 regions and WORLD
load_df_pop = function() {
  assign('df_pop',
         get(load(file.path(data_path,'RDA/',outD,'pop_by_reg.RData'))) %>% 
           dplyr::select(-annualPop))
  # update year name
  df_pop <- df_pop %>% 
    dplyr::mutate(year = ifelse(t == 1, 2010,
                         ifelse(t == 2, 2020,
                                ifelse(t == 3, 2030, 2050)))) %>% 
    dplyr::filter(year >= 2020) %>% 
    dplyr::select(-t)
  # update region column names
  df_pop <- df_pop %>% 
    dplyr::rename(n = region) %>% 
    dplyr::rename(region = nn) %>% 
    dplyr::select(-fullname)
  # compute WORLD pop
  df_pop_world <- df_pop %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(across(c(pop),sum),
                     .groups = 'drop') %>%
    dplyr::mutate(region = 11, n = 'WORLD') %>% 
    dplyr::ungroup() %>%
    as.data.frame()
  
  df_pop <- bind_rows(df_pop,
                      df_pop_world)
  
  return(df_pop)
}

#' load_df_av 
#' 
#' @return dong and dechezlepretre avoided damages (dong and dechezlepretre avoided damages) dataframe for all R10 regions and WORLD
load_df_av = function() {
  # load(file.path(data_path,'RDA/',outD,'df_pol_wrongMay2025.RData'))
  load(file.path(data_path,'RDA/',outD,'df_pol2.RData'))

  # add WORLD as a region in df_pol
  df_pol.world <- df_pol %>%
    dplyr::group_by(year,alpha,scenario,policy,carbon_budget,cb_group,pollutant,model) %>% 
    dplyr::summarise(across(c(dong_damage_avoided),sum),
                     across(c(dech_damage_avoided),sum),
                     .groups = 'drop') %>%
    dplyr::mutate(region = '11', n = 'WORLD') %>%
    as.data.frame()
  df_pol = bind_rows(df_pol, df_pol.world)
  
  rm(df_pol.world)
  
  # change from USD/2010 to USD/2020
  df_pol = df_pol %>% 
    dplyr::mutate(dong_damage_avoided = dong_damage_avoided * GDP_incr) %>% 
    dplyr::mutate(dech_damage_avoided = dech_damage_avoided * GDP_incr)
  
  return(df_pol)
}





#' load_df_vsl 
#' 
#' @return vsl damages (vsl absolute and avoided damage) dataframe for all R10 regions and WORLD
load_df_vsl = function() {
  load(file.path(data_path,'RDA/',outD,'df_vsl.RData'))
  df_vsl <- df_vsl %>% 
    dplyr::rename(vsl_damage_avoided = vsl_damage_absolute)
  # load(file.path(data_path,'FromZeus/RDA',outD,'hcl_gdp_damage_avoided.RData'))
  # df_hc = hcl_gdp_damage_avoided %>%
  
  load(file.path(data_path,'RDA',outD,'df_hcl.RData'))
  df_hc = df_hcl %>%
    dplyr::filter(pollutant == 'PM25') %>%
    dplyr::rename('hcl_damage_avoided' = 'hcl_gdp_damage_avoided')
    # dplyr::rename('hcl_damage_avoided' = 'hcl_gdp_damage_avoided')
  df_vsl = merge(df_vsl, df_hc, by = c('region','year','model','cb_group','carbon_budget','scenario','policy','impact_function_global_level','n',
                                       'impact_function','parameter','cf','group','pollutant','alpha')) 
  # %>%
  #   dplyr::select(-c(vsl_damage_absolute))
  
  # add WORLD as a region in df_vsl
  df_vsl.world <- df_vsl %>% 
    dplyr::group_by(year,alpha,scenario,policy,carbon_budget,cb_group,pollutant,model,impact_function_global_level) %>% 
    dplyr::summarise(across(c(vsl_damage_avoided),sum),
                     across(c(hcl_damage_avoided),sum),
                     .groups = 'drop') %>%
    dplyr::mutate(region = '11', n = 'WORLD') %>%
    as.data.frame() %>%
    tidyr::separate(impact_function_global_level, c('pollutant','impact_function','parameter','tmp1','tmp2'), remove = FALSE) %>%
    dplyr::mutate(cf = ifelse(tmp1 %in% c('zLO','zMED','zHI'), tmp1, 'zUNI')) %>%
    dplyr::mutate(group = ifelse(tmp1 %in% c('gWITH','gOUT'), tmp1,
                                 ifelse(tmp2 %in% c('gLO','gMED','gHI'), tmp2, 'gUNI'))) %>%
    dplyr::select(-c(tmp1,tmp2))
  df_vsl.world$pollutant = str_replace(df_vsl.world$pollutant, 'PM25MORT', 'PM25')
  df_vsl = bind_rows(df_vsl, df_vsl.world) %>%
    tidyr::unite(impact_function_group, c(impact_function,group), sep = '_')
  
  rm(df_vsl.world)
  
  # change from USD/2010 to USD/2020
  df_vsl = df_vsl %>% 
    dplyr::mutate(hcl_damage_avoided = hcl_damage_avoided * GDP_incr) %>% 
    dplyr::mutate(vsl_damage_avoided = vsl_damage_avoided * GDP_incr)
  
  return(df_vsl)
}


load_raw_df_mort = function() {
  namesr10 = fread(paste(data_path,'WITCH','r10namesrelation.csv',sep='/'))
  assign('df_mort', get(load(file.path('../../../Analysis/TFM_analysis/Data/mort_imp_fun_v24_new.RData'))))
  rm(datMort)
  df_mort = data.table(df_mort)
  setnames(df_mort, 't', 'year')
  df_mort$t = ifelse(df_mort$year == 2020, 1,
                     ifelse(df_mort$year == 2030, 2, 
                            ifelse(df_mort$year == 2050, 3, NA)))
  df_mort = merge(df_mort, namesr10[, c('region','nn')], by.x = 'Regions', by.y = 'region')
  df_mort = df_mort %>%
    dplyr::rename('scenario' = 'scen') %>%
    dplyr::rename('n' = 'Regions') %>%
    dplyr::rename('region' = 'nn') %>%
    dplyr::select(region,n,value,year,ci_level,z_level,scenario,policy,carbon_budget,cb_group,pollutant,model,impact_function_group) %>%
    as.data.frame()
  
  return(df_mort)
}

load_raw_df_w_mort = function() {
  namesr10 = fread(paste(data_path,'WITCH','r10namesrelation.csv',sep='/'))
  assign('df_mort', get(load(file.path('../../../Analysis/TFM_analysis/Data/mort_imp_fun_v24_new.RData'))))
  rm(datMort)
  df_mort = data.table(df_mort)
  setnames(df_mort, 't', 'year')
  df_mort$t = ifelse(df_mort$year == 2020, 1,
                     ifelse(df_mort$year == 2030, 2, 
                            ifelse(df_mort$year == 2050, 3, NA)))
  df_mort = merge(df_mort, namesr10[, c('region','nn')], by.x = 'Regions', by.y = 'region')
  df_mort = df_mort %>%
    dplyr::rename('scenario' = 'scen') %>%
    dplyr::rename('n' = 'Regions') %>%
    dplyr::rename('region' = 'nn') %>%
    dplyr::select(region,n,value,year,ci_level,z_level,scenario,policy,carbon_budget,cb_group,pollutant,model,impact_function_group) %>%
    as.data.frame()

  # add WORLD as a region in df_mort
  df_mort.world <- df_mort %>% 
    dplyr::group_by(year,ci_level,z_level,scenario,policy,carbon_budget,model,cb_group,pollutant,impact_function_group) %>% 
    dplyr::summarise(across(c(value),sum),
                     .groups = 'drop') %>%
    dplyr::mutate(region = '11', n = 'WORLD') %>%
    dplyr::mutate(region = as.numeric(region)) %>%
    dplyr::ungroup() %>%
    dplyr::select(region,n,value,year,ci_level,z_level,scenario,policy,carbon_budget,cb_group,pollutant,model,impact_function_group) %>%
    as.data.frame()
  return(df_mort.world)
}


#' load_df_mort 
#' 
#' @return premature mortality due to AP dataframe for all R10 regions and WORLD
load_df_mort = function() {
  namesr10 = fread(paste(data_path,'WITCH','r10namesrelation.csv',sep='/'))
  assign('df_mort', get(load(file.path('../../../Analysis/TFM_analysis/Data/mort_imp_fun_v24_new.RData'))))
  rm(datMort)
  df_mort = data.table(df_mort)
  setnames(df_mort, 't', 'year')
  df_mort$t = ifelse(df_mort$year == 2020, 1,
                     ifelse(df_mort$year == 2030, 2, 
                            ifelse(df_mort$year == 2050, 3, NA)))
  df_mort = merge(df_mort, namesr10[, c('region','nn')], by.x = 'Regions', by.y = 'region')
  df_mort = df_mort %>%
    dplyr::rename('scenario' = 'scen') %>%
    dplyr::rename('n' = 'Regions') %>%
    dplyr::rename('region' = 'nn')
  
  # percentiles
  df_mort = df_mort %>%
    tidyr::unite(ci_z_level, c('ci_level', 'z_level'), remove = TRUE) %>%
    dplyr::mutate(across('ci_z_level', \(x) str_replace(x,'ciHI_zLO|ciHI_zUNI', '95th'))) %>%
    dplyr::mutate(across('ci_z_level', \(x) str_replace(x,'ciMED_zMED|ciMED_zUNI', '50th'))) %>%
    dplyr::mutate(across('ci_z_level', \(x) str_replace(x,'ciLO_zHI|ciLO_zUNI', '5th'))) %>%
    dplyr::filter(ci_z_level %in% c('5th','50th','95th')) %>%
    dplyr::mutate(ci_z_level = factor(ci_z_level, levels = c('5th','50th','95th')))

  # add WORLD as a region in df_mort
  df_mort.world <- df_mort %>% 
    dplyr::group_by(year,ci_z_level,scenario,policy,carbon_budget,model,cb_group,pollutant,impact_function_group) %>% 
    dplyr::summarise(across(c(value),sum),
                     .groups = 'drop') %>%
    dplyr::mutate(region = '11', n = 'WORLD') %>%
    dplyr::mutate(region = as.numeric(region)) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  df_mort = bind_rows(df_mort, df_mort.world) %>%
    dplyr::select(region,n,value,year,ci_z_level,scenario,policy,carbon_budget,cb_group,pollutant,model,impact_function_group) %>%
    as.data.frame()
  
  rm(df_mort.world)
  return(df_mort)
}


#' load_df_conc 
#' 
#' @return premature mortality due to AP dataframe for all R10 regions and WORLD
load_df_conc = function() {
  namesr10 = fread(paste(data_path,'WITCH','r10namesrelation.csv',sep='/'))
  assign('df_conc', get(load(file.path('/media/klaudia/easystore/MAMME/TFM/Analysis/TFM_analysis/Data/conc_fasstr_v24_allyears.RData'))))
  rm(datConc)
  df_conc = data.table(df_conc)
  df_conc$t = ifelse(df_conc$year == 2020, 1,
                     ifelse(df_conc$year == 2030, 2, 
                            ifelse(df_conc$year == 2050, 3, NA)))
  df_conc = merge(df_conc, namesr10[, c('region','nn')], by = 'region') %>%
    dplyr::rename('n' = 'nn')

  # add WORLD as a region in df_conc
  df_conc.world <- df_conc %>% 
    dplyr::group_by(year,scenario,policy,carbon_budget,model,cb_group,pollutant) %>% 
    dplyr::summarise(across(c(value),mean),
                     .groups = 'drop') %>%
    dplyr::mutate(region = 'WORLD', n = 11) %>%
    dplyr::mutate(n = as.numeric(n)) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  df_conc = bind_rows(df_conc, df_conc.world) %>%
    dplyr::select(region,n,value,year,scenario,policy,carbon_budget,cb_group,pollutant,model) %>%
    as.data.frame()
  
  rm(df_conc.world)
  return(df_conc)
}

load_df_conc = function() {
  namesr10 = fread(paste(data_path,'WITCH','r10namesrelation.csv',sep='/'))
  namesr10 = dplyr::bind_rows(namesr10, data.frame('fullname' = 'World', 
                                                   'region' = 'WORLD', 
                                                   'nn' = 11))
  assign('df_conc', get(load(file.path('../../../Analysis/TFM_analysis/Data/conc_fasstr_v30_allyears.RData'))))
  rm(datConc)
  df_conc = data.table(df_conc)
  df_conc = merge(df_conc, namesr10[, c('region','nn')], by = 'region')
  df_conc = df_conc %>%
    dplyr::rename('n' = 'region') %>%
    dplyr::rename('region' = 'nn')

  return(df_conc)
}

#' map_plot 
#' 
#' @param df dataset
#' @param save if true, save; return plot otherwise
#' @param what topic of the map
#' @param y year
#' @param facet_title display facet title if true, erase otherwise
#' @return Figure: map
map_plot = function(df, save, what, y, facet_title){

    # obtain regions
  wreg <- read_csv(file.path('../fasstr_data','WITCH','mapwitch10.csv'),show_col_types = FALSE)
  # transforms ISO codes into country names
  wreg <- wreg %>%
    mutate(region = countrycode(ISO, origin = 'iso3c', destination = 'country.name'))
  # Correcting some mismatches
  wreg['ISO'=='ANT']$region = 'Netherlands Antilles'
  
  # world data
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI"))
  world <- world|>mutate(adm0_a3=ifelse(sovereignt=='South Sudan','SSD',adm0_a3))
  # merge the WITCH regional definition with the world map
  world <- merge(world,wreg, by.x = "adm0_a3", by.y = "ISO")
  
  if (!what %in% c('av_mort_diff','av_mort_diff_main','av_damage_diff_main')) {
    levels(df$alpha) <- c("high","medium",'low')
  } 
  
  #merge world data with our df
  world0 <- merge(df[df$year %in% y,], world, by = "n", allow.cartesian=TRUE)
  # Use a better projection 'equal projection'
  target_crs <- '+proj=eqearth +wktext'
  world1 <- st_as_sf(world0)
  world1 <- st_transform(world1, crs = target_crs)
  
  legend = ifelse(grepl('av_mort_diff', what, fixed = TRUE), 'Avoided deaths\n[100.000 people/year]',
                  'US Billion\n')
  
  if (grepl('main', what)) {
    world1$quantile <- factor(world1$quantile, levels = c('v05','vmed','v95'))
  }
  
  #plot
  pl <- ggplot(data = world1, xaxt = "n") +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs, expand = FALSE, clip = "off") +
    coord_sf(datum = target_crs, expand = FALSE) +
    labs(x = '',y = '') +
    theme_void() +
    theme(panel.background = element_rect(fill = 'white', linetype = 'blank'), plot.background = element_rect(fill = 'white', linetype = 'blank'),
          plot.title = element_text(hjust = 0.5, size = 12))

  if (what == 'av_damage_diff' & length(unique(world1$method)) == 4) {
    pl = pl +
      scale_fill_material("teal",guide = guide_colourbar(direction = "vertical", title = element_blank()),
                          name = "US Billion\n",trans='pseudo_log',breaks = c(0,floor(median(world1$value)),floor(max(world1$value))))+ 
      facet_wrap(. ~ method,
                ncol = 4,
                labeller = labeller(method = method_av.labs, alpha = alpha.labs)) +
      theme(legend.key.size = unit(1, 'cm'), legend.key.width = unit(6,'cm'), legend.position = 'bottom', legend.direction='horizontal',
            strip.text = element_text(size = 12), strip.text.y = element_text(angle = 90),
            legend.title = element_text(size=12), legend.text = element_text(size=12)) + 
      guides(fill = guide_colourbar(barheight = 0.8, barwidth = 9))
  } else if (what == 'indiv_av_damage_diff' & length(unique(world1$method)) < 3) {
      pl = pl +
        theme(legend.key.height = unit(1, 'cm'), legend.key.width = unit(0.5,'cm'),legend.position = 'left',
              ggh4x.facet.nestline = element_line(colour = "#545459"),
              legend.title = element_text(size=12), legend.text = element_text(size=12),
              strip.text = element_text(size = 12))
      if (!facet_title) {
        pl = pl +
          ggh4x::facet_nested(method ~ year + alpha,
                              labeller = labeller(method = method_av.labs, alpha = alpha.labs),
                              strip = strip_nested(
                                text_x = list(element_blank(), element_text(size = 0)),
                                by_layer_x = TRUE)) +
          scale_fill_material("teal",guide = guide_colourbar(direction = "vertical", title = element_blank()),
                              name = "              ",trans='pseudo_log') + 
          guides(fill = guide_colourbar(barwidth = 0.8, barheight = 9))
      } else {
        pl = pl +
          ggh4x::facet_nested(method ~ year + alpha, ncol = 5,
                              labeller = labeller(method = method_av.labs, alpha = alpha.labs),
                              nest_line = element_line(linetype = 'solid', linewidth = 0.5),
                              strip = strip_nested(
                                text_x = list(element_text(), element_text()))) +
          scale_fill_material("teal",guide = (guide_colourbar(direction = "vertical", title = element_blank())),
                              name = legend,trans='pseudo_log') + 
          guides(fill = guide_colourbar(barwidth = 0.8, barheight = 9))
      }
  } else if (what == c('av_mort_diff')) {
    maxVal = floor(max(world1$value))
    pl = pl +
      scale_fill_material("teal",guide = guide_colourbar(direction = "vertical", title = element_blank()),
                          name = legend,trans='pseudo_log',
                          breaks = c(0,floor(maxVal/2),maxVal)
                          # breaks = c(floor(min(world1$value)),floor(median(world1$value)),floor(max(world1$value)))
      )+
      facet_wrap(impact_function ~ ., ncol = 4,
                 labeller = labeller(impact_function = impact_function_group.labs, ci_z_level = ci_z_level.labs)) +
      theme(legend.key.size = unit(1.5, 'cm'), legend.key.height = unit(3,'cm'),
            strip.text.y = element_text(size = 12,angle = 90),strip.text.x = element_text(size = 12),
            legend.title = element_text(size=12), legend.text = element_text(size=12), legend.position = 'bottom') + 
      guides(fill = guide_colourbar(barheight = 0.8, barwidth = 9))
  } else if (grepl('main', what)) {
    maxVal = floor(max(world1$value))
    pl = pl +
      scale_fill_material("teal",guide = guide_colourbar(direction = "vertical", title = element_blank()),
                          name = legend,trans='pseudo_log',
                          breaks = c(0,floor(maxVal/2),maxVal)
                          # breaks = c(floor(min(world1$value)),floor(median(world1$value)),floor(max(world1$value)))
                          )+
      facet_wrap(quantile ~ ., ncol = 3,
                 labeller = labeller(quantile = quantile.labs)) +
      theme(legend.key.size = unit(1.5, 'cm'), legend.key.height = unit(3,'cm'),
            strip.text.y = element_text(size = 12,angle = 90),strip.text.x = element_text(size = 12),
            legend.title = element_text(size=12), legend.text = element_text(size=12), legend.position = 'bottom') + 
      guides(fill = guide_colourbar(barheight = 0.8, barwidth = 9))
  }

  if (save) {
    name = paste0('test/econ_img/',folder,'/map_',what,'_',paste(y, collapse = '-'),'.png')
    print(name)
    ggsave(file=file.path(name), width = 550, height = 500, units = 'mm', plot = pl)
  } else {
    return(pl)
  }
  
}





#' prob_distrib_mort
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @param legend TRUE by default, if false it is removed
#' @return Figure: probability distribution of premature mortality
prob_distrib_mort = function(df, y, remove_xfacet, reg, legend = TRUE) {

  # change scale to million premature deaths
  df = df %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  df = df |> dplyr::filter(n == reg & year == y)
  df_medi <- df[, .(medi = quantile(value, 0.5)),
                by=c('year','pollutant','cb_group','scenario',
                     'ci_z_level','impact_function_group')]
  
  df = data.table(df)

  pl <- ggplot(df) +
    geom_density(data = df, aes(x = value, group = interaction(scenario,ci_z_level),
                                color = scenario, fill = scenario, linetype = ci_z_level), 
                 linewidth = 0.8, alpha = 0.25) +
    geom_vline(aes(color = scenario, xintercept = medi, linetype = ci_z_level),
               data = df_medi, linewidth = 1) +
    facet_wrap(. ~ impact_function_group, nrow = 2, scales = 'free_y',
               labeller = labeller(impact_function_group = impact_function_group.labs)) +
    ggpubr::theme_pubr() +
    scale_fill_manual(values = scenario.colors,
                      name = 'Policy design',
                      labels = scenario.labs)+
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_linetype_manual(values = ci_z_level.linestyle,
                          name = 'Parameters & \nCF quantiles',
                          labels = ci_z_level.labs)+
    guides(linewidth = 'none', linetype = guide_legend(title = "CI for RR functions\n(parameters & zcf)", 
                                                       keywidth = 3, override.aes = list(linewidth = 2)),
           color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    labs(x = 'Premature deaths [million people/year]', y = 'Probability density') +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE, 
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.position = "bottom")
  
  if (remove_xfacet) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if (!legend) {
    pl = pl + theme(legend.position = 'none') + labs(x = '')
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))
  name = paste0('paper_figures/fig1/distrib_plot_mort_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 1000, height = 400, units = 'mm', plot = pl)
  
  return(pl)
}

#' prob_distrib_mort_main
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @param legend TRUE by default, if false it is removed
#' @return Figure: probability distribution of premature mortality
prob_distrib_mort_main = function(df, y, remove_xfacet, reg, legend = TRUE) {

  # change scale to million premature deaths
  df = df %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  df = df |> dplyr::filter(n == reg & year == y)
  df_medi <- df[, .(medi = quantile(value, 0.5)),
                by=c('year','cb_group','ci_z_level','scenario')]
  df = data.table(df)

  pl <- ggplot(df) +
    geom_density(data = df, aes(x = value, group = interaction(scenario,ci_z_level),
                                color = scenario, fill = scenario, linetype = ci_z_level), 
                 linewidth = 0.8, alpha = 0.25) +
    geom_vline(aes(color = scenario, xintercept = medi, linetype = ci_z_level),
               data = df_medi, linewidth = 1) +
    ggpubr::theme_pubr() +
    scale_fill_manual(values = scenario.colors,
                      name = 'Policy design',
                      labels = scenario.labs)+
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_linetype_manual(values = ci_z_level.linestyle,
                          name = 'Parameters & \nCF quantiles',
                          labels = ci_z_level.labs)+
    guides(linewidth = 'none', linetype = guide_legend(title = "CI for RR functions\n(parameters & zcf)", 
                                                       keywidth = 3, override.aes = list(linewidth = 2)),
           color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    labs(x = 'Premature deaths [million people/year]', y = 'Probabilty density') +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE, 
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.position = "bottom")
  
  if (remove_xfacet) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if (!legend) {
    pl = pl + theme(legend.position = 'none') + labs(x = '')
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))
  name = paste0('paper_figures/fig1/distrib_plot_mort_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 1000, height = 400, units = 'mm', plot = pl)
  
  return(pl)
}




#' cum_fun_mort
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @return Figure: cumulative function of premature mortality
cum_fun_mort = function(df, y, remove_xfacet, reg) {
  
  # change scale to million premature deaths
  df = df %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  df = df |> filter(n == reg & year == y) #& quantile_name %in% c('v05','vmed','v95'))
  dat_tmp <- df %>% 
    dplyr::group_by(scenario, cb_group, impact_function_group, ci_z_level) %>%
    dplyr::reframe(
      value = unique(value),
      ecdf = ecdf(value)(unique(value))
    ) 
  
  
  pl <- ggplot(dat_tmp, aes(value, ecdf, color = scenario, fill = scenario, linetype = ci_z_level)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(. ~ impact_function_group, nrow = 2,
               labeller = labeller(impact_function_group = impact_function_group.labs)) +
    ggpubr::theme_pubr() +
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_fill_manual(values = scenario.colors,
                      name = 'Policy design',
                      labels = scenario.labs)+
    scale_linetype_manual(values = ci_z_level.linestyle,
                          name = 'Parameters & \nCF quantiles',
                          labels = ci_z_level.labs)+
    guides(linewidth = 'none', linetype = guide_legend(title = "CI for RR functions\n(parameters & zcf)", 
                                                       keywidth = 3, override.aes = list(linewidth = 2)),
           color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    labs(x = 'Premature deaths [million people/year]', y = 'Cumulative probability') +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.position = "bottom")
  
  if (remove_xfacet) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))
  name = paste0('paper_figures/fig1/cumulative_plot_mort_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 1000, height = 400, units = 'mm', plot = pl, limitsize = FALSE)
  
  return(pl)
}


#' cum_fun_mort_main
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @return Figure: cumulative function of premature mortality
cum_fun_mort_main = function(df, y, remove_xfacet, reg) {
  
  # change scale to million premature deaths
  df = df %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  df = df |> filter(n == reg & year == y) #& quantile_name %in% c('v05','vmed','v95'))
  dat_tmp <- df %>% 
    dplyr::group_by(scenario,cb_group,ci_z_level) %>%
    dplyr::reframe(
      value = unique(value),
      ecdf = ecdf(value)(unique(value))
    ) 
  
  pl <- ggplot(dat_tmp, aes(value, ecdf, color = scenario, fill = scenario, linetype = ci_z_level)) +
    geom_line(linewidth = 0.8) +
    ggpubr::theme_pubr() +
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_fill_manual(values = scenario.colors,
                      name = 'Policy design',
                      labels = scenario.labs)+
    scale_linetype_manual(values = ci_z_level.linestyle,
                          name = 'Parameters & \nCF quantiles',
                          labels = ci_z_level.labs)+
    guides(linewidth = 'none', linetype = guide_legend(title = "CI for RR functions\n(parameters & zcf)", 
                                                       keywidth = 3, override.aes = list(linewidth = 2)),
           color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    labs(x = 'Premature deaths [million people/year]', y = 'Cumulative probaiblity') +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.position = "bottom")
  
  if (remove_xfacet) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))
  name = paste0('paper_figures/fig1/cumulative_plot_mort_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 1000, height = 400, units = 'mm', plot = pl, limitsize = FALSE)
  
  return(pl)
}




#' prob_distrib_econ
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @param legend TRUE by default; if FALSE, it should be erased
#' @return Figure: probability distribution of avoided damage
prob_distrib_econ = function(df, y, remove_xfacet, reg, legend = TRUE) {
  df = df[df$n == reg & df$year == y,]
  df = data.table(df)
  datall_medi <- df[, .(medi = quantile(value, 0.5)),
                    by=c('cb_group','scenario','alpha','method')]
  
  pl <- ggplot(df) +
    geom_density(data = df, aes(x = value, group = interaction(scenario, alpha),
                                color = scenario, fill = scenario, linetype = alpha), linewidth = 0.8, alpha = 0.25) +
    geom_vline(aes(color = scenario, xintercept = medi, linetype = alpha), 
               data = datall_medi, linewidth = 1) +
    facet_wrap(. ~ method, scales = 'free_y', nrow = 1,
                       labeller = labeller(method = method_av.labs)) +
    ggpubr::theme_pubr() +
    scale_fill_manual(values = scenario.colors,
                      name = 'Policy design',
                      labels = scenario.labs)+
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_linetype_manual(values = alpha.linetype,
                          name = 'CI for damage functions\n(elasticities and RR)',
                          labels = alpha.labs)+
    scale_x_continuous(labels = scales::comma) +
    labs(y = 'Probability density') +
    guides(linewidth = 'none', color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2)),
           linetype = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE, 
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.5), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom")
  if (remove_xfacet) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if (!legend) {
    pl = pl + theme(legend.position = 'none')
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm')) + labs(x = '')
  name = paste0('paper_figures/fig3/distrib_plot_econ_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 800, height = 300, units = 'mm', plot = pl)
  
  return(pl)
}

#' prob_distrib_econ_main
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @param legend TRUE by default; if FALSE, it should be erased
#' @return Figure: probability distribution of avoided damage
prob_distrib_econ_main = function(df, y, remove_xfacet, reg, legend = TRUE) {
  df = df[df$n == reg & df$year == y,]
  df = data.table(df)
  datall_medi <- df[, .(medi = quantile(value, 0.5)),
                    by=c('cb_group','scenario','alpha')]
  
  pl <- ggplot(df) +
    geom_density(data = df, aes(x = value, group = interaction(scenario, alpha),
                                color = scenario, fill = scenario, linetype = alpha), linewidth = 0.8, alpha = 0.25) +
    geom_vline(aes(color = scenario, xintercept = medi, linetype = alpha), 
               data = datall_medi, linewidth = 1) +
    ggpubr::theme_pubr() +
    scale_fill_manual(values = scenario.colors,
                      name = 'Policy design',
                      labels = scenario.labs)+
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_linetype_manual(values = alpha.linetype,
                          name = 'CI for damage functions\n(elasticities and RR)',
                          labels = alpha.labs)+
    labs(x = 'US Billion', y = 'Probability density') +
    scale_x_continuous(labels = scales::comma) +
    guides(linewidth = 'none', color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2)),
           linetype = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE, 
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.5), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom")
  if (remove_xfacet) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  if (!legend) {
    pl = pl + theme(legend.position = 'none')
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))
  name = paste0('paper_figures/fig3/distrib_plot_econ_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 800, height = 300, units = 'mm', plot = pl)
  
  return(pl)
}




#' cum_fun_econ
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @return Figure: cumulative function of avoided damage
cum_fun_econ = function(df, y, remove_xfacet, reg) {
  df = df |> filter(n == reg & year == y)
  dat_tmp <- df %>% 
    dplyr::group_by(scenario,cb_group,method,alpha) %>% 
    dplyr::reframe(
      value = unique(value),
      ecdf = ecdf(value)(unique(value))
      )

  # plot
  pl <- ggplot(dat_tmp, aes(value, ecdf, color = scenario, fill = scenario, linetype = alpha)) +
    geom_line(linewidth = 0.8, alpha=0.8) +
    facet_wrap(. ~ method, scales = 'free_y', nrow = 1,
               labeller = labeller(method = method_av.labs)) +
    ggpubr::theme_pubr() +
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_fill_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_linetype_manual(values = alpha.linetype,
                          name = 'CI for damage functions\n(elasticities and RR)',
                          labels = alpha.labs,)+
    labs(x = 'US Billion', y = 'Cumulative probability') +
    scale_x_continuous(labels = scales::comma) +
    guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2)),
           linetype = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.5), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom")
    
  if (remove_xfacet) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))

  name = paste0('paper_figures/fig3/cumulative_plot_econ_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 600, height = 200, units = 'mm', plot = pl)

  return(pl)
}


#' cum_fun_econ_main
#' 
#' @param df dataset
#' @param y year
#' @param remove_xfacet if true, remove facet text x-axis
#' @param reg region
#' @return Figure: cumulative function of avoided damage
cum_fun_econ_main = function(df, y, remove_xfacet, reg) {
  df = df |> filter(n == reg & year == y)
  dat_tmp <- df %>% 
    dplyr::group_by(scenario,cb_group,alpha) %>% 
    dplyr::reframe(
      value = unique(value),
      ecdf = ecdf(value)(unique(value))
      )

  # plot
  pl <- ggplot(dat_tmp, aes(value, ecdf, color = scenario, fill = scenario, linetype = alpha)) +
    geom_line(linewidth = 0.8, alpha=0.8) +
    ggpubr::theme_pubr() +
    scale_color_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_fill_manual(values = scenario.colors,
                       name = 'Policy design',
                       labels = scenario.labs)+
    scale_linetype_manual(values = alpha.linetype,
                          name = 'CI for damage functions\n(elasticities and RR)',
                          labels = alpha.labs,)+
    labs(x = 'US Billion', y = 'Cumulative probability') +
    scale_x_continuous(labels = scales::comma) +
    guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2)),
           linetype = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 12), strip.background = element_blank(),
          axis.title.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.5), axis.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom")
    
  if (remove_xfacet) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  
  pl = pl + theme(legend.key.size = unit(1, 'cm'))

  name = paste0('paper_figures/fig3/cumulative_plot_econ_',reg,'_',y,'.png')
  print(name)
  ggsave(file=file.path(name), width = 600, height = 200, units = 'mm', plot = pl)

  return(pl)
}


#' sensitivity_plot
#' 
#' @param datIni dataset
#' @param reg region
#' @param poll pollutant
#' @return Figure: sensitivity of premature deaths estimation factors
m_sensitivity_plot = function(datIni,reg,poll) {
  
  # change scale to million premature deaths
  datIni = datIni %>% 
    dplyr::mutate(value = round(value / 1e6, digits = 2))
  
  dat <- data.table(datIni)[year %in% c(2030,2050) & n == reg & pollutant == poll,
                .(cmed = quantile(value, 0.5),
                  c05  = quantile(value, 0.05),
                  c95  = quantile(value, 0.95),
                  mmax  = max(value),
                  mmin  = min(value),
                  mmean  = mean(value)),
                by = c('year','region','cb_group','impact_function_group',
                       'scenario', 'ci_level', 'z_level')] %>% 
    do_rename_imp_fun_etal()
  
  dat[ci_level == "ciLO", ci_label := "2.5th"]
  dat[ci_level == "ciMED", ci_label := "50th"]
  dat[ci_level == "ciHI", ci_label := "97.5th"]
  dat[, ci_level := factor(ci_label, levels = c("2.5th", "50th", "97.5th"))]

  dat[z_level == "zLO", z_label := '2.5th']
  dat[z_level == "zMED", z_label := '50th']
  dat[z_level == "zHI", z_label := '97.5th']
  dat[z_level == "zUNI", z_label := 'No ZCF\nuncertainty']
  dat[, z_level := factor(z_label, levels = c('2.5th','50th','97.5th','No ZCF\nuncertainty'))]

  dat$cb_group <- factor(dat$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  dat$z_level <- fct_rev(factor(dat$z_level, levels = c('2.5th','50th','97.5th','No ZCF\nuncertainty')))
  dat$imp_fun_label <- fct_rev(factor(dat$imp_fun_label, levels = c("Cohen et al. (2005) [51]", "Krewski et al. (2009) [55]", "Burnett et al. (2014) [50]",
                                                                    "GBD (low) (2015) [56]", "GBD (medium) (2015) [56]","GBD (high) (2015) [56]", 
                                                                    "Burnett et al.\n(with) (2018) [49]", "Burnett et al.\n(without) (2018) [49]")
  ))

  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = imp_fun_label,
                      group = interaction(scenario,ci_level,z_level),
                      color = ci_level,
                      linetype = z_level),
                  linewidth = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(x = cmed,
                   y = imp_fun_label,
                   group = interaction(scenario,ci_level,z_level),
                   shape = scenario),
               position = position_dodge(width = 0.5),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = imp_fun_label,
                   group = interaction(scenario,ci_level,z_level),
                   color = ci_level,
                   shape = scenario),
               position = position_dodge(width = 0.5),
               size = 1.5) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 12),
          axis.text.x.bottom = element_text(size = 10)) +
    scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    scale_shape_manual(values = c(16,17),
                       name = 'Policy design') +
    scale_color_brewer(palette = "Set1",
                       name = 'Parameter\npercentile') +
    scale_linetype_manual(values = shortpal_linetype,
                          labels = shortpal_linetype.labs,
                          name = 'ZCF\npercentile') +
    labs(title='', x = 'Premature deaths [million people/year]', y = 'ZCF percentile') +
    guides(shape = 'none')

  return(pl)
}


#' sensitivity_plot
#' 
#' @param datIni dataset
#' @param reg region
#' @param poll pollutant
#' @return Figure: sensitivity of premature deaths estimation factors
econ_sensitivity_plot = function(datIni,reg,poll) {
  
  dat <- data.table(datIni)[year %in% c(2030,2050) & n == reg & pollutant == poll,
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
  
  pl <- ggplot(dat) +
    geom_errorbar(aes(xmin = c05,
                      xmax = c95,
                      y = meth_label,
                      group = interaction(scenario,alpha,impact_function_group),
                      linetype = alpha,
                      color = impact_function_group),
                  linewidth = 1,
                  width = 0.33,
                  position = position_dodge(width = 0.9)) +
    geom_point(aes(x = cmed,
                   y = meth_label,
                   group = interaction(scenario,alpha,impact_function_group),
                   shape = scenario),
               position = position_dodge(width = 0.9),
               size = 2.5) +
    geom_point(aes(x = cmed,
                   y = meth_label,
                   group = interaction(scenario,alpha,impact_function_group),
                   color = impact_function_group,
                   shape = scenario),
               position = position_dodge(width = 0.9),
               size = 1.5) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y.left = element_text(size = 14),
          axis.text.x.bottom = element_text(size = 10),
          legend.title = element_text(size=12), legend.text = element_text(size=12)) +
    scale_x_continuous(labels = scales::comma) +
    scale_shape_manual(values = c(16,17),
                       name = 'Policy design')+
    scale_color_manual(values = set1,
                       name = 'RR function') +
    scale_linetype_manual(values = c('low' = 'dotted', 'medium' = 'solid', 'high' = 'dashed'),
                          name = 'Elasticity') +
    labs(title='', x = 'US Billion', y = "") +
    guides(shape = 'none')

  return(pl)
}


