# # Step 1: Checking Assumptions
# # Normality check
# dattlo = datt %>% filter(cb_group == '<1000') %>% as.data.table()
# hist(value[model == "IMAGE_3_0"])
# qqnorm(output[A == "a1"])
# shapiro.test(output[A == "a1"])
# 
# library(boot)
# library(sensitivity)
# n <- 1000
# X1 <- data.frame(matrix(runif(8 * n), nrow = n))
# X2 <- data.frame(matrix(runif(8 * n), nrow = n))
# X3 <- data.frame(matrix(runif(8 * n), nrow = n))
# x <- sobol(model = sobol.fun, X1 = X1, X2 = X2, X3 = X3, order = 2, nboot = 100)
# print(x)
# summary(x)
# library(ggplot2)
# ggplot(x)
# 
# 


###############################################
###############################################
###############################################

doEcon_gsa = function(datIni) {
  
  data = datIni %>%
    mutate(impact_function = unlist(str_extract_all(impact_function_global_level,"(?<=PM25MORT_).+(?=_ci)"))) %>%
    select(year,policy,carbon_budget,model,cb_group,region,
           impact_function,parameter,cf,method,scenario,alpha,
           value)
  N = length(data$value)
  
  # individual uncertainty
  dat_medi_general = data.table(data)[, .(v05 = quantile(value,0.05),
                                                        v50 = quantile(value, 0.5),
                                                        v95 = quantile(value,0.95))] 

  items = c('model','method','alpha','impact_function','parameter','cf')

  sd_calc = NULL
  for (yr in unique(data$year)) {
    for (cb in unique(data$cb_group)) {
      for (it in items) {
        data_subset = data %>%
          dplyr::group_by(get(it)) %>%
          dplyr::summarise(v05 = quantile(value,0.05),
                           v50 = quantile(value,0.50),
                           v95 = quantile(value,0.95)) %>%
          ungroup() %>%
          dplyr::rename_with(~"subitem", 1) %>%
          as.data.table()
        
        tmp05_sqd = 0
        tmp50_sqd = 0
        tmp95_sqd = 0
        for (subit in unique(data_subset$subitem)) {
          tmp05_sqd = tmp05_sqd + 
            (data_subset[subitem == subit]$v05 - dat_medi_general$v05)^2
          tmp50_sqd = tmp50_sqd + 
            (data_subset[subitem == subit]$v50 - dat_medi_general$v50)^2
          tmp95_sqd = tmp95_sqd + 
            (data_subset[subitem == subit]$v95 - dat_medi_general$v95)^2
        }
        sd_05 = sqrt(tmp05_sqd/N)
        sd_50 = sqrt(tmp50_sqd/N)
        sd_95 = sqrt(tmp95_sqd/N)
        
        # save values
        res = data.table(year = yr,
                         cb_group = cb,
                         item = it,
                         sd_05 = sd_05,
                         sd_50 = sd_50,
                         sd_95 = sd_95)
        sd_calc = rbindlist(list(sd_calc,res))
      }
    }
  }
  sd_calc = sd_calc %>%
    mutate(item = factor(item, levels=c('model','method','alpha','rr function','parameter','cf')),
           cb_group = factor(cb_group, levels = c("<1000", "[1000,2000]", ">2000")))
  
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
  
  name = 'Results/Econ/GSA/sd'
  print(name)
  ggsave(file=paste0(name,'.png'), width = 300, height = 200, units = 'mm', plot = pl)
  ggsave(file=paste0(name,'.pdf'), width = 300, height = 200, units = 'mm', plot = pl)
}

do_econ_gsa_folder = function() {
  if(!dir.exists(file.path('Results/Econ/GSA'))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Econ/'))){
      dir.create(file.path('Results/Econ/'))
    }
    dir.create(file.path('Results/Econ/GSA'))
  }
}

