#' do_mort_uncert.individual
#' 
#' @param dat_to_plot dataset
#' @param reg region
#' @return Figure: individual uncertainty along premature mortality computation factors
do_mort_uncert.individual = function(dat_to_plot,reg) {
  dat_to_plot = dat_to_plot %>%
    dplyr::select(value,model,impact_function_group,ci_level,z_level,year)
  # invdividual uncertainty
  dat_medi_general = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                     v05 = quantile(value,0.05),
                                     v25 = quantile(value,0.25),
                                     v75 = quantile(value,0.75),
                                     v95 = quantile(value,0.95)),
                                 by = c('year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_general = pivot_longer(dat_medi_general, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_general$ci = 'general'
  dat_medi_iam = dat_to_plot[,  .(v50 = quantile(value, 0.5),
                                  v05 = quantile(value,0.05),
                                  v25 = quantile(value,0.25),
                                  v75 = quantile(value,0.75),
                                  v95 = quantile(value,0.95)),
                             , by=c('model','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_iam = pivot_longer(dat_medi_iam, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_iam$ci = 'iam'
  dat_medi_impfun = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                    v05 = quantile(value,0.05),
                                    v25 = quantile(value,0.25),
                                    v75 = quantile(value,0.75),
                                    v95 = quantile(value,0.95)),
                                , by=c('impact_function_group','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_impfun = pivot_longer(dat_medi_impfun, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_impfun$ci = 'impfun'
  
  dat_medi_param = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                   v05 = quantile(value,0.05),
                                   v25 = quantile(value,0.25),
                                   v75 = quantile(value,0.75),
                                   v95 = quantile(value,0.95)),
                               , by=c('ci_level','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_param = pivot_longer(dat_medi_param, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_param$ci = 'param'
  
  dat_medi_cf = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                v05 = quantile(value,0.05),
                                v25 = quantile(value,0.25),
                                v75 = quantile(value,0.75),
                                v95 = quantile(value,0.95)),
                            , by=c('z_level','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_cf = pivot_longer(dat_medi_cf, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_cf$ci = 'cf'
  
  dat_indiv = bind_rows(dat_medi_general, dat_medi_iam, dat_medi_impfun, dat_medi_param, dat_medi_cf)
  dat_indiv = pivot_wider(dat_indiv, names_from = quantiles, values_from = value)
  pl_indiv = dat_indiv %>%
    mutate(ci = factor(ci, levels=c("general", "iam", "impfun", "param", "cf"))) %>%
    ggplot(aes(x = ci)) +
    geom_boxplot(aes(ymin = v05, lower = v25, middle = v50, upper = v75, ymax = v95), stat = 'identity', width = 0.8) +
    geom_hline(yintercept = 0, color = 'grey', linetype = 'dotted') +
    facet_grid(. ~ year) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    scale_y_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    theme_pubr() +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 20), strip.background = element_blank(),
          axis.title = element_text(size = 20), axis.text = element_text(size = 15),
          legend.position = "none") +
    labs(y = 'Premature deaths [million people/year]', x = 'Uncertainty sources')
  
  pl_indiv
  name = paste0('paper_figures/SI/uncert/indiv_uncert_mort_',reg,'_',unique(dat_indiv$year),'.pdf')
  ggsave(file=file.path(name), width = 200, height = 150, units = 'mm', plot = pl_indiv)
  
  return(pl_indiv)
}


#' do_mort_uncert.propagation
#' 
#' @param dat_to_plot dataset
#' @param reg region
#' @return Figure: uncertainty propagation along premature mortality computation factors
do_mort_uncert.propagation = function(dat_to_plot,reg) {
  dat_to_plot = dat_to_plot %>%
    dplyr::select(value,model,impact_function_group,ci_level,z_level,year)
  
  # propagated uncertainty
  dat_medi_general = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                     v05 = quantile(value,0.05),
                                     v25 = quantile(value,0.25),
                                     v75 = quantile(value,0.75),
                                     v95 = quantile(value,0.95)),
                                 by = c('year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_general = pivot_longer(dat_medi_general, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_general$ci = 'general'
  dat_medi_iam = dat_to_plot[,  .(v50 = quantile(value, 0.5),
                                  v05 = quantile(value,0.05),
                                  v25 = quantile(value,0.25),
                                  v75 = quantile(value,0.75),
                                  v95 = quantile(value,0.95)),
                             , by=c('model','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_iam = pivot_longer(dat_medi_iam, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_iam$ci = '+iam'
  dat_medi_impfun = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                    v05 = quantile(value,0.05),
                                    v25 = quantile(value,0.25),
                                    v75 = quantile(value,0.75),
                                    v95 = quantile(value,0.95)),
                                , by=c('model','impact_function_group','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_impfun = pivot_longer(dat_medi_impfun, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_impfun$ci = '+impfun'
  
  dat_medi_param = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                   v05 = quantile(value,0.05),
                                   v25 = quantile(value,0.25),
                                   v75 = quantile(value,0.75),
                                   v95 = quantile(value,0.95)),
                               , by=c('model','impact_function_group','ci_level','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_param = pivot_longer(dat_medi_param, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_param$ci = '+param'
  
  dat_medi_cf = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                v05 = quantile(value,0.05),
                                v25 = quantile(value,0.25),
                                v75 = quantile(value,0.75),
                                v95 = quantile(value,0.95)),
                            , by=c('model','impact_function_group','ci_level','z_level','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_cf = pivot_longer(dat_medi_cf, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_cf$ci = '+cf'
  
  dat_prop = bind_rows(dat_medi_general, dat_medi_iam, dat_medi_impfun, dat_medi_param, dat_medi_cf)
  dat_prop = pivot_wider(dat_prop, names_from = quantiles, values_from = value)
  pl_prop = dat_prop %>%
    mutate(ci = factor(ci, levels=c("general", "+iam", "+impfun", "+param", "+cf"))) %>%
    ggplot(aes(x = ci)) +
    geom_boxplot(aes(ymin = v05, lower = v25, middle = v50, upper = v75, ymax = v95), stat = 'identity', width = 0.9) +
    geom_hline(yintercept = 0, color = 'grey', linetype = 'dotted') +
    facet_grid(. ~ year) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    scale_y_continuous(labels = function(x) ifelse(x %% 1 == 0, format(x, digits = 1), format(x, digits = 2))) +    theme_pubr() +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 20), strip.background = element_blank(),
          axis.title = element_text(size = 20), axis.text = element_text(size = 15),
          legend.position = "none") +
    labs(y = 'Premature deaths [million people/year]', x = 'Added uncertainty sources')
  
  pl_prop
  name = paste0('paper_figures/SI/uncert/prop_uncert_mort_',reg,'_',unique(dat_prop$year),'.pdf')
  ggsave(file=file.path(name), width = 200, height = 150, units = 'mm', plot = pl_prop)
  
  return(pl_prop)
}




#' do_econ_uncert.individual
#' 
#' @param dat_to_plot dataset
#' @param reg region
#' @return Figure: uncertainty propagation along avoided damages computation factors
do_econ_uncert.individual = function(dat_to_plot,reg) {
  dat_to_plot = data.table(dat_to_plot)
  # individual uncertainty
  dat_medi_general = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                     v05 = quantile(value,0.05),
                                     v25 = quantile(value,0.25),
                                     v75 = quantile(value,0.75),
                                     v95 = quantile(value,0.95)),
                                 by = c('year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_general = pivot_longer(dat_medi_general, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_general$ci = 'general'
  
  dat_medi_iam = dat_to_plot[,  .(v50 = quantile(value, 0.5),
                                  v05 = quantile(value,0.05),
                                  v25 = quantile(value,0.25),
                                  v75 = quantile(value,0.75),
                                  v95 = quantile(value,0.95)),
                             , by=c('model','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_iam = pivot_longer(dat_medi_iam, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_iam$ci = 'iam'
  
  dat_medi_method = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                    v05 = quantile(value,0.05),
                                    v25 = quantile(value,0.25),
                                    v75 = quantile(value,0.75),
                                    v95 = quantile(value,0.95)),
                                , by=c('method','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_method = pivot_longer(dat_medi_method, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_method$ci = 'method'
  
  dat_medi_alpha = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                   v05 = quantile(value,0.05),
                                   v25 = quantile(value,0.25),
                                   v75 = quantile(value,0.75),
                                   v95 = quantile(value,0.95)),
                               , by=c('alpha','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_alpha = pivot_longer(dat_medi_alpha, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_alpha$ci = 'alpha'
  
  dat_medi_impfun = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                    v05 = quantile(value,0.05),
                                    v25 = quantile(value,0.25),
                                    v75 = quantile(value,0.75),
                                    v95 = quantile(value,0.95)),
                                , by=c('impact_function_group','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_impfun = pivot_longer(dat_medi_impfun, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_impfun$ci = 'impfun'
  
  dat_medi_param = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                   v05 = quantile(value,0.05),
                                   v25 = quantile(value,0.25),
                                   v75 = quantile(value,0.75),
                                   v95 = quantile(value,0.95)),
                               , by=c('parameter','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_param = pivot_longer(dat_medi_param, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_param$ci = 'parameters'
  
  dat_medi_cf = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                v05 = quantile(value,0.05),
                                v25 = quantile(value,0.25),
                                v75 = quantile(value,0.75),
                                v95 = quantile(value,0.95)),
                            , by=c('cf','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_cf = pivot_longer(dat_medi_cf, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_cf$ci = 'cf'
  
  dat_indiv = bind_rows(dat_medi_general, dat_medi_iam, dat_medi_method, dat_medi_alpha, dat_medi_impfun, dat_medi_param, dat_medi_cf)
  dat_indiv = pivot_wider(dat_indiv, names_from = quantiles, values_from = value)
  pl_indiv = dat_indiv %>%
    mutate(ci = ifelse(ci == 'alpha', 'elast', ci)) %>%
    mutate(ci = factor(ci, levels=c("general", "iam", "method", "elast", "impfun", "parameters", "cf"))) %>%
    ggplot(aes(x = ci)) +
    geom_boxplot(aes(ymin = v05, lower = v25, middle = v50, upper = v75, ymax = v95), stat = 'identity', width = 0.8) + 
    geom_hline(yintercept = 0, color = 'grey', linetype = 'dotted') +
    facet_grid(. ~ year) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    scale_y_continuous(labels = scales::comma) +
    theme_pubr() + 
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 20), strip.background = element_blank(),
          axis.title = element_text(size = 20), axis.text = element_text(size = 15),
          legend.position = "none") +
    labs(y = 'US Billion', x = 'Uncertainty sources')
  
  name = paste0('paper_figures/SI/uncert/indiv_uncert_econ_',reg,'_',unique(dat_indiv$year),'.pdf')
  ggsave(file=file.path(name), width = 200, height = 150, units = 'mm', plot = pl_indiv)
  
  return(pl_indiv)
}



#' do_econ_uncert.propagation
#' 
#' @param dat_to_plot dataset
#' @param reg region
#' @return Figure: individual uncertainty along avoided damages computation factors
do_econ_uncert.propagation = function(dat_to_plot,reg) {
  dat_to_plot = data.table(dat_to_plot)
  # propagated uncertainty
  dat_medi_general = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                     v05 = quantile(value,0.05),
                                     v25 = quantile(value,0.25),
                                     v75 = quantile(value,0.75),
                                     v95 = quantile(value,0.95)),
                                 by = c('year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_general = pivot_longer(dat_medi_general, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_general$ci = 'general'
  dat_medi_iam = dat_to_plot[,  .(v50 = quantile(value, 0.5),
                                  v05 = quantile(value,0.05),
                                  v25 = quantile(value,0.25),
                                  v75 = quantile(value,0.75),
                                  v95 = quantile(value,0.95)),
                             , by=c('model','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_iam = pivot_longer(dat_medi_iam, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_iam$ci = '+iam'
  dat_medi_method = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                    v05 = quantile(value,0.05),
                                    v25 = quantile(value,0.25),
                                    v75 = quantile(value,0.75),
                                    v95 = quantile(value,0.95)),
                                , by=c('model','method','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_method = pivot_longer(dat_medi_method, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_method$ci = '+method'
  dat_medi_alpha = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                   v05 = quantile(value,0.05),
                                   v25 = quantile(value,0.25),
                                   v75 = quantile(value,0.75),
                                   v95 = quantile(value,0.95)),
                               , by=c('model','method','alpha','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_alpha = pivot_longer(dat_medi_alpha, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_alpha$ci = '+alpha'
  dat_medi_impfun = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                    v05 = quantile(value,0.05),
                                    v25 = quantile(value,0.25),
                                    v75 = quantile(value,0.75),
                                    v95 = quantile(value,0.95)),
                                , by=c('model','method','alpha','impact_function_group','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_impfun = pivot_longer(dat_medi_impfun, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_impfun$ci = '+impfun'
  
  dat_medi_param = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                   v05 = quantile(value,0.05),
                                   v25 = quantile(value,0.25),
                                   v75 = quantile(value,0.75),
                                   v95 = quantile(value,0.95)),
                               , by=c('model','method','alpha','impact_function_group','parameter','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_param = pivot_longer(dat_medi_param, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_param$ci = '+param'
  
  dat_medi_cf = dat_to_plot[, .(v50 = quantile(value, 0.5),
                                v05 = quantile(value,0.05),
                                v25 = quantile(value,0.25),
                                v75 = quantile(value,0.75),
                                v95 = quantile(value,0.95)),
                            , by=c('model','method','alpha','impact_function_group','parameter','cf','year')] %>%
    dplyr::select(year,v05,v25,v50,v75,v95) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(v50 = median(v50),
                  v05 = min(v05),
                  v25 = median(v25),
                  v75 = median(v75),
                  v95 = max(v95)) %>%
    unique()
  dat_medi_cf = pivot_longer(dat_medi_cf, cols = c(v50, v05, v25, v75, v95), names_to = 'quantiles', values_to = 'value')
  dat_medi_cf$ci = '+cf'
  
  dat_prop = bind_rows(dat_medi_general, dat_medi_iam, dat_medi_method, dat_medi_alpha, dat_medi_impfun, dat_medi_param, dat_medi_cf)
  dat_prop = pivot_wider(dat_prop, names_from = quantiles, values_from = value)
  pl_prop = dat_prop %>%
    mutate(ci = ifelse(ci == '+alpha', '+elast', ci)) %>%
    mutate(ci = factor(ci, levels=c("general", "+iam", "+method", "+elast", "+impfun", "+param", "+cf"))) %>%
    ggplot(aes(x = ci)) +
    geom_boxplot(aes(ymin = v05, lower = v25, middle = v50, upper = v75, ymax = v95), stat = 'identity', width = 0.9) +
    geom_hline(yintercept = 0, color = 'grey', linetype = 'dotted') +
    facet_grid(. ~ year) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    scale_y_continuous(labels = scales::comma) +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = "grey90"), panel.ontop = FALSE,
          strip.text = element_text(size = 20), strip.background = element_blank(),
          axis.title = element_text(size = 20), axis.text = element_text(size = 15),
          legend.position = "none") +
    labs(y = 'US Billion', x = 'Added uncertainty sources')
  
  name = paste0('paper_figures/SI/uncert/prop_uncert_econ_',reg,'_',unique(dat_prop$year),'.pdf')
  ggsave(file=file.path(name), width = 200, height = 150, units = 'mm', plot = pl_prop)
  
  return(pl_prop)
}



