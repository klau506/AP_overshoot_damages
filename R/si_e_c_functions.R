source('zzz.R')

################################################################################
#                                    KS TEST                                   #
################################################################################

ks_testt = function(datIni) {
  res_test = NULL
  # LOOP YEAR
  for (y in c(2030,2050)) {
    #LOOP REGION
    for (reg in unique(datIni$region)) {
      # LOOP CARBON BUDGET
      for(cbg in unique(datIni$cb_group)) {
        # LOOP POLLUTANT
        for(poll in unique(datIni$pollutant)) {
          dat = datIni |> filter(year == y & region == reg & cb_group == cbg)
          
          res = setDT(glance(ks.test(dat[scenario == "NZ" & pollutant == poll, value],
                                     dat[scenario == "EoC" & pollutant == poll, value],
                                     alternative = 'two.sided')))
          res
          res[,dataNZ := length(dat[scenario == "NZ" & pollutant == poll, value])]
          res[,dataEoC := length(dat[scenario == "EoC" & pollutant == poll, value])]
          res[,pollutant := poll]
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


################################################################################
#                                DISTRIB PLOT                                  #
################################################################################
#datIni = data to plot: datEmi or datConc
#reg = R10 region
#poll = pollutant
#xx = TRUE if the years should be displayed, FALSE otherwise
#yy = TRUE if the cb_groups should be displayed, FALSE otherwise
#xxbb = OX label
#E = TRUE if Emissions, FALSE if Concentrations
do_distrib_plot = function(datIni,reg,poll,xx,yy,xxbb,E) {
  dat_to_plot = datIni |> filter(year %in% c(2030,2050) & region == reg & pollutant == poll)
  
  dat_to_plot$cb_group <- factor(dat_to_plot$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  datall_medi <- dat_to_plot[, .(medi = quantile(value, 0.5)),
                             by=c('year','region','pollutant','cb_group','scenario')]
  pl_title = paste(reg,poll,sep = '_')
  
  round_num <<- 1
  pl <- ggplot(dat_to_plot) +
    geom_density(aes(x = value, group = interaction(scenario,pollutant,year,cb_group),
                     color = scenario, fill = scenario), alpha = 0.5) +
    geom_vline(aes(color = scenario, xintercept = medi),
               data = datall_medi, linetype="dashed", linewidth = 1) +
    facet_grid(cb_group ~ year,scales='free')+
    scale_color_manual(values = scenario.colors,
                       name = 'Climate policy design')+
    scale_fill_manual(values = scenario.colors,
                      name = 'Climate policy design')+
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5)+
    scale_y_continuous(breaks = custom_y_labels) +
    labs(title='', x = xxbb, y = "Probability density") + 
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black'), strip.background = element_rect(fill = '#e8e8e8'),
          strip.text.y = element_text(size = 7))
  
  pl
  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  return(pl)
}

################################################################################
#                                CUM FREQ PLOT                                 #
################################################################################
#datIni = data to plot: datEmi or datConc
#reg = R10 region
#poll = pollutant
#xx = TRUE if the years should be displayed, FALSE otherwise
#yy = TRUE if the cb_groups should be displayed, FALSE otherwise
#xxbb = OX label
#E = TRUE if Emissions, FALSE if Concentrations
do_cumm_plot = function(datIni,reg,poll,xx,yy,xxbb,E) {
  dat_to_plot = datIni |> filter(year %in% c(2030,2050) & region == reg & pollutant == poll)
  dat_to_plot$cb_group <- factor(dat_to_plot$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  dat_tmp <- ddply(dat_to_plot, .(scenario,cb_group,pollutant,year), summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))
  pl_title = paste(reg,poll,sep = '_')
  
  round_num <<- 1
  pl <- ggplot(dat_tmp, aes(value, ecdf, color = scenario, fill = scenario)) + geom_line() +
    facet_grid(cb_group ~ year,scales='free')+
    scale_color_manual(values = scenario.colors,
                       name = 'Climate policy design')+
    scale_fill_manual(values = scenario.colors,
                      name = 'Climate policy design')+
    rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5) +
    scale_y_continuous(breaks = custom_y_labels) +
    labs(title='', x = xxbb, y = "Cumulative frequency") +
    theme_pubr() +
    theme(panel.background = element_rect(fill = 'grey99'), panel.grid.major = element_line(colour = "grey90"),
          panel.ontop = FALSE, legend.direction = 'horizontal', legend.box = 'vertical', legend.position = "bottom",
          strip.text = element_text(color = 'black'), strip.background = element_rect(fill = '#e8e8e8'),
          strip.text.y = element_text(size = 7))
  
  if(!xx) {
    pl = pl + theme(strip.text.x = element_blank())
  }
  if(!yy) {
    pl = pl + theme(strip.text.y = element_blank())
  }
  
  return(pl)
}