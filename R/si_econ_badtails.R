############################################################################################
#                                   EXCEEDANCE PROBABILITY                                 #
############################################################################################

doECON_compute_bad_tails_prob = function(datIni,th) {
  # add threshold column, by region and pollutant
  dat_th = data.table(datIni)[, .(threshold = quantile(value, th)),
                  by=c('pollutant','region','scenario')]
  dat_th = dat_th[scenario == 'NZ']
  
  # define time span
  time_id <- unique(datIni$year)
  time_size <- length(time_id)
  
  # DEFINE VARIABLES THAT COLLECT RESULTS
  # define the tail probabilities for the different impacts, regions and years
  tail_probability <- array(0, c(7, time_size))
  
  # define the thresholds for the different impacts and regions
  impact_threshold <- array(0)
  
  # define a test to check whether there is equal exceeding probability, i.e.
  # H_0 (or null hypothesis) against the alternative hypothesis H_1, i.e. that
  # the NZ distribution has longer tail than the EoC distribution.
  # N.B. We look for empirical evidence to reject H_0
  impact_test <- array(0, c(time_size))
  
  # define the significance level of the test
  method <- 0.05
  
  binom_test = NULL
  
  #LOOP REGION
  for (reg in unique(datIni$region)) {
    # LOOP CARBON BUDGET
    for(cbg in unique(datIni$cb_group)) {
      # LOOP IMPACT FUNCTION
      for(meth in unique(datIni$method)) {
        # LOOP YEARS
        for(yr in time_id){
          
          k = ifelse(yr == 2030, 1,
                     ifelse(yr == 2050, 2, 3))
          
          d = datIni |> filter(region == reg & cb_group == cbg
                                 & method == meth & year == yr) %>%
            as.data.table()
          
          time_ssize <- nrow(d[scenario == 'NZ'])
          
          poll = unique(d$pollutant)
          time_num_exceed <- c(sum(d[scenario == "EoC" & year == yr]$value > dat_th[region == reg & pollutant == poll]$threshold),
                               sum(d[scenario == "NZ" & year == yr]$value > dat_th[region == reg & pollutant == poll]$threshold))
          
          # estimation of the exceedance probability for the NZ distribution
          est <- binom::binom.confint(x = time_num_exceed[1], n = time_ssize,
                                      conf.level = 0.95, tol = 1e-8, methods = "exact" )
          tail_probability[1,k] <- est[1,4] # mean
          tail_probability[2,k] <- est[1,5] # lower
          tail_probability[3,k] <- est[1,6] # upper
          # estimation of the exceedance probability for the EoC distribution
          est <- binom::binom.confint(x = time_num_exceed[2], n = time_ssize,
                                      conf.level = 0.95, tol = 1e-8, methods = "exact")
          tail_probability[4,k] <- est[1,4] # mean
          tail_probability[5,k] <- est[1,5] # lower
          tail_probability[6,k] <- est[1,6] # upper
          
          # perform the test for checking whether we reject the null hypothesis
          test <- stats::binom.test(time_num_exceed[1], time_ssize, tail_probability[4,k],
                                    alternative = "greater")
          
          # save test outcome
          impact_test[k] <- test$p.value
          
          # save number of data sets
          tail_probability[7,k] = as.integer(length(unique(d[scenario == "NZ" & year == yr]$value)))
        }
        impact_rejection <- sum(impact_test < 0.05)
        
        res_EoC = data.table(yr = time_id, value = tail_probability[1,],
                             lower = tail_probability[2,], upper = tail_probability[3,])
        res_EoC[,scenario := 'EoC']
        res_NZ = data.table(yr = time_id, value = tail_probability[4,],
                            lower = tail_probability[5,], upper = tail_probability[6,])
        res_NZ[,scenario := 'NZ']
        res = rbind(res_NZ, res_EoC)
        res[,dtsets := tail_probability[7,1]]
        res[,method := meth]
        res[,cb_group := cbg]
        res[,region := reg]
        binom_test = rbindlist(list(binom_test,res))
      }
    }
  }
  
  return(binom_test)
}

doECON_plot_bad_tails_prob = function(datIni,th,SI=TRUE) {
  #plot
  datall = data.frame(datIni)
  datall$cb_group <- factor(datall$cb_group, levels = c("<1000", "[1000,2000]", ">2000"))
  
  for (reg in unique(datIni$region)) {
    dat_to_plot = datall |> filter(region == reg)
    
    pl_2030 <- ggplot(data=dat_to_plot %>% filter(yr == 2030), aes(x=as.factor(yr), y=value, fill = scenario)) +
      geom_bar(stat="identity", width = 0.2, position = position_dodge())+
      geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.35, color = scenario),alpha=0.5) +
      geom_hline(yintercept = 0) +
      facet_grid(method ~ cb_group,
                 labeller = labeller(method = method_av.labs))+ #,scales='free'
      theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
            panel.ontop = FALSE,legend.position = "bottom",strip.background = element_rect(fill = 'white', color = 'white')) +
      ggtitle('2030') +
      xlab('Year') + ylab('Exceedance probability') +
      scale_color_manual(values = scenario.colors,
                         name = 'Climate Policy')+
      scale_fill_manual(values = scenario.colors,
                        name = 'Climate Policy')
    if (SI) {
      pl_2030 <- pl_2030 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                                 strip.text.y = element_blank(), axis.title.x = element_blank())
      
      pl_2050 <- ggplot(data=dat_to_plot %>% filter(yr == 2050), aes(x=as.factor(yr), y=value, fill = scenario)) +
        geom_bar(stat="identity", width = 0.2, position = position_dodge())+
        geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.35, color = scenario),alpha=0.5) +
        geom_hline(yintercept = 0) +
        facet_grid(method ~ cb_group,
                   labeller = labeller(method = method_av.labs))+ #,scales='free'
        theme(panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"),
              panel.ontop = FALSE, axis.text.y = element_blank(),legend.position = "bottom",
              strip.background = element_rect(fill = 'white', color = 'white'))+
        ggtitle('2050') +
        xlab('Year') + ylab('') +
        scale_color_manual(values = scenario.colors,
                           name = 'Climate Policy')+
        scale_fill_manual(values = scenario.colors,
                          name = 'Climate Policy')+
        theme(strip.text.y = element_text(size = 8), axis.text.x = element_blank(),
              axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
        rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5)
      
      # Unify plot
      pl = ggarrange(pl_2030 + font("title", size = 8) + labs(title = ''),
                     pl_2050 + font("title", size = 8) + labs(title = ''),
                     labels = c('2030','2050'),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend="bottom",
                     widths = c(1,1.2))
      pl = annotate_figure(pl,
                           top = text_grob(do_rename_regions_string(reg), color = "black", face = "bold", size = 13))
      
      name_file = paste0("Results/Econ/ExceedanceProb/",th,'/',reg)
      print(name_file)
      ggsave(file=paste0(name_file,'.pdf'), width = 200, height = 200, units = "mm",plot = pl, limitsize = FALSE)
    } else {
      return(pl_2030 + rotate_y_facet_text(angle = 0, align = 0.5, valign = 0.5))
    }
  }
}

do_econ_badtails_folder = function(th) {
  if(!dir.exists(file.path('Results/Econ/ExceedanceProb',th))){
    if(!dir.exists(file.path('Results/'))){
      dir.create(file.path('Results/'))
    }
    if(!dir.exists(file.path('Results/Econ/'))){
      dir.create(file.path('Results/Econ/'))
    }
    if(!dir.exists(file.path('Results/Econ/ExceedanceProb'))){
      dir.create(file.path('Results/Econ/ExceedanceProb'))
    }
    dir.create(file.path('Results/Econ/ExceedanceProb',th))
  }
}