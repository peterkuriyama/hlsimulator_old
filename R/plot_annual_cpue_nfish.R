plot_annual_cpue_nfish <- function(parsed_list, cex.axis = 1.2, ...){

  nfish_cpue <- merge(parsed_list$end_nfish %>% group_by(year) %>% summarise(nfish = sum(value)),
      cpue %>% group_by(year) %>% summarise(cpue = mean(value)) %>% as.data.frame,
      by = 'year')
  # nfish <- max(parsed_list[[1]] %>% group_by(year) %>% summarise(sum(value)))


  #Get values from simulation to add descriptions to plots and filename
  #get nfish from parent environment
  nfish <- get('nfish', parent.frame())
  nlocs <- length(unique(parsed_list$samples$location))
  distribute <- get('distribute', parent.frame())

  nlocs_total <- parsed_list$init_nfish %>% filter(year == 1) %>% summarise(loc = length(value))

  perc_sampled <- nlocs / nlocs_total
  rand <- get('random_locations', parent.frame())

  plot(nfish_cpue$nfish, nfish_cpue$cpue, ylim = c(0, 1), pch = 19,
    xaxs = 'i', yaxs = 'i', xlim = c(0, nfish), type = 'o', axes = FALSE,
    ann = FALSE, cex = 1.2 * cex.axis, lwd = cex.axis * 1.2)

  axis(side = 1, at = pretty(0:nfish),
    labels = format(pretty(0:nfish), big.mark = ',', scientific = FALSE),
    cex.axis = cex.axis)
  axis(side = 2, las = 2, cex.axis = cex.axis)

  mtext(side = 3, paste0(perc_sampled * 100, "% of ", nlocs_total ," locations sampled"), 
    adj = .01, line = -1, cex = cex.axis * .8)
  mtext(side = 3, paste0("nfish = ", nfish), 
    adj = .01, line = -2, cex = cex.axis * .8)
  mtext(side = 3, paste0('Initial Fish Distribution ', distribute), 
    adj = .01, line = -3, cex = cex.axis * .8)
browser()
  if(rand == TRUE) 
  {
    mtext(side = 3, 'Random Sampling', 
      adj = .01, line = -5, cex = cex.axis * .8)
  }

}

