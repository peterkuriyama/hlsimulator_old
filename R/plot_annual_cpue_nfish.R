#' Plot annual cpue nfish
#'
#' Plot annual averaged cpue and true number of fish
#'
#' @param parsed_list output of survey_over_years melted for formatting
#' @param cex.axis spcify size of axis, all other cex paramters scaled to this
#' @param text_space spacing inbetween text descriptions in upper left of plot
#' @keywords plot
#' @export
#' @examples 
#' plot example

plot_annual_cpue_nfish <- function(parsed_list, cex.axis = 1.2, text_space = .25, ...){


  nfish_cpue <- merge(parsed_list$end_nfish %>% 
                        group_by(year) %>% 
                        summarise(nfish = sum(value)),
                      parsed_list$cpue %>% 
                        group_by(year) %>% 
                        summarise(cpue = mean(value)) %>% 
                        as.data.frame, by = 'year')

  #Get ellipses arguments
  dot_args <- list(...)

# match.call(get, call(dot_args$move_func))

  #Get values from simulation to add descriptions to plots and filename
  nfish <- dot_args$nfish
  nlocs <- dot_args$nlocs
  distribute <- get('distribute', parent.frame())
  move_func_name <- dot_args$move_func_name

  nlocs_total <- parsed_list$init_nfish %>% filter(year == 1) %>% summarise(loc = length(value))

  perc_sampled <- nlocs / nlocs_total
  rand <- get('random_locations', parent.frame())

  plot(nfish_cpue$nfish, nfish_cpue$cpue, ylim = c(0, 1), pch = 19,
    xaxs = 'i', yaxs = 'i', xlim = c(0, nfish), type = 'o', axes = FALSE,
    ann = FALSE, cex = 1.2 * cex.axis, lwd = cex.axis * 1.2, xpd = TRUE)

  #Add axes
  axis(side = 1, at = pretty(0:nfish),
    labels = format(pretty(0:nfish), big.mark = ',', scientific = FALSE),
    cex.axis = cex.axis)
# browser()
  mtext(side = 1, outer = T, "True Number of Fish", line = -2, cex = cex.axis * 1.3)
  mtext(side = 2, outer = T, "CPUE (nfish / nhooks)", line = -1.5, cex = cex.axis * 1.3)
  # mtext(side = 2, outer = T, "CPUE (nfish / nhooks)")
  axis(side = 2, las = 2, cex.axis = cex.axis)

  #Add text
  #specify text spacing
  spacing <- seq(1, 10, by = (1 + text_space))

  mtext(side = 3, paste0(perc_sampled * 100, "% of ", nlocs_total ," locations sampled"), 
    adj = .01, line = -spacing[1], cex = cex.axis * .8)
  mtext(side = 3, paste0(format(nfish, big.mark = ','), ' fish initially'), 
    adj = .01, line = -spacing[2], cex = cex.axis * .8)
  mtext(side = 3, paste0( distribute, ' Initial Fish Distribution'), 
    adj = .01, line = -spacing[3], cex = cex.axis * .8)
  
  if(move_func_name == 'move_fish_cw'){
    mtext(side = 3, 'Clockwise Fish Movement', 
      adj = .01, line = -spacing[4], cex = cex.axis * .8)  
  }

  if(move_func_name == 'move_fish_left'){
    mtext(side = 3, 'Offshore (left) Fish Movement', 
      adj = .01, line = -spacing[4], cex = cex.axis * .8)  
  }

  if(rand == TRUE) 
  {
    mtext(side = 3, 'Random Location Sampling', 
      adj = .01, line = -spacing[5], cex = cex.axis * .8)
  }

}

