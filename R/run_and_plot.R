#' Run and Plot Function
#'
#' High level wrapper function to run simulation and generate certain plots
#'
#' @param numrow Number of rows in matrix
#' @param numcol Number of columns in matrix
#' @param nfish Number of fish to allocate among matrix
#' @param seed Set seed if distribute == random, defaults to 300
#' @param distribute Specify fish distribution to be 'uniform', 'patchy', or 'area' specific
#' @param maxfish Maximum number of fish that can be sampled at a time
#' @param percent percentage of area to sample. Only necessary if distribute == 'patchy'
#' @param area Specify area to distribute fish, options are 'upperleft', 'upperright', 'lowerleft', 'lowerright',
#' 'upperhalf', 'lowerhalf', 'righthalf', 'lefthalf' 
#'   Only necessary if distribute == 'area'
#' @param move_func input movement function name as text. Later parsed into function call
#' @keywords initialize
#' @export
#' @examples 
#' run_and_plot(numrow = 10, numcol = 10, nfish = 10000,
#'  distribute = 'uniform', seed = 300, nyears = 15, random_locations = TRUE, 
#'  nlocs = 10, move_func = 'move_fish_left', max_prob = .2, min_prob = .01)
#'
#'  run_and_plot(numrow = 10, numcol = 10, nfish = 1000,
#'    distribute = 'uniform', seed = 300, nyears = 15, random_locations = TRUE, 
#'    nlocs = 10, move_func = 'move_fish_cw', move_prob = .8)


run_and_plot <- function(numrow = 10, numcol = 10, nfish = 1000, distribute,
  seed = 300, nyears = 15, location_list, random_locations = FALSE, nlocs = 10,
  move_func, ...){

  if(is.character(move_func) == FALSE) stop('movement function must be character string')
  move_func_name <- move_func
  move_func <- eval(parse(text = move_func))

  run <- survey_over_years(numrow = numrow, numcol = numcol, nfish = nfish, 
              distribute = distribute,
              seed = seed, nyears = nyears, location_list = location_list, 
              random_locations = random_locations, nlocs = nlocs, 
              move_func = move_func, ...)
              

  run <- parse_master_list(run)

  plot_annual_cpue_nfish(parsed_list = run, nfish = nfish,
    distribute = distribute, seed = seed, nyears = nyears, 
    nlocs = nlocs, move_func_name = move_func_name, ...)

  return(run)
}


