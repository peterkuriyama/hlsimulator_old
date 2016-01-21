#' Initialize Population
#'
#' This function initializes the spatial distribution of the fish population
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
#' @keywords initialize
#' @export
#' @examples 
#' 
#' Uniformly distribute fish
#' initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'uniform')
#' 
#' Distribute fish in upper right quadrant only
#' initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'area',
#'                             area = 'upperright')
#'
#' Distribute fish in upper half
#' initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'area',
#'                             area = 'upperhalf')
#'
#' Patchily distribute fish
#' initialize_population(numrow = 10, numcol = 10, nfish = 100, distribute = 'patchy',
#'   percent = .2)
#'
#' 

survey_over_years <- function(numrow = 10, numcol = 10, nfish = 1000, distribute,
  seed = 300, nyears = 15, location_list, random_locations = FALSE, nlocs = 10){

  init <- initialize_population(numrow = numrow, numcol = numcol, nfish = nfish, 
        distribute = distribute, seed = seed)

  #Specify locations to fish if random locations is TRUE
  if(random_locations == TRUE){
    potential_location <- expand.grid(1:numrow, 1:numcol)
    sampled <- base::sample(1:nrow(potential_location), replace = FALSE, size = nlocs)
    location_list <- potential_location[sampled, ]
    location_list <- split(location_list, f = seq(nrow(location_list)))
    location_list <- lapply(location_list, FUN = function(x) c(as.numeric(x[1]), 
      as.numeric(x[2])))
    names(location_list) <- NULL
  }

  #conduct nyears iterations
  temp_area <- init
  master_list <- vector('list', length = nyears)
  cpue_list <- vector('list', length = nyears)


  for(ii in 1:nyears){
    temp <- conduct_survey(fish_area = temp_area, location = location_list, 
            scope = 0, nhooks = 15, ndrops = 3,
            process = 'hypergeometric')
    master_list[[ii]] <- temp

    cpue <- temp$cpue
    cpue$year <- ii
    cpue_list[[ii]] <- cpue

    temp_area <- temp$sampled_area
    temp <- move_fish_left(temp_area)
    temp_area <- temp$final

  }
  
  # cpues <- ldply(cpue_list)
  # cpues <- melt(cpues, id = c('location', 'year'))
  # nchange <- sapply(master_list, function(x) sum(x$sampled_area))
  # # cpues$nfish <- nchange

  # cpues %>% 
  #   group_by(year) %>% 
  #   summarise(mm = mean(value)) %>% 
  #   as.data.frame %>% 
  #   mutate(nfish = nchange) %>% 
  #   ggplot(aes(x = nfish, y = mm)) + geom_line()

  #return cpues
  return(master_list)
}