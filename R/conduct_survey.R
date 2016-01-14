#' Conduct Survey
#'
#' Give function arguments for
#' This function initializes the spatial distribution of the fish population

#'@param fish_area Matrix with the distribution of fish
#'@param location_list list specifying rows and columns to survey in
#'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
#'@param nhooks number of hooks at the smallest sampling size
#'@param ndrops number of drops, default is 5 following hook and line protocol
#'@param process specify process by which fish are sampled, options are 'multinomial' and 'hypergeometric'
#' @keywords survey
#' @export
#' @examples
#' put example here dude
#'



conduct_survey <- function(fish_area, location_list, scope, nhooks, ndrops, process = 'hypergeometric'){

  #keep initial population matrix
  init_area <- init

  #Name sample list
  sample_list <- vector('list', length = length(location_list))
  names(sample_list) <- paste(location_list)

  #Sample at locations with for loop
  for(zz in 1:length(sample_list)){
# print(zz)
     # if(zz == 16) browser()
    # location <- location_list[[zz]]
    temp <- fish_population(fish_area = fish_area, location = location_list[[zz]], scope = scope,
                    nhooks = nhooks, ndrops = ndrops, process = process)
    fish_area <- temp[[1]]
    sample_list[[zz]] <- temp[[2]]

  }

  samples <- ldply(sample_list)
  names(samples) <- c('location', paste0('drop', 1:ndrops))

  out <- list(init_area = init_area, sampled_area = fish_area, samples = samples,
       cpue = samples[2:(ndrops + 1)] / nhooks)

  return(out)
}
