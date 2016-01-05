#' Initialize Population
#'
#' This function initializes the spatial distribution of the fish population
#' @param nrow Number of rows in matrix
#' @param ncol Number of columns in matrix
#' @param nfish Number of fish to allocate among matrix
#' @param distribute Specify if the fish distribution be random or uniform
#' @param seed Set seed if distribute == random, defaults to 300
#' @keywords initialize
#' @export
#' @examples 
#' put example here dude

initialize_population <- function(nrow, ncol, nfish = 100, distribute, seed = 300){
  browser()
  
  #Initialize area
  fishArea <- matrix(0, nrow = nrow, ncol = ncol)
  
  
  
}