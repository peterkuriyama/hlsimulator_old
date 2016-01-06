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
  # browser()
  
  #Create matrix of zeroes
  fishArea <- matrix(0, nrow = nrow, ncol = ncol)
  
  # Uniformly populate matrix
  if(distribute == 'uniform'){
    #Spit out error if nfish is not large enough to have whole fish in each cell
    # if(nfish %% nrow * ncol != 0) stop('must have whole fish in each cell, adjust nfish, nrow or ncol')
    
#     apply(fishArea)
#     
#     fishArea <-
  }
  
  
  #Populate matrix with fish
  if(distribute == 'random'){
    #set seed
    set.seed(seed)
    
    #Populate matrix with samples
    #For now only does upper left of matrix
    #Not allowed to populate with more than nfish
    
    for(ii in 1:(nrow / 2)){
      for(jj in 1:(ncol / 2)){
        samp <- sample(1:10, 1)
        
        if(samp > nfish){
          samp <- nfish
          nfish <- nfish - samp
        } else {
          nfish <- nfish - samp
        } 
        
        fishArea[ii, jj] <- samp
      }
    }
    
  }
  
  return(fishArea)
}