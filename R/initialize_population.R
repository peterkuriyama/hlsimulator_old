#' Initialize Population
#'
#' This function initializes the spatial distribution of the fish population
#' @param numrow Number of rows in matrix
#' @param numcol Number of columns in matrix
#' @param nfish Number of fish to allocate among matrix
#' @param seed Set seed if distribute == random, defaults to 300
#' @param distribute Specify if the fish distribution be 'uniform', 'patchy' or 'random'
#' @param maxfish Maximum number of fish that can be sampled at a time
#' @param percent percentage of area to sample. Only necessary if distribute == 'patchy'
#' @param area Specify area to distribute fish, options are 'upperleft', 'upperright', 'lowerleft', 'lowerright'. 
#'   Only necessary if distribute == 'random'
#' @keywords initialize
#' @export
#' @examples 
#' put example here dude

initialize_population <- function(numrow, numcol, nfish = 100, seed = 300, distribute,  
  maxfish = 10, area = 'upperleft', percent = .1){
  #Create matrix of zeroes
  fishArea <- matrix(0, nrow = numrow, ncol = numcol)
  
  #---------------------------------------------------------------------------------------------------------
  # Uniformly populate matrix, work on this
  if(distribute == 'uniform'){
    #Spit out error if nfish is not large enough to have whole fish in each cell
    # if(nfish %% nrow * ncol != 0) stop('must have whole fish in each cell, adjust nfish, nrow or ncol')
    
    #Specify 
    # samp.df <- expand.grid(1:numrow, 1:numcol)
    # names(samp.df) <- c('x', 'y')
    
    # samp.df

    nfish.uni <- nfish - (nfish %% nrow(samp.df)) #number of fish for uniform allocation
    nfish <- nfish - nfish.uni
    
    # #Initialize empty vector of samples and the counter
    # samp.vec <- vector(length = nfish)
    # counter <- 1
    
    # #While loop generates samples
    # while(nfish > 0){
    #   samp <- sample(1:maxfish, 1) #Maximum number of fish allowed per sample
    #   if(samp >= nfish) samp <- nfish #prevents nfish from being exceeded
    
    #   samp.vec[counter] <- samp #store value in counter
      
    #   nfish <- nfish - samp #update nfish
    #   counter <- counter + 1 #update counter
    # }


    # samp.df$
    # nfish / nrow(samp.df)

  }
  
  #---------------------------------------------------------------------------------------------------------
  #Patchily Distributed Fish
  if(distribute == 'patchy'){
    #set seed
    # set.seed(seed)

    #Maybe specify percentage of things to pick ultimately??
    possible.picks <- expand.grid(1:numrow, 1:numcol)
    nsamps <- percent * nrow(possible.picks)

    samp.df <- possible.picks[sample(1:nrow(possible.picks), size = nsamps), ] #hard coded to populate 10 cells now
    names(samp.df) <- c('x', 'y')
    
    
  }
    
  #---------------------------------------------------------------------------------------------------------
  #If distribution is area specific...
  #Populate matrix with fish
  if(distribute == 'area'){
    
    #Adjust rows and columns depending on specified area
    if(area == 'upperleft'){
      rows <- 1:(numrow / 2)
      columns <- 1:(numcol / 2)
    }
    
    if(area == 'upperright'){
      rows <- 1:(numrow / 2)
      columns <- (1 + (numcol / 2)):numcol
    }
    
    if(area == 'lowerleft'){
      rows <- (1 + (numrow / 2)):numrow
      columns <- 1:(numcol / 2)
    }
    
    if(area == 'lowerright'){
      rows <- (1 + (numrow / 2)):numrow
      columns <- ((1 + numcol / 2)):numcol
    }

    #set seed
    # set.seed(seed)
    
    #Populate matrix with samples
    #For now only does upper left of matrix
    #Not allowed to populate with more than nfish
    #Will continue until all fish are allocated
    
    #Initialize empty vector of samples and the counter
    # samp.vec <- vector(length = nfish)
    # counter <- 1
    
    #While loop generates samples
    # while(nfish > 0){
    #   samp <- sample(1:maxfish, 1) #Maximum number of fish allowed per sample
    #   if(samp >= nfish) samp <- nfish #prevents nfish from being exceeded
    
    #   samp.vec[counter] <- samp #store value in counter
      
    #   nfish <- nfish - samp #update nfish
    #   counter <- counter + 1 #update counter
    # }

    #create data frame with matrix indices of interest
    # samp.df <- expand.grid(rows, columns) #rows and columns are set depending on arguments
    # names(samp.df) <- c('x', 'y')
    
    # #Ensure that the length of sample vec is a multiple of number of rows in samp.df
    # samp.vec <- c(samp.vec, rep(0, length(samp.vec) %% nrow(samp.df)))
    
    # #These steps Sum the values which are stored by row
    # samp.mat <- matrix(samp.vec, nrow = nrow(samp.df)) 
    # samp.df$fish <- rowSums(samp.mat) 
    
    # #assign to fishing area
    # for(ii in 1:nrow(samp.df)){
    #   fishArea[samp.df[ii, 1], samp.df[ii, 2]] <- samp.df[ii, 3]
    # }
  }
  set.seed(seed)
  #Now sample fish
  samp.vec <- vector(length = nfish)
  counter <- 1
  
  #While loop generates samples
  while(nfish > 0){
    samp <- sample(1:maxfish, 1) #Maximum number of fish allowed per sample
    if(samp >= nfish) samp <- nfish #prevents nfish from being exceeded
    
    samp.vec[counter] <- samp #store value in counter
    
    nfish <- nfish - samp #update nfish
    counter <- counter + 1 #update counter
  }

  #Ensure that the length of sample vec is a multiple of number of rows in samp.df
  samp.vec <- c(samp.vec, rep(0, length(samp.vec) %% nrow(samp.df)))
  samp.mat <- matrix(samp.vec, nrow = nrow(samp.df))
  samp.df$fish <- rowSums(samp.mat)

# browser()
  #Add uniform # of fish to each cell
  if(distribute == 'uniform'){
    samp.df$fish <- samp.df$fish + nfish.uni / nrow(samp.df)
  }
  
  #assign to fishing area
  for(ii in 1:nrow(samp.df)){
    fishArea[samp.df[ii, 1], samp.df[ii, 2]] <- samp.df[ii, 3]
  }  
  return(fishArea)
}