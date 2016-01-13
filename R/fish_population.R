#'Fish the Population
#'
#'Function to fish the population
#'@param fish_area Matrix with the distribution of fish
#'@param location vector specifying row and column to fish in
#'@param scope the scope of fishing movement, default to 1 so fish in surrounding 1 cells can move in
#'@param nhooks number of hooks at the smallest sampling size
#'@param ndrops number of drops, default is 5 following hook and line protocol
#'@param process specify process by which fish are sampled, options are 'multinomial' and 'hypergeometric'
#may need to add angler specifications in at each time
#currently it's just 15 hooks per drop, without the ability to specify angler 
#location on boat

#also play with sampling probabilities and movements

fish_population <- function(fish_area, location, scope = 1, nhooks, ndrops,
  process){
  #------------------------------------------------------
  #Count fish within the range
  row_range <- (location[1] - scope):(location[1] + scope)
  row_range <- row_range[row_range %in% 1:nrow(fish_area)]

  col_range <- (location[2] - scope):(location[2] + scope)
  col_range <- col_range[col_range %in% 1:ncol(fishArea)]

  fish_range <- fish_area[row_range, col_range] #define range to fish...
  fish_in_loc <- fish_area[location[1], location[2]] #Number of fish in specified location

  nfish_outside <- sum(fish_range) - fish_in_loc

  #------------------------------------------------------
  #Move fish to specified location
  
  #Calculate movement probabilities
  #currently nothing goes out of specified location
  #Movement probabilites depend on number of fish outside relative to 
  #total fish outside
  probs <- fish_range / nfish_outside
  
  #create data frame of fish_range [fish to catch] because 
    #easier to manipulate
  fish_df <- melt(fish_range)
  fish_df$prob <- melt(probs)$value

  #Probability in specified location is zero
  zero_index <- which(fish_df$Var1 == 2 & fish_df$Var2 == 2)
  fish_df[zero_index, 'prob'] <- 0

  #Calculate number of moving fish with binomial distribution
  fish_df$moving <- apply(fish_df, MAR = 1, FUN = function(x) rbinom(n = 1, size = x['value'], 
    prob = x['prob']))

  #Now update fish
  fish_df$moved <- fish_df$value - fish_df$moving #moved column indicates nfish after movement
  fish_df[zero_index, 'moved'] <- fish_df[zero_index, 'value'] + sum(fish_df$moving)
  
  #------------------------------------------------------
  #Now fish in specified cell, called zero.index
  fish_to_catch <- fish_df[zero_index, 'moved']
  # lambda <- fish_to_catch / nhooks #expected cpue (nfish / nhooks)
  
  hookProbs <- rep(1 / (nhooks + 1), (nhooks + 1)) #All Hooks have equal probability
  catches <- matrix(nrow = (nhooks + 1), ncol = nhooks)

  
  if(process == 'hypergeometric'){

  }


  #multinomial process is still really in development
  if(process == 'multinomial'){
    for(ii in 1:nhooks){
    catches[, ii] <- rmultinom(1, size = 1, prob = hookProbs)

    rmultinom(1, size = fish_to_catch, prob = hookProbs)

    #update hook probabilties if fish are caught
    if (sum(catches[, ii]) == 1 & which(catches[, ii] == 1) != 1) {
      hookProbs[1] <- hookProbs[1] + hookProbs[which(catches[, ii] == 1)]
      hookProbs[which(catches[, ii] == 1)] <- 0
    }

   } 
  }


  fo
  
  # sum(catches)
  sum(catches[2:nrow(catches), ])
  
  #Catch fish with poisson sample
  samples <- rpois(ndrops, lambda = lambda) 

  #Update number of fish in each cell
  fish_df$fished <- fish_df$moved
  fish_df[zero_index, 'fished'] <- fish_df[zero_index, 'fished'] - sum(samples)
  
  #movement back to cells is based on proportions that moved in
  move_back_probs <- fish_df$moving
  move_back_probs[zero_index] <- fish_df[zero_index, 'value']

  # Sample from multinomial distribution
  moved_back <- as.vector(rmultinom(1, size = fish_df[zero_index, 'fished'], 
      prob = move_back_probs / sum(move_back_probs)))
  

  fish_df$delta <- moved_back
  #update fish counts
  fish_df$final <- fish_df$fished + fish_df$delta 
  fish_df[zero_index, 'final'] <- fish_df[zero_index, 'delta']

  #Update fish_area matrix
  fish_area[row_range, col_range] <- matrix(fish_df$final,
    nrow = nrow(fish_range), ncol = ncol(fish_range))

  return(list(updated_area = fish_area, samples = samples))

}
