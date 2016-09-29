#' Move Fish to Hotspots
#'
#' This function is meant to simulate fishing moving offshore. One function call results in 
#' fish moving one column to the left with some probability (specified in the arguments). 
#' Probabilities decline evenly by column (highest on furthest left column). Note that the movement
#' is determined by the number of fish surrounding the designated hotspots. 

#' @param fish_area Input matrix of distributed fish
#' @param max_prob Maximum probability of moving towards hotspot, highest in cells adjacent to hotspot
#' @param hs_locs Locations of hotspots that fish are attracted to
#' @param hs_prob Probability of fish moving away from hotspot, default to 0.1

#' @keywords movement
#' @export
#' @examples 
#' 

fish_area <- initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
  nfish = 1000, percent = .50)

hs_locs <- data.frame("row" = c(4, 2), "column" = c(7, 10))
max_prob <- .3
out_prob <- .1
seed <- 300

move_fish_hs <- function(fish_area, max_prob, hs_locs , seed = 300, out_prob = .1, 
  ...){

  numrow <- nrow(fish_area)
  numcol <- ncol(fish_area)
  
  #Convert fish_area into a data frame to facilitate calculations
  fish_area_df <- melt(fish_area)
  names(fish_area_df) <- c('row', 'column', 'nfish')

  #create indices of pasted row and column
  fish_area_df$ind <- paste(fish_area_df$row, fish_area_df$column)
  hs_locs$ind <- paste(hs_locs$row, hs_locs$column)

  #Define probability of moving
  hs_inds <- which(fish_area_df$ind %in% hs_locs$ind) #indices of hotspots
  
  #First cut subset cells that are 1 away
  first <- subset(fish_area_df, row )


  ##If there are multiple hotspots, move fish in a for loop for each hotspot
  for(ii in 1:nrow(hs_locs)){
    
    #Define probabilities of moving based on proximity to the hotspot
    x <- hs_locs[ii, ]
    firsts <- expand.grid((as.numeric(x[1]) - 1):(as.numeric(x[1]) + 1),
      (as.numeric(x[2]) - 1):(as.numeric(x[2]) + 1))
    firsts$ind <- paste(firsts$Var1, firsts$Var2)
      
    fish_area_df[fish_area_df$ind %in% firsts$ind, 'prob'] <- max_prob

    seconds <- expand.grid((as.numeric(x[1]) - 2):(as.numeric(x[1]) + 2),
          (as.numeric(x[2]) - 2):(as.numeric(x[2]) + 2))
    seconds$ind <- paste(seconds$Var1, seconds$Var2)
    seconds <- seconds[-which(seconds$ind %in% firsts$ind), ]
    
    #Here I define the probability of moving for cells 2 away. Might need to change this
    fish_area_df[fish_area_df$ind %in% seconds$ind, 'prob'] <- max_prob / 2
    fish_area_df[fish_area_df$ind == x$ind, 'prob'] <- out_prob

    set.seed(seed)

    #Sample # moving fish from the number of fish in each cell
    fish_area_df$sample <- rbinom(n = length(fish_area_df$nfish),
      size = fish_area_df$nfish, prob = fish_area_df$prob)

    #Update the cells based on the draws, except for the 
    fish_area_df$nfish_new <- fish_area_df$nfish - fish_area_df$sample

    #Change the hotspot number of fish back to original
    hotspot <- which(fish_area_df$ind %in% x$ind )
    fish_area_df[hotspot, 'nfish_new'] <- fish_area_df[hotspot, 'nfish']
    
    #Randomly pick one of the firsts places to move the sampled fish from the hotspot
    picked_spot <- firsts[base::sample(1:nrow(firsts), 1), 'ind']
    

    fish_area_df[fish_area_df$ind == picked_spot, 'nfish_new'] <- 
      fish_area_df[fish_area_df$ind == picked_spot, 'nfish_new'] + fish_area_df[hotspot, 'sample']
    fish_area_df[hotspot, 'nfish_new'] <- fish_area_df[hotspot, 'nfish_new'] - fish_area_df[hotspot, 'sample']

    new_fish_area <- matrix(fish_area_df$nfish_new, nrow = nrow(fish_area), ncol = ncol(fish_area))


  }


  
  
  



firsts <- expand.grid((as.numeric(x[1]) - 1):(as.numeric(x[1]) + 1),
      (as.numeric(x[2]) - 1):(as.numeric(x[2]) + 1))
firsts$ind <- paste(firsts$Var1, firsts$Var2)
  
fish_area_df[fish_area_df$ind %in% firsts$ind, 'prob'] <- max_prob

seconds <- expand.grid((as.numeric(x[1]) - 2):(as.numeric(x[1]) + 2),
      (as.numeric(x[2]) - 2):(as.numeric(x[2]) + 2))
seconds$ind <- paste(seconds$Var1, seconds$Var2)
seconds <- seconds[-which(seconds$ind %in% firsts$ind), ]
fish_area_df[fish_area_df$ind %in% seconds$ind, 'prob'] <- max_prob / 2

matrix(fish_area_df$prob, nrow = 10, ncol = 10) 
  
  #sample fish in each cell then move to corresponding place in cw region
  set.seed(seed)
  fish_area_df$sample <- rbinom(n = length(fish_area_df$nfish),
    size = fish_area_df$nfish, prob = move_prob)
  fish_area_df$nfish_left <- fish_area_df$nfish - fish_area_df$sample

  moving_fish <- fish_area_df
  moving_fish$nfish <- NULL
  moving_fish$nfish_left <- NULL
  
  #move fish based on what quadrant they're in  
  #upper left move to lower left
  moving_fish[which(moving_fish$quadrant == 'ul'), 'row'] <- 
    moving_fish[which(moving_fish$quadrant == 'ul'), 'row'] + numrow / 2

  #lower left move to lower right
  moving_fish[which(moving_fish$quadrant == 'll'), 'column'] <- 
    moving_fish[which(moving_fish$quadrant == 'll'), 'column'] + numcol / 2

  #lower right move to upper right
  moving_fish[which(moving_fish$quadrant == 'lr'), 'row'] <- 
    moving_fish[which(moving_fish$quadrant == 'lr'), 'row'] - numrow / 2

  #upper right move to upper left
  moving_fish[which(moving_fish$quadrant == 'ur'), 'column'] <- 
    moving_fish[which(moving_fish$quadrant == 'ur'), 'column'] - numcol / 2


  merged_fish <- merge(fish_area_df[, c('row', 'column', 'nfish_left')],
                   moving_fish[, c('row', 'column', 'sample')], 
                   by = c('row', 'column'), all = TRUE)


  merged_fish$final_fish <- merged_fish$nfish_left + merged_fish$sample


  out <- matrix(0, nrow = numrow, ncol = numcol)
  for(rr in 1:nrow(merged_fish)){
    temp <- merged_fish[rr, ]
    out[temp$row, temp$col] <- temp$final_fish
  }

  return(list(init = fish_area, final = out))
}

  
  

