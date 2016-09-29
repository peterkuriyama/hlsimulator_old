
#Need to start documentation with examples of this
#Note vessels cannot fish in the same locations in a year
#Can simulate declining population trend by decreasing the number of fish distributed throughout
#the matrix

#fishing is done in a for loop that is sequential , that is one vessel fishes
#then another fishes, so that


#Function to fish an area 
#
#Number of Anglers
#On number of boats

#---------------arguments
nangler <- 3 #number of anglers

#data frame of fishing locations specifying row, column, and vessel
location <- data.frame('long' = c(3, 2, 8), 
                       'lat' = c(3, 5, 8), 
                       'vessel' = c(1, 1, 2) )

#define hotspots
hotspots <- data.frame('long' = c(3, 2), 
                       'lat' = c(3, 5))

ndrops <- 3
scope <- 2

#---------------start of function

#initialize and format fishing area 
fish_area <- initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
  nfish = 1000, percent = .50)
fish_area_melted <- melt(fish_area) #melt because it is then easier to subset
fish_area_melted$ind <- paste(fish_area_melted$Var1, fish_area_melted$Var2)

#------------------------------------------------------
#Define range of rows and columns to fish in 
row_ranges <- vector('list', length = length(unique(location$vessel)))
col_ranges <- vector('list', length = length(unique(location$vessel)))

for(ii in 1:length(unique(location$vessel))){
  temp <- subset(location, vessel == ii)
  
  ##Format x values (longs)
  longs <- data.frame(long = temp$long, min_long = temp$long - scope, 
    max_long = temp$long + scope)
  
  row_range <- unlist(lapply(as.data.frame(t(longs)), function(x) x[2]:x[3]))
    #transpose then use lapply to get sequences from min_range to max_range
    #then unlist so that everything is a vector
  names(row_range) <- NULL
  row_range <- unique(row_range)
  row_range <- row_range[row_range %in% 1:nrow(fish_area)]


  ##Format y values (lats)
  lats <- data.frame(lat = temp$lat, min_lat = temp$lat - scope, 
    max_lat = temp$lat + scope)
  
  col_range <- unlist(lapply(as.data.frame(t(lats)), function(x) x[2]:x[3]))
  names(col_range) <- NULL
  col_range <- unique(col_range)
  col_range <- col_range[col_range %in% 1:ncol(fish_area)]

  #Assign to slot in list
  row_ranges[[ii]] <- row_range
  col_ranges[[ii]] <- col_range
}

#------------------------------------------------------
#Fish locations in a for loop
for(ii in 1:length(row_ranges)){
  row_range <- row_ranges[[ii]]
  col_range <- col_ranges[[ii]]

  locs <- subset(location, vessel == ii)
  locs$ind <- paste(locs$long, locs$lat)
  
  #Extract fish fishing the range, and extract number of fish in specified fishing locations
  fish_range_melted <- subset(fish_area_melted, Var1 %in% 
                              row_range & Var2 %in% col_range)
  #create column of var1 and var2 pasted. Makes subsetting by row based on
  #location data frame easier
  fish_range_melted$ind <- paste(fish_range_melted$Var1, fish_range_melted$Var2)
  
  fish_location_melted <- fish_area_melted[which(locs$ind == fish_area_melted$ind), ]

  #define zero index, locations where 
  zero_index <- which(fish_range_melted$ind %in% locs$ind)
  
  fish_range <- matrix(fish_range_melted$value, nrow = length(row_range),
    ncol = length(col_range))

  fish_in_loc <- fish_location_melted$value
  nfish_outside <- sum(fish_range) - sum(fish_in_loc)
  
  #---------------------
  #Move fish to specified location
  probs <- fish_range / nfish_outside #probability of 
  fish_df <- melt(fish_range)
  fish_df$ind <- paste(fish_df$Var1, fish_df$Var2)
  
  #Probability of moving
  fish_df$prob <- melt(probs)$value

  if(nfish_outside == 0){
    fish_df$prob <- 0
  }

  #Stop fishing if there are no fish (return 0s for samples)
  if(sum(fish_df$value) == 0){
    return(list(updated_area = fish_area, samples = rep(0, ndrops)))
  }

  fish_df[zero_index, 'prob'] <- 0

  #calculate number of moving fish with binomial distribution
  fish_df$moving <- apply(fish_df, MAR = 1, FUN = function(x) rbinom(n = 1, 
    size = as.numeric(x['value']), prob = as.numeric(x['prob'])))

  #now update fish
  fish_df$moved <- fish_df$value - fish_df$moving #column indicates nmber of fish remaining
  #after movement

  #assign fish to hotspot locations
  names(fish_df)[1] <- 'lat'
  names(fish_df)[2] <- 'long'
  
  #sample fish that moved and then assign to hotspot location
  set.seed(seed)
  draws <- base::sample(sum(fish_df$moving), size = nrow(hotspots) - 1, replace = FALSE)
  draws <- c(draws, sum(fish_df$moving) - sum(draws))

  #assign the values to the hotspots
  fish_df[which(fish_df$unq %in% hotspots$unq), 'moved'] <- fish_df[which(fish_df$unq %in% hotspots$unq), 
                                                                    'moved'] + draws
  
  return()                                                                    

}





#boat samples fish, some boats are better than others
#sampled fish are distributed among anglers, some anglers are better than others








w