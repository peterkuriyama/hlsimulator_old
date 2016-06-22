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

ndrops <- 3
scope <- 2

#---------------start of function

#initialize and format fishing area 
fish_area <- initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
  nfish = 1000, percent = .50)
fish_area_melted <- melt(fish_area) #melt because it is then easier to subset

#Define range of rows and columns to fish in 
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

  ##Format y values (lats)
  lats <- data.frame(lat = temp$lat, min_lat = temp$lat - scope, 
    max_lat = temp$lat + scope)
  
  col_range <- unlist(lapply(as.data.frame(t(lats)), function(x) x[2]:x[3]))
  names(col_range) <- NULL
  col_range <- unique(col_range)



}


location %>% group_by(vessel) %>% unique(long)




#boat samples fish, some boats are better than others
#sampled fish are distributed among anglers, some anglers are better than others








