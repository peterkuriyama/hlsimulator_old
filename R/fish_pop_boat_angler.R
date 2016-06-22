
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

ndrops <- 3
scope <- 2

#---------------start of function

#initialize and format fishing area 
fish_area <- initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
  nfish = 1000, percent = .50)
fish_area_melted <- melt(fish_area) #melt because it is then easier to subset

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

  subset(fish_area_melted, Var1 )

}





#boat samples fish, some boats are better than others
#sampled fish are distributed among anglers, some anglers are better than others








