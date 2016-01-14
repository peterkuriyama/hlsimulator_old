setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(reshape2)
install_github('peterkuriyama/hlsimulator')
library(hlsimulator)

#------------------------------------------------------------------------------------------------------------
#Active Development things

#Things that might affect cpue relationship
#number of locations sampled
#distribution of fish
#number of fish
#percentage of distributed fish (if patchy distribution)

#combining these affects may lead to different results


#Check CPUE with different numbers of fish
#cpues with fish
#Test simulation

#-------------------------------------------------------------------------------------------
#Look at number of locations
locations <- expand.grid(1:10, 1:10)
locations <- data.frame(x = locations[, 1], y = locations[, 2])
names(locations) <- c("", "")

set.seed(1)
locations.list <- sapply(1:100, FUN = function(x) sample(1:100, x, replace = FALSE))
avg.cpue <- vector(length = length(locations.list))

for(ii in 1:length(locations.list)){
# print(ii)
  temp.locs <- vector('list', length = length(locations.list[[ii]]))
  for(jj in 1:length(temp.locs)){
    # temp.locs[[jj]] <- locations[locations.list[[ii]][jj], ]
    temp.locs[[jj]] <- c(locations[locations.list[[ii]][jj], 1], locations[locations.list[[ii]][jj], 2])
  }

  init <- initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'uniform',
                                seed = 300)

  temp <- conduct_survey(fish_area = init, location_list = temp.locs, scope = 1, nhooks = 15, ndrops = 5)

  avg.cpue[ii] <- mean(unlist(temp$cpue))
}


plot(1:100, avg.cpue, type = 'o', pch = 19)
#cpue decreases as more locations are sampled...



avg.cpue <- nfish.vec


for(ii in 1:length(nfish.vec)){
  init <- initialize_population(numrow = 10, numcol = 10, nfish = nfish.vec[ii], distribute = 'uniform',
                                percent = .3, seed = 301)

  temp <- conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                                c(8, 2),
                                                                c(3, 3)), scope = 1, nhooks = 15, ndrops = 5)
  avg.cpue[ii] <- mean(unlist(temp$cpue))

}

plot(nfish.vec, avg.cpue, type = 'o', pch = 19, yli = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish.vec)))


#-------------------------------------------------------------------------------------------
#Look at different numbers of fish
nfish.vec <- seq(100, 10000, by = 100)
avg.cpue <- nfish.vec

for(ii in 1:length(nfish.vec)){
  init <- initialize_population(numrow = 10, numcol = 10, nfish = nfish.vec[ii], distribute = 'uniform',
                                percent = .3, seed = 301)

  temp <- conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                        c(8, 2),
                                                        c(3, 3)), scope = 1, nhooks = 15, ndrops = 5)
  avg.cpue[ii] <- mean(unlist(temp$cpue))

}

plot(nfish.vec, avg.cpue, type = 'o', pch = 19, yli = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish.vec)))

png(file = '/Users/peterkuriyama/Desktop/hl_cpue_check.png')
plot(nfish, avg.cpue, type = 'o', pch = 19, yli = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish)))
dev.off()


conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                      c(8, 2),
                                                      c(3, 3)),
               scope = 1, nhooks = 15, ndrops = 5)














initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'uniform')

initialize_population(numrow = 10, numcol = 10, nfish = 1000, distribute = 'area',
                            area = 'upperright')

initialize_population(numrow = 10, numcol = 10, nfish = 100, distribute = 'patchy',
  percent = .2)


fish_population(fish_area = init, location = c(4, 10), scope = 1, nhooks = 15,
  ndrops = 3)


init <- initialize_population(numrow = 10, numcol = 10, nfish = 5000, distribute = 'patchy',
                              percent = .5, seed = 301)

conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                      c(8, 2),
                                                      c(3, 3)),
               scope = 1, nhooks = 15, ndrops = 5)


fish_mat <- initialize_population(numrow = 10, numcol = 10, nfish = 5000, distribute = 'patchy',
  percent = .5, seed = 301)
fish_population(fish_area = init, location = c(4, 10), scope = 1,
  nhooks = 15, ndrops = 3, process = 'hypergeometric')


#------------------------------------------------------------------------------------------------------------

fish_population(fish_area = init, location = c(4, 10), scope = 1,
  nhooks = 15, ndrops = 3, process = 'hypergeometric')

niters <- 5
samples <- vector('list', length = niters)

for(ii in 1:niters){
  temp <- fish_population(fish_area = init, location = c(4, 10), scope = 1,
    nhooks = 5, ndrops = 3)
  init <- temp[[1]]
  samples[[ii]] <- temp[[2]]
}

samples1 <- ldply(samples)
samples2 <- ldply(samples)


#------------------------------------------------------
#Treat fish and hooks as a predator prey relationship

#1. Set number of hooks in particular cell
#2. Probability of moving into cell with hook
#3. Probability of biting hook
#hard code this in so I can actually see if this method works
# fish.range[2, 1] <- 3


















#------------------------------------------------------
#------------------------------------------------------
#Scraps
# fish.range * move.prob
# #Define movement probabilities
# 1 - (fish.range / nfish.range)


# #Each fish has a probability of catching a hook
# #Probability depends on nuber


# #conusmption by one fish
# check_cons <- function(max.prob = .9, nhooks){
#   zz <- (max.prob * nhooks) / (.6 + nhooks)
#   return(zz)
# }


# tt <- data.frame(hook = 1:75, prob = sapply(1:75,
#   FUN = function(x) check_cons(nhooks = x)))
# tt$prob.many <- 1 - exp(-(tt$prob * 50))

# plot(tt$hook, tt$prob.many, pch = 19, ylim = c(0, 1),
#   main = '70 fish and 1:75 hooks')




# #Highest probability of hook saturation:
#  #Five fish in location and more than five fish in nfish.range
# if(fish.loc >= nhooks & (nfish.range - fish.loc) >= nhooks){
#   hook.prob <- seq(from = max.prob, to = 0, by = -delta.prob)
# }

# #High probability of hook saturation:
# #if more than five fish in location and fewer than nhooks fish surrounding
# if(fish.loc >= nhooks & (nfish.range - fish.loc) < nhooks){
#   hook.prob <- seq(from = max.prob - (2 * delta.prob), to = 0, by = -delta.prob)
# }

# #Medium prob of hook saturation
# #fewer than five fish in specific location, more than five surrounding
# if(fish.loc < nhooks & (nfish.range - fish.loc) >= nhooks){
#  hook.prob <- seq(from = max.prob - (4 * delta.prob), to = 0, by = -delta.prob)
# }

# #Low prob of hook saturation
# #fewer than five in speicifc location, fewer than five surrounding
# if(fish.loc < nhooks & (nfish.range - fish.loc) < nhooks & nfish.range >= nhooks){
#   hook.prob <- seq(from = max.prob - (6 * delta.prob), to = 0, by = -delta.prob)
# }

# #Lowest prob of hook saturation
# #fewer than nhooks fish in entire range
# if(fish.loc < nhooks & (nfish.range - fish.loc) < nhooks & nfish.range < nhooks){
#   hook.prob <- seq(from = max.prob - (8 * delta.prob), to = 0, by = -delta.prob)
# }

# #Subset hook probabilites based on number of hooks
# hook.prob <- hook.prob[1:nhooks]

# #Sample fish
# fish <- rbinom(n = nhooks, size = 1, prob = hook.prob)

# #Subtract sampled fish from number of fish in fish.range matrix
# xx <- melt(fish.range)
# xx[which(xx$value != 0), 'prob'] <- 1 / sum(xx$value != 0) #equal probabilities for now
# # xx$prob <- xx$value / sum(xx$value)

# # use a multinomial sample to find which cells to subtract sampled fish from
# fish.caught <- rmultinom(prob = xx$prob, n = 1, size = sum(fish))
# xx$value <- xx$value - fish.caught

# #update whole matrix
# return(xx)

# #Keep
# # while(sum(fish) > nfish.range){
# #   fish <- rbinom(n = nhooks, size = 1, prob = hook.prob)
# # }





# #Update numbers of fish based on catches
# #Record catches
# prob.catch <- fish.loc


# #check this is working
# #------------------------------------------------------------------------------------------
# # check.this <- vector('list', length = 1000000)
# # for(ii in 1:length(check.this)){
# #   check.this[[ii]] <- rbinom(n = nhooks, size = 1, prob = hook.prob)
# # }

# # ct <- ldply(check.this)
# # colSums(ct) / length(check.this)


# #------------------------------------------------------------------------------------------





# fishArea, location, prob.max, prob.delta
