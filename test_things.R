#habitat specific movement, when patchy certain attraction to 
#certain cells 
setwd("/Users/peterkuriyama/School/Research/hlsimulator")

library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
# library(purrr)

library(devtools)
install_github('peterkuriyama/hlsimulator@master')

install('../hlsimulator')


# load_all('../hlsimulator')


library(hlsimulator)

#update documentation
move_fish_hs(fish_area = initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
                              nfish = 1000, percent = .50), 
                  hotspots = data.frame('row' = c(3, 2), 'column' = c(3, 5)),
                  max_prob = 0.1)
     

#--------------------------------------------------------------------------------------------
##TO DO
#Number of vessels fishing...?
#Number of anglers fishing...?

#Add in new probability distribution


##Put in no movement function
## Depends on how much of population we sample
##Recruitment functions?

#Things that might affect cpue relationship
#number of locations sampled
#distribution of fish
#number of fish
#percentage of distributed fish (if patchy distribution)

#-------------------------------------------------------------------------------------------
#Get back into this
#start with lowest level function call, 

tt <- initialize_population(distribute = 'patchy', numrow = 10, numcol = 10, seed = 5,
  nfish = 1000, percent = .50)

#Input locations for each vessel as a data.frame
#Location input with columns for vessel, row, column

location <- data.frame(vessel = c(1, 1, 2), x = c(3, 3, 8), y = c(3, 5, 8))

set.seed(1)
fish_population(fish_area = tt, location = data.frame(vessel = c(1, 1, 2), x = c(3, 3, 8), 
  y = c(3, 5, 8)),
  scope = 2, nhooks = 5, ndrops = 3, process = "equal_prob")


#-------------------------------------------------------------------------------------------
#Ideal CPUE Figure
Linf <- 1
k <- .2
t0 <- 6

x <- 1:40
y <- Linf * (1 - exp(-k * (x - t0)))


pdf(width = 7, height = 7, file = 'figs/ideal_cpue.pdf')
par(mar = c(5.1, 5.1, 1, 1.5))
plot(x, y, type = 'l', axes = F, ann = F, lwd = 4, ylim = c(-1.5, 1),
  xaxs = 'i', yaxs = 'i')  
axis(side = 1, at = c(0, 10, 20, 30, 40), labels = c(0, '1000', '2000', '3000', '4000'),
  cex.axis = 1.5)
axis(side = 2, at = c(-1.5, -1, -.5, 0, .5, 1), 
  labels = c(0, .2, .4, .6, .8, 1), las = 2, cex.axis = 1.5)
mtext(side = 1, "Number of Fish", line = 3.5, cex = 2)
mtext(side = 2, 'CPUE', line = 3.2, cex = 2)
dev.off()

#-------------------------------------------------------------------------------------------
#Plot actual CPUE stuff from data
load(paste0('data/',list.files('data'))[1])
load(paste0('data/',list.files('data'))[2])

# #Massage data so that columns are aligned
dat <- Grand.2014.JF

boc <- subset(dat, ComName == 'Bocaccio')
ver <- subset(dat, ComName == 'Vermilion Rockfish')

boc <- boc[, c('Year', 'SiteName' ,'VesName', 'Length.cm', 'Weight.kg')]

png(width = 7, height = 7, res = 200, units = 'in',
  file = 'figs/boc_lengths.png')
ggplot(boc, aes(x = Length.cm)) + geom_histogram() + theme_bw() + 
  facet_wrap(~ Year)
dev.off()

hist(boc$Length.cm)

dat %>% group_by(Year, SiteName) %>% summarise(nfish = sum(SurvFish),
  nhooks = length(SurvFish), cpue = nfish / nhooks) %>% 
  as.data.frame -> site_cpue

# ggplot(subset(site_cpue, SiteName %in% high_sites), aes(x = Year, y = cpue, group = SiteName)) + 
#   geom_line()

site_cpue_plot <- ggplot(site_cpue, aes(x = Year, y = cpue, group = SiteName)) + 
        geom_line() + facet_wrap(~ SiteName) + 
        theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank())
site_cpue <- site_cpue[-33, ]

ggplot(site_cpue, )

# hist(site_cpue$nfish, breaks = 30)

pdf(width = 10, height = 9, 
  file = 'figs/site_cpue.pdf')
print(site_cpue_plot)
dev.off()

#-------------------------------------------------------------------------------------------
#One Cell
#Evaluate survey in one cell multiple times
# run_and_plot(numrow = 1, 
#                   numcol = 1, nfish = 1000,
#                   seed = 301, distribute = 'uniform',
#                   nyears = 1, 
#                   location_list = list(c(1, 1)),
#                   move_func = 'move_fish_none',
#                   nhooks = 15, ndrops = 5, scope = 0,
#                   process = 'equal_prob',
#                   p0 = .4)

#Fish in one cell

to_loop_over <- expand.grid(1:400, seq(.1, 1, by = .1))
names(to_loop_over) <- c('nfish', 'p0')
one_cell_results <- vector('list', length = nrow(to_loop_over))

for(ii in 1:nrow(to_loop_over)){
  if(ii %% 100 == 0) print(ii)
  temp <- run_and_plot(numrow = 1, numcol = 1, 
                       nfish = to_loop_over[ii, 1],
                       seed = 301, distribute = 'uniform',
                       nyears = 1, 
                       location_list = list(c(1, 1)),
                       move_func = 'move_fish_none',
                       nhooks = 15, ndrops = 5, scope = 0,
                       process = 'equal_prob',
                       p0 = to_loop_over[ii, 2])
  temp$cpue$nfish <- to_loop_over[ii, 1]
  temp$cpue$p0 <- to_loop_over[ii, 2]

  one_cell_results[[ii]] <- temp$cpue
}

one_cell_results <- ldply(one_cell_results)
mean_cpue <- one_cell_results %>% group_by(nfish, p0) %>% 
    summarise(cpue = mean(value)) %>% as.data.frame

unique_p0 <- unique(mean_cpue$p0)
grays <- gray.colors(length(unique_p0), start = .4, end = 1)

#----------------------------------------------------------------
#Plot only one p0 value
four <- subset(mean_cpue, p0 == .4)

pdf(width = 7, height = 7, file = 'figs/one_cell_p0_.4.pdf')
plot(four$nfish, four$cpue, type = 'o', ylim = c(0, 1.05), pch = 19,
  xaxs = 'i', yaxs = 'i', xpd = FALSE, 
  xlab = 'Population Size (# fish)', ylab = 'CPUE', xlim = c(0, 200),
  cex.axis = 1.5)
dev.off()

#----------------------------------------------------------------
#Plot all p0 values

pdf(width = 7, height = 7, file = 'figs/one_cell_all_p0.pdf')
plot(mean_cpue$nfish, mean_cpue$p0, type = 'n', ylim = c(0, 1),
  xlab = 'Population Size (# fish)', ylab = 'CPUE', 
  xlim = c(0, 200), cex.axis = 1.5)

for(ii in 1:length(unique_p0)){
  temp <- subset(mean_cpue, p0 == unique_p0[ii])
  lines(temp$nfish, temp$cpue, col = grays[ii],
    lwd = 2)
}

legend('bottomright', legend = rev(unique_p0), lwd = 2, 
  col = rev(grays), bty = 'n', cex = 1.4)
dev.off()

plot(subset(mean_cpue, p0 == 0.1)$nfish, 
  subset(mean_cpue, p0 == 0.1)$cpue, type = 'o', pch = 19)


ggplot(mean_cpue, aes(x = nfish, y = cpue, group = p0)) + 
  geom_point(aes(colour = p0))


fish_one_cell <- seq(0, 200, by = 1)
one_cell_results <- vector('list', length = length(fish_one_cell))

for(ii in 1:length(fish_one_cell)){
  temp <- run_and_plot(numrow = 1, 
                  numcol = 1, nfish = fish_one_cell[ii],
                  seed = 301, distribute = 'uniform',
                  nyears = 1, 
                  location_list = list(c(1, 1)),
                  move_func = 'move_fish_none',
                  nhooks = 15, ndrops = 5, scope = 0,
                  process = 'equal_prob',
                  p0 = .4)
  one_cell_results[[ii]] <- temp$cpue 
}

names(one_cell_results) <- fish_one_cell
ocr <- ldply(one_cell_results)
names(ocr)[1] <- 'nfish'
ocr$nfish <- as.integer(ocr$nfish)

ocr_cpue <- ocr %>% group_by(nfish) %>% dplyr::summarise(cpue = mean(value))

plot(ocr_cpue$nfish, ocr_cpue$cpue, type = 'o', pch = 19)

plot(ocr$nfish, ocr$value)

ocr %>% group_by(nfish) %>% summarise(cpue = mean(value))

ggplot(ocr, aes(x = nfish, y = value)) + geom_point()



#-------------------------------------------------------------------------------------------
location_list1 <- list(c(2, 2), 
                       c(3, 1),
                       c(3, 4),
                       c(3, 7),
                       c(4, 9),
                       c(2, 6),
                       c(7, 1),
                       c(9, 3),
                       c(8, 5),
                       c(8, 7),
                       c(9, 10),
                       c(10, 6))



run1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 10000,
                   distribute = 'patchy',
                   seed = 301,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 5, scope = 0, print_text = TRUE, 
                   process = 'equal_prob', p0 = .4, percent = .5)
inits <- subset(run1$init_nfish, year == 1)
inits <- inits[order(inits$value, decreasing = TRUE), ]

best_sites <- sample(which(inits$value > 0), size = 12)
inits[best_sites, ]

best_locations <- list(c(4, 5), 
                       c(5, 8),
                       c(3, 2),
                       c(2, 5),
                       c(5, 2),
                       c(8, 7),
                       c(9, 6),
                       c(3, 3),
                       c(5, 10),
                       c(8, 1),
                       c(5, 9),
                       c(10, 4))

# half_good_locations <- list()
half_good_locations <- list(c(4, 5), 
                            c(7, 2),
                            c(3, 2),
                            c(10, 8),
                            c(5, 2),
                            c(8, 8),
                            c(9, 6),
                            c(3, 3),
                            c(6, 4),
                            c(1, 6),
                            c(9, 10),
                            c(10, 4))

#Modify number of fish
pdf(width = 7, height = 7, file = 'figs/nomovement_best_locs_15000.pdf')
run15_best <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 15000,
                   distribute = 'patchy',
                   seed = 301,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = best_locations, 
                   move_func = 'move_fish_none', move_prob = .7,
                   nhooks = 15, ndrops = 5, scope = 0, print_text = TRUE, 
                   process = 'equal_prob', p0 = .4, percent = .5)
dev.off()

pdf(width = 7, height = 7, file = 'figs/cwmovement_best_locs_15000.pdf')
run15_best <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 15000,
                   distribute = 'patchy',
                   seed = 301,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = best_locations, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 5, scope = 0, print_text = TRUE, 
                   process = 'equal_prob', p0 = .4, percent = .5)
dev.off()

pdf(width = 7, height = 7, file = 'figs/cwmovement_half_best_locs_15000.pdf')
run15_half <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 15000,
                   distribute = 'patchy',
                   seed = 301,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = half_good_locations, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 5, scope = 0, print_text = TRUE, 
                   process = 'equal_prob', p0 = .4, percent = .5)
dev.off()

pdf(width = 7, height = 7, file = 'figs/cwmovement_best_locs_10.pdf')
run15_half <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 10000,
                   distribute = 'patchy',
                   seed = 301,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = best_locations, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 5, scope = 0, print_text = TRUE, 
                   process = 'equal_prob', p0 = .4, percent = .5)
dev.off()

pdf(width = 7, height = 7, file = 'figs/cwmovement_half_best_locs_10.pdf')
run15_half <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 10000,
                   distribute = 'patchy',
                   seed = 301,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = half_good_locations, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 5, scope = 0, print_text = TRUE, 
                   process = 'equal_prob', p0 = .4, percent = .5)
dev.off()





inits <- subset(run5$init_nfish, year == 1)



subset(run5$init_nfish, year == 1)[order(subset(run5$init_nfish, year == 1)$value), ]


matrix(pp2$init_nfish$value, nrow = 10, ncol = 10, byrow = T)

pes <- 3 #potential exploitable stock size
p0 <- .5 
f0 <- .5
f <- p0 / 5

pes <- 5

nn <- pes / p0

function(pes, p0 = .5, f0 = .5){
  nn <- pes / p0
}


#Define probabilities, with same hook

fish0 <- f0 ^ nn
fish1 <- 5 * (f + f0) ^ nn - f0 ^ nn
fish2 <- 10 * ((2 * f + f0) ^ nn - 2 * (f + f0) ^ nn + f0 ^ nn)

fish31 <- (3 * f + f0) ^ nn
fish32 <- -3 * (2 * f + f0) ^ nn
fish33 <- 3 * (f + f0) ^ nn
fish34 <- -f0 ^ nn
fish3 <- 10 * (fish31 + fish32 + fish33 + fish34)

fish41 <- (4 * f + f0) ^ nn
fish42 <- -4 * (3 * f + f0) ^ nn
fish43 <- 6 * (2 * f + f0) ^ nn
fish44 <- -4 * (f + f0) ^ nn
fish45 <- f0 ^ nn
fish4 <- 5 * (fish41 + fish42 + fish43 + fish44 + fish45)

fish5 <- 1 - (fish0 + fish1 + fish2 + fish3 + fish4)


data.frame(fish0, fish1, fish2, fish3, fish4, fish5)


#Same hook probabilities
ff <- .2
f0 <- 1






#-------------------------------------------------------------------------------------------
#Define Probabilities of 
#Specify hook probabilities for nhooks
prob_capture <- .2 #probability that fish detect gear

#probability of catching no fish
zero_prob <- 1
#define hook probabilities, prob of not catching anything, 
hook_probs <- c(.2, .2, .2, .2, .2) #prob of no fish, hook 1, hook2 etc

zero_prob * hook_probs


nfish <- 1

#check hook probabilities
# if(zero_prob + sum(hook_probs) != 1) stop('zero_prob and hook_probs must sum to 1')

#calculate probabilities of hooking fish given these parameters
hook_combos <- expand.grid(rep(list(0:1), 5)) #combination of hooks
hook_combos$row_sum <- rowSums(hook_combos)

#Specify probabilities of each hook-catch configuration
hook_combos$prob <- 1

nhooks <- length(hook_probs)

#-------------------------------------------------------------------------------------------
#only one hook
ones <- hook_combos[which(hook_combos$row_sum == 1), ]

for(ll in 1:nrow(ones)){
  temp <- ones[ll, ]
  f_i <- hook_probs[which(temp[1, 1:nhooks] == 1)]
  ones[ll, 'prob'] <- (f_i + zero_prob) ^ nfish - zero_prob ^ nfish
}

#-------------------------------------------------------------------------------------------
#Two hooks
twos <- hook_combos[which(hook_combos$row_sum == 2), ]

for(ll in 1:nrow(twos)){
  temp <- twos[ll, ]
  yes <- which(temp[1, 1:nhooks] == 1)

  f_1 <- hook_probs[yes[1]]
  f_2 <- hook_probs[yes[2]]

  twos[ll, 'prob'] <- (f_1 + f_2 + zero_prob) ^ nfish - (f_1 + zero_prob) ^ nfish -
    (f_2 + zero_prob) ^ nfish - zero_prob ^ nfish
}

#-------------------------------------------------------------------------------------------
#Three Hooks
threes <- hook_combos[which(hook_combos$row_sum == 3), ]

for(ll in 1:nrow(threes)){
  temp <- which(threes[ll, 1:nhooks] == 1)  


}












sapply(ones, function(x) which(x[1:nhooks] == 1))




(hook_probs[which(ones[1, 1:5] == 1)] + zero_prob) ^ nfish - zero_prob ^ nfish


dd <- apply(ones, MAR = 1, 
  FUN = function(x) (hook_probs[which(ones[x, 1:5] == 1)] + zero_prob) ^ nfish - zero_prob ^ nfish)



apply(hook_combos, MAR = 1, FUN = function(x) paste(which(x[1:5] == 1)))







for(ii in 1:nhooks){


  print(ii)
}


ones$prob <- 




#Calculate probabilities based on 


hook_combos %>% group_by(row_sum) %>% mutate(prob = function(x))


zero_index <- which(hook_combos$row_sum ==  0)
hook_combos

hook_combos[which(hook_combos$row_sum ==  0), 'prob'] <- 


which(hook_combos$row_sum ==  1)

hook_combos %>% filter(row_sum == 0) %>% mutate(prob = zero_prob ^ nfish)






sum


#-------------------------------------------------------------------------------------------
#NO recruitment and strong local depletion effects with year after year sampling

#-------------------------------------------------------------------------------------------
##Compare the effect of fish movement on fixed location sampling
#Specify 10 locations
location_list1 <- list(c(2, 2), 
                       c(3, 1),
                       c(3, 4),
                       c(3, 7),
                       c(4, 9),
                       c(2, 6),
                       c(7, 1),
                       c(9, 3),
                       c(8, 5),
                       c(8, 7),
                       c(9, 10),
                       c(10, 6))

pdf(width = 13, height = 5.65, 
  file = 'figs/fixed_location_different_movement.pdf')

par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_left', max_prob = .7, min_prob = 0,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()


#-------------------------------------------------------------------------------------------
#random locations and various movement patterns

pdf(width = 13, height = 5.65, 
  file = 'figs/random_location_different_movement.pdf')

par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_left', max_prob = .7, min_prob = 0,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()

#-------------------------------------------------------------------------------------------
#Fixed locations and moving fish
pdf(width = 13, height = 5.65, 
  file = 'figs/fixed_location_clockwise_movement_different_distributions.pdf')
par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'patchy',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'area',
                   area = 'upperright',
                   seed = 302,
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7, 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()


#-------------------------------------------------------------------------------------------
#Random locations and moving fish
pdf(width = 13, height = 5.65, 
  file = 'figs/random_location_clockwise_movement_different_distributions.pdf')
par(mfcol = c(1, 3), oma = c(2, 2, 0, 0))

#no fish movement and uniform initial distribution
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#clockwise fish movement
pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'patchy',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7,
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)

#left fish movement
pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'area',
                   area = 'upperright',
                   seed = 302,
                   nyears = 15, 
                   random_locations = TRUE,
                   location_list = location_list1, 
                   nlocs = 12, 
                   move_func = 'move_fish_cw', move_prob = .7, 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE)
dev.off()


#-------------------------------------------------------------------------------------------
par(mfcol = c(1, 3))
pp1 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 1000,
                   distribute = 'uniform',
                   area = 'upperleft', 
                   seed = 302,
                   nyears = 30, 
                   random_locations = TRUE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, print_text = TRUE,
                   xlim_s = c(0, 10000))

pp2 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'uniform',
                   area = 'upperleft', 
                   seed = 302, 
                   nyears = 30, 
                   random_locations = TRUE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0, 
                   xlim_s = c(0, 10000))

pp3 <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 10000,
                   distribute = 'uniform',
                   area = 'upperleft', 
                   seed = 302, 
                   nyears = 30, 
                   random_locations = TRUE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0)







#high level wrapper to run and plot
pp <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 5000,
                   distribute = 'area',
                   area = 'upperleft', 
                   seed = 302, 
                   nyears = 15, 
                   random_locations = FALSE,
                   location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 50, 
                   move_func = 'move_fish_none', 
                   nhooks = 15, ndrops = 3, scope = 0)

pp <- run_and_plot(numrow = 10, 
                   numcol = 10, 
                   nfish = 1000,
                   distribute = 'uniform', 
                   seed = 302, 
                   nyears = 15, 
                   random_locations = TRUE,
                   # location_list = list(c(2, 2), c(8, 8)), 
                   nlocs = 10, 
                   move_func = 'move_fish_cw', 
                   move_prob = .8, 
                   nhooks = 15, ndrops = 3, scope = 0)




xx301[[4]] == xx300.2[[4]]


xx300.1$end_nfish == xx300.2$end_nfish
xx301$end_nfish == xx300.2$end_nfish

run_and_plot(numrow = 10, numcol = 10, nfish = 10000,
  distribute = 'uniform', seed = 300, nyears = 15, random_locations = TRUE, 
  nlocs = 10, move_func = 'move_fish_left', max_prob = .2, min_prob = .01)















#-------------------------------------------------------------------------------------------
#develop function to move fish around ontogenetically or 
xx <- survey_over_years(numrow = 10, numcol = 10, nfish = 100000, 
  distribute = 'uniform',
  seed = 300, nyears = 15, location_list, 
  random_locations = TRUE, nlocs = 100, move_func = move_fish_cw, move_prob = .8,
  scope = 1, process = 'equal_prob', ndrops = 3, nhooks = 5)



thing <- parse_master_list(xx)

nfish_cpue <- merge(thing$end_nfish %>% group_by(year) %>% summarise(nfish = sum(value)),
      cpue %>% group_by(year) %>% summarise(cpue = mean(value)) %>% as.data.frame,
      by = 'year')


plot(nfish_cpue$nfish, nfish_cpue$cpue, ylim = c(0, 1), pch = 19,
  xaxs = 'i', yaxs = 'i', xlim = c(0, max(pretty(nfish_cpue$nfish))))


ggplot(cpue, aes(x = variable, y = value, colour = year, group = year)) + geom_line() + 
  facet_wrap(~ location) + scale_color_gradientn(colors = gray.colors(length(cpue$year)))








#separate elements from each iteration
#everything a list for now
fish_list <- lapply(xx, FUN = function(x) x$sampled_area)
fish_melt <- melt(fish_list)
names(fish_melt) <- c('row', 'column', 'nfish', 'year')
fish_melt$density <- fish_melt$nfish / 100


ggplot(fish_melt, aes(x = row, y = column)) + geom_raster(aes(fill = density)) + 
  facet_wrap(~ year)

ggplot(fish_melt)

melt(fish_list)
melt(fish_list[[1]])

ggplot(fish_list[[1]] aes(x))

cpue_list <- lapply(xx, FUN = function(x) x$cpue)

#extract number of fish in each year
  nfish <- sapply(xx, FUN = function(x) {
    sum(unlist(x$sampled_area))
  })
  cpue <- sapply(xx, FUN = function(x) {
    mean(unlist(x$cpue[, 2:4]))
  })

plot(nfish, cpue, pch = 19, type = 'o')

to.plot <- lapply(xx, FUN = function(x) x$cpue)
sapply(to.plot, FUN = function(x) mean(unlist(x[, 2:4])))


survey_over_years(nfish = 10000, distribute = 'uniform', seed = 300, nyears = 15,
  random_locations = TRUE, nlocs = 10)


#-------------------------------------------------------------------------------------------
#Look at number of locations

cpues <- explore_nlocs_cpue(numrow = 10, numcol = 10, nfish = 2000, seed = 200, 
  numlocs = 10, distribute = 'patchy',
  percent = .5, scope = 0)

to.plot <- melt(cpues)
names(to.plot)[1] <- 'nlocs'

xx <- sapply(to.plot$location, FUN = function(x) eval(parse(text = x)))

to.plot$x <- as.vector(xx[1, ])
to.plot$y <- as.vector(xx[2, ])


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


plot(nfish.vec, avg.cpue, type = 'o', pch = 19, ylim = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish.vec)))

png(file = '/Users/peterkuriyama/Desktop/hl_cpue_check.png')
plot(nfish, avg.cpue, type = 'o', pch = 19, yli = c(0, 1), xaxs = 'i',
     yaxs = 'i', xlim = c(0, max(nfish)))
dev.off()


conduct_survey(fish_area = init, location_list = list(c(4, 10),
                                                      c(8, 2),
                                                      c(3, 3)),
               scope = 1, nhooks = 15, ndrops = 5)



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

#-------------------------------------------------------------------------------------------
#Move fish cw
# fish_area1 <- initialize_population(distribute = 'area', area = 'upperright', numrow = 10, numcol = 10,
#   nfish = 1000)

# tt <- fish_area1
# ttp <- vector('list', length = 8)
# for(ll in 1:8){
#   temp <- move_fish_cw(fish_area = tt, move_prob = .8)
#   tt <- temp$final
#   ttp[[ll]] <- tt
# }

# ttp <- melt(ttp)

# ggplot(ttp, aes(x = Var1, y = Var2)) + geom_raster(aes(fill = value)) + 
#   theme_bw() + facet_wrap(~ L1)

# ggplot(ttp[1], )

# move_fish_cw(fish_area = fish_area1, move_prob = .8)





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
