### Advent of Code 2025
### Day 4 
# https://adventofcode.com/2025/day/4



# Part 1 ------------------------------------------------------------------
# The forklifts can only access a roll of paper if there are fewer than four rolls of paper in the eight adjacent positions.
# Consider your complete diagram of the paper roll locations. How many rolls of paper can be accessed by a forklift?

test <- c('..@@.@@@@.',
          '@@@.@.@.@@',
          '@@@@@.@.@@',
          '@.@@@@..@.',
          '@@.@@@@.@@',
          '.@@@@@@@.@',
          '.@.@.@.@@@',
          '@.@@@.@@@@',
          '.@@@@@@@@.',
          '@.@.@@@.@.')
test_c <- strsplit(test,'') |> unlist()
test_c0 <- gsub('@',1,test_c)
test_f <- gsub('[.]',0,test_c0) |> as.numeric() |> matrix(10,10, byrow = T)

setwd("C:/Users/brendan.turley/Documents/R_projects/Advent-of-code/2025/inputs")
test <- read.table('aoc_d04.txt',colClasses = "character") |> unlist()

test_c <- strsplit(test,'') |> unlist()
test_c0 <- gsub('@',1,test_c)
test_f <- gsub('[.]',0,test_c0) |> as.numeric() |> matrix(length(test), nchar(test[1]), byrow = T)

ind_rolls <- which(test_f==1,arr.ind = T)
ind_rolls <- cbind(ind_rolls, ind_rolls[,1]-1)
ind_rolls <- cbind(ind_rolls, ind_rolls[,1]+1)
ind_rolls <- cbind(ind_rolls, ind_rolls[,2]-1)
ind_rolls <- cbind(ind_rolls, ind_rolls[,2]+1)
colnames(ind_rolls)[3:6] <- c('trow','brow','lcol','rcol')
# ind_rolls[which(ind_rolls<1 | ind_rolls>10)] <- NA
ind_rolls[which(ind_rolls<1)] <- 1
ind_rolls[which(ind_rolls>nrow(test_f))] <- nrow(test_f)

out <- rep(NA, nrow(ind_rolls))
for(i in 1:nrow(ind_rolls)){
  # i=10
  ind <- ind_rolls[i,]
  
  adj_r <- test_f[ind[3]:ind[4],ind[5]:ind[6]] |> sum()
  out[i] <- ifelse(adj_r<5,1,0)
  
}
sum(out) # my (correct) answer: 1569






rolls <- matrix(NA, length(test), nchar(test[1]))

for(i in 1:length(test)){
  # i = 1
  ti <- strsplit(test[i],'') |> unlist()
  ind <- which(ti=='@')
  
  for(j in 1:length(ind)){
    # j = 1
    
    if((ind[j]-2)>=1 | (ind[j]+2)<=length(ti)){
      adj_r <- grepl('@', ti[(ind[j]-2):(ind[j]+2)]) |> sum()
      rolls[i,ind[j]] <- ifelse(adj_r<5, 1, 0)
    }
  }
 
}


# general
ind[j]-2

#edge case
if((ind[j]-2)<1 | (ind[j]-1)<1){
  
}