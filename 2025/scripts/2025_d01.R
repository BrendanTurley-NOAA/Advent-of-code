### Advent of Code 2025
### Day 1 
# https://adventofcode.com/2025/day/1


# Part 1 ------------------------------------------------------------------
# The actual password is the number of times the dial is left pointing at 0 after any rotation in the sequence.

init <- 50
test <- c('L68',
          'L30',
          'R48',
          'L5',
          'R60',
          'L55',
          'L1',
          'L99',
          'R14',
          'L82')

setwd("C:/Users/brendan.turley/Documents/R_projects/Advent-of-code/2025/inputs")
test <- read.table('aoc_d01.txt') |> unlist()
dial <- c(init,
          rep(NA,length(test)))

# i <- 1


for(i in 1:length(test)){
  
  input <- test[i]
  
  l_r <- substr(input ,1 ,1)
  dis <- substr(input ,2 ,10) |> as.numeric()
  
  
  if(l_r=='L'){
    out <- dial[i] - dis
    while(out < 0){
      out <- 99 + out + 1
    }
    out
  }
  
  if(l_r=='R'){
    out <- dial[i] + dis
    while(out > 99){
      out <- out - 99 - 1
    }
    out
  }
  
  dial[i+1] <- out
}
length(which(dial==0)) ### 1100
### correct



# Part 2 ------------------------------------------------------------------
# You remember from the training seminar that "method 0x434C49434B" means you're actually supposed to count the number of times any click causes the dial to point at 0, regardless of whether it happens during a rotation or at the end of one

init <- 50
test <- c('L68',
          'L30',
          'R48',
          'L5',
          'R60',
          'L55',
          'L1',
          'L99',
          'R14',
          'L82')
test <- c('L50',
          'L101')

setwd("C:/Users/brendan.turley/Desktop")
test <- read.table('aoc_d01.txt') |> unlist()

dial <- c(init,
          rep(NA,length(test)))
clicks <- rep(NA,length(test))

for(i in 1:length(test)){
  n <- 0
  
  input <- test[i]
  
  l_r <- substr(input ,1 ,1)
  dis <- substr(input ,2 ,10) |> as.numeric()
  
  
  if(l_r=='L'){
    out <- dial[i] - dis
    while(out < 0){
      out <- 100 + out
      n <- n + 1
    }
    out
  }
  
  if(l_r=='R'){
    out <- dial[i] + dis
    while(out > 99){
      out <- out - 100
      n <- n + 1
    }
    out
  }
  
  n <- ifelse(dial[i]==0 & n!=0, n-1, n)
  n <- ifelse(out==0 & n!=0, n-1, n)
  
  dial[i+1] <- out
  clicks[i] <- n
}
length(which(dial==0)) ### 1100
# length(which(clicks==1))
sum(clicks,na.rm=T) ### 5061

length(which(dial==0)) + sum(clicks,na.rm=T) #6161

cbind(c(NA,test),dial,c(NA,clicks))

