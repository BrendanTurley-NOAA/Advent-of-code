### Advent of Code 2025
### Day 3 
# https://adventofcode.com/2025/day/3



# Part 1 ------------------------------------------------------------------
# The batteries are arranged into banks; each line of digits in your input corresponds to a single bank of batteries. Within each bank, you need to turn on exactly two batteries; the joltage that the bank produces is equal to the number formed by the digits on the batteries you've turned on. For example, if you have a bank like 12345 and you turn on batteries 2 and 4, the bank would produce 24 jolts. (You cannot rearrange batteries.)
# You'll need to find the largest possible joltage each bank can produce
# The total output joltage is the sum of the maximum joltage from each bank, so in this example, the total output joltage is 98 + 89 + 78 + 92 = 357.
# There are many batteries in front of you. Find the maximum joltage possible from each bank; what is the total output joltage?

test <- c('987654321111111',
'811111111111119',
'234234234234278',
'818181911112111')

setwd("C:/Users/brendan.turley/Documents/R_projects/Advent-of-code/2025/inputs")
test <- read.table('aoc_d03.txt',colClasses = "character") |> unlist()


out <- rep(NA,length(test))
for(i in 1:length(test)){
  # i=1
  num_seq <- strsplit(test[i], split = "") |> unlist() |> as.numeric()
  
  num1 <- which.max(num_seq[1:(length(num_seq)-1)])
  num_seq2 <- num_seq[(num1+1):length(num_seq)]
  num2 <- which.max(num_seq2)
  
  out[i] <- paste0(num_seq[num1],num_seq2[num2]) |> as.numeric() 
}
sum(out) #17346




# Part 2 ------------------------------------------------------------------


test <- c('987654321111111',
          '811111111111119',
          '234234234234278',
          '818181911112111')

setwd("C:/Users/brendan.turley/Documents/R_projects/Advent-of-code/2025/inputs")
test <- read.table('aoc_d03.txt',colClasses = "character") |> unlist()

options(scipen=999) 

out <- rep(NA,length(test))
for(i in 1:length(test)){
  # i=1
  num_seq <- strsplit(test[i], split = "") |> unlist() |> as.numeric()
  num1 <- which.max(num_seq[1:(length(num_seq)-11)])
  num_seq2 <- num_seq[num1:length(num_seq)]
  rank_i <- length(num_seq2)-12
  seq_rank <- rank(num_seq2, ties.method = 'first')>rank_i
  # num_seq2[seq_rank]
  out[i] <- paste0(num_seq2[seq_rank],collapse = '') |> as.numeric()
  
}
options(scipen=999) 

sum(out) |> format(scientific = FALSE)
