### Advent of Code 2025
### Day 5 
# https://adventofcode.com/2025/day/5



# Part 1 ------------------------------------------------------------------
# The database operates on ingredient IDs. It consists of a list of fresh ingredient ID ranges, a blank line, and a list of available ingredient IDs.
# The fresh ID ranges are inclusive: the range 3-5 means that ingredient IDs 3, 4, and 5 are all fresh. The ranges can also overlap; an ingredient ID is fresh if it is in any range.


test_id <- c('3-5',
             '10-14',
             '16-20',
             '12-18')

tests <- c('1',
          '5',
          '8',
          '11',
          '17',
          '32') |> as.numeric()

test_id2 <- strsplit(test_id,'-') |> unlist() |> as.numeric()
test_ids <- matrix(test_id2, length(test_id), 2, byrow = T)


options(scipen=999) 
setwd("C:/Users/brendan.turley/Documents/R_projects/Advent-of-code/2025/inputs")
test <- read.table('aoc_d05.txt',colClasses = "character") |> unlist()
splt <- as.numeric(test)

test_id <- test[is.na(splt)]
test_id2 <- strsplit(test_id,'-') |> unlist() |> as.numeric()
test_ids <- matrix(test_id2, length(test_id), 2, byrow = T)
tests <- test[!is.na(splt)] |> as.numeric()


n <- 0
for(i in 1:length(test)){
  lhs <- tests[i] < test_ids[,1]
  rhs <- tests[i] > test_ids[,2]
  
  ans <- which(rowSums(cbind(lhs,rhs))==0)
  
  if(!identical(ans, integer(0))){
    n <- n + 1
  }
}
n # my right answer: 558
