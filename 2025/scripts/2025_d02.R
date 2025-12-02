### Advent of Code 2025
### Day 2 
# https://adventofcode.com/2025/day/2


# Part 1 ------------------------------------------------------------------
# find the invalid IDs by looking for any ID which is made only of some sequence of digits repeated twice. So, 55 (5 twice), 6464 (64 twice), and 123123 (123 twice) would all be invalid IDs.
# None of the numbers have leading zeroes; 0101 isn't an ID at all. (101 is a valid ID that you would ignore.)
# Your job is to find all of the invalid IDs that appear in the given ranges

test <- c('11-22','95-115','998-1012','1188511880-1188511890','222220-222224',
          '1698522-1698528','446443-446449','38593856-38593862','565653-565659',
          '824824821-824824827','2121212118-2121212124')
# Test answer: Adding up all the invalid IDs in this example produces 1227775554.
setwd("C:/Users/brendan.turley/Desktop")
test <- read.csv('aoc_d02.txt',header=F) |> unlist() |> strsplit(',') |> unlist()

out_i <- list()
for(i in 1:length(test)){
  # i=2
  inp_i <- test[i] |> strsplit('-') |> unlist() |> as.numeric()
  inp_seq <- inp_i[1]:inp_i[2]
  # nch <- nchar(inp_seq[1])

  out <- rep(NA,length(inp_seq))
  for(j in 1:length(inp_seq)){
    # i=2
    nch <- nchar(inp_seq[j])
    if(nch%%2==0){
      fh <- substr(inp_seq[j],1,nch/2) |> as.numeric()
      sh <- substr(inp_seq[j],(nch/2)+1,nch) |> as.numeric()
      out[j] <- ifelse(fh == sh, inp_seq[j], 0)
    } else {
      out[j] <- 0
    }
   
  }
  out_i[[i]] <- out
}

for(i in 1:length(out_i)){
  print(out_i[[i]])
}
sum(unlist(out_i)) #40214376723




# Part 2 ------------------------------------------------------------------
# Now, an ID is invalid if it is made only of some sequence of digits repeated at least twice. So, 12341234 (1234 two times), 123123123 (123 three times), 1212121212 (12 five times), and 1111111 (1 seven times) are all invalid IDs

# 11-22 still has two invalid IDs, 11 and 22.
# 95-115 now has two invalid IDs, 99 and 111.
# 998-1012 now has two invalid IDs, 999 and 1010.
# 1188511880-1188511890 still has one invalid ID, 1188511885.
# 222220-222224 still has one invalid ID, 222222.
# 1698522-1698528 still contains no invalid IDs.
# 446443-446449 still has one invalid ID, 446446.
# 38593856-38593862 still has one invalid ID, 38593859.
# 565653-565659 now has one invalid ID, 565656.
# 824824821-824824827 now has one invalid ID, 824824824.
# 2121212118-2121212124 now has one invalid ID, 2121212121.
# Test answer: Adding up all the invalid IDs in this example produces 4174379265.



