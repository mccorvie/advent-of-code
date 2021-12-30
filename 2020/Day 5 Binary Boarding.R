library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day05"
input <- str_split( readLines( file.path( dir, ff)), "" )
seats <- unlist(map( input, ~ sum( (. == "B"| .=="R" ) * 2^(9:0))))
answer1 <- max( seats)
answer1

answer2 <- setdiff( min(seats):max(seats), seats)
answer2

