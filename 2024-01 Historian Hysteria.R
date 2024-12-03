# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 1
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

input

nums <- map( input, \(x) str_split_1( x, "[ ]+" )) |> 
  map( as.numeric ) 

list1 <- map_dbl( nums, \(x) x[1])
list2 <- map_dbl( nums, \(x) x[2])
listd <- sort(list1)-sort( list2)
#part 1
listd |> abs() |> sum()


table1 <- table( list1)
table2 <- table( list2)
# part2
( table2[names(table1)] * as.numeric( names( table1 ))*table1) |> sum( na.rm=T)

