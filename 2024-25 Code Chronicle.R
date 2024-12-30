#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 25
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = F
input = if( use_test ) test else raw

input <- c( input, "")
gaps <- which(input=="")
schematics <- map( gaps, \(n) str_split( input[(n-7):(n-1)], "", simplify=T)=="#" )
locks <- schematics |> keep( \(mm) all( mm[1,]))
keys  <- schematics |> keep( \(mm) all( mm[nrow(mm),]))

map( locks, \(lock) map_lgl( keys, \(key) !any( lock & key))) |> unlist() |> sum()
