#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

raw <- read_advent(day = 15, year=2022) |> head(-1)
test <- readLines( "test15" )

input<- raw
input <- test

#dir <- "~/Desktop/aoc-input"
#ff  <- "input01"

# input_raw <- read_delim( file.path( dir, ff) ) 
# input_raw <- readLines( file.path( dir, ff))
# input_raw <- scan(  file.path( dir, ff))
