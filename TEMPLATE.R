#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = xx, year=2022) 

#dir <- "~/Desktop/aoc-input"
#ff  <- "input01"

# input_raw <- read_delim( file.path( dir, ff) ) 
# input_raw <- readLines( file.path( dir, ff))
# input_raw <- scan(  file.path( dir, ff))
