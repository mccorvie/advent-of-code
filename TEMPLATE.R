library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input01"

input_raw <- read.delim( file.path( dir, ff) ) 

input_raw <- readLines( file.path( dir, ff))

input_raw <- scan(  file.path( dir, ff))