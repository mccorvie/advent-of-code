library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input01"

#input_raw <- read_csv( file.path( dir, ff) ) 
input_raw <- readLines( file.path( dir, ff)) |> strtoi()

xx  <- input_raw
gg  <- is.na( aa) |> cumsum()
cal <- split( xx, gg) |> map_dbl(~ sum(., na.rm=T)) 

sola <- max( cal )
solb <- sum( sort( cal, decreasing=T)[1:3])


