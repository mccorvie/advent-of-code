library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input03"

input <- readLines( file.path( dir, ff))
lengths =  str_length(input)
ruck1 = str_sub( input, 1, lengths/2) |> str_split("")
ruck2 = str_sub( input,lengths/2+1, lengths) |> str_split("")

all_lets <- c( letters,LETTERS)

part1 <- map2( ruck1, ruck2, intersect) |> map_dbl( ~ which(. == all_lets)) |> sum()

gg <- split( input,rep( 1:(length(input)%/%3), each=3))
badge <- NULL
for( idx in 1:length(gg))
{
  rucks = str_split( gg[[idx]] , "")
  badge <- c( badge,intersect( intersect( rucks[[1]], rucks[[2]]), rucks[[3]]))
}

part2 <- badge |> map_dbl( ~ which(. == all_lets)) |> sum()

