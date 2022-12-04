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
part2



##
## xapply functional programming ----
##

input <- sapply(readLines( file.path( dir, ff)), \(x) strsplit(x, ""))

## Part 1

double_letters <- sapply(input, \(x) intersect(x[1:(length(x)/2)], x[(length(x)/2+1):length(x)]))
sum(match(double_letters, c(letters, LETTERS)))

## Part 2

idx <- rep(1:(length(input)/3), each = 3)
list <- split(input, idx)
tripple_letters <- sapply(list, \(x) Reduce(intersect, x))
sum(match(tripple_letters, c(letters, LETTERS)))
