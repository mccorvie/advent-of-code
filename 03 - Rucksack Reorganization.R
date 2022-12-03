library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input03"

# input_raw <- read_delim( file.path( dir, ff) ) 
input <- readLines( file.path( dir, ff))
# input_raw <- scan(  file.path( dir, ff))


lengths=  str_length(input)


ruck1 = str_sub( input, 1, lengths/2) |> str_split("")
ruck2 = str_sub( input,lengths/2+1, lengths) |> str_split("")

low <- letters
upper <- c( rep( "", 26),LETTERS)

aa <- map2( ruck1, ruck2, intersect) |> map( ~ which(. == low)) |> map_dbl(~ ifelse( length(.)>0,.,0))
bb <- map2( ruck1, ruck2, intersect) |> map( ~ which(. == upper)) |> map_dbl(~ ifelse( length(.)>0,.,0))

gg <- split( input,rep( 1:(length(input)%/%3), each=3))

gg
length( input)

badge <- NULL
for( idx in 1:length(gg))
{
  rucks = str_split( gg[[idx]] , "")
  badge <- c( badge,intersect( intersect( rucks[[1]], rucks[[2]]), rucks[[3]]))
  
}

aa <- badge |> map( ~ which(. == low)) |> map_dbl(~ ifelse( length(.)>0,.,0))
bb <- badge |> map( ~ which(. == upper)) |> map_dbl(~ ifelse( length(.)>0,.,0))

sum(aa+bb)
