#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 3
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))
test <- readLines( paste0( "test", day,"b" ))

use_test = F
input = if( use_test ) test else raw

instr <- input |> str_match_all( "mul\\([0-9]+,[0-9]+\\)") |> unlist()
mul   <- \(x,y) x*y

#part 1
instr |> map_dbl( \(tt) eval(parse( text = tt ))) |> sum()

do <- \() 
{
  is_this_thing_on <<-T
  0
}

dont <- \() 
{
  is_this_thing_on <<-F
  0
}

mul <- \(x,y)
{
  if( !is_this_thing_on ) return( 0 )
  x*y
}

is_this_thing_on <- T
instr <- input |> 
  str_match_all( c( "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" ) ) |> 
  unlist() |> 
  str_remove_all( "'") |> # functions names in R can't have '
  map_dbl( \(tt) eval(parse( text = tt ))) |> 
  sum()


