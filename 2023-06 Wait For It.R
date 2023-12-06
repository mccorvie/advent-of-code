# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 6
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw


#part 1
races <- input |> str_extract_all( "\\d+") |> map( \(v) as.numeric(v))
#part 2
races <- input |> str_extract_all( "\\d+") |> map( \(v) reduce( v, paste0) |> as.numeric())

trials <- map2( races[[1]], races[[2]], \(t,d) tibble( time=t, dist=d, hold=1:t ))|> 
  reduce( add_row )


cc <- trials |> mutate( my_dist = (time-hold)*hold  ) |> 
  filter( my_dist > dist ) |> 
  group_by( time ) |> #danger
  summarize( n=n()) |> 
  ungroup() |> 
  summarize( part1 = prod(n))




## Quadratic formula for the win

# part 1
races <- input |> str_extract_all( "\\d+") |> map( \(v) as.numeric(v))

# part 2
races <- input |> str_extract_all( "\\d+") |> map( \(v) reduce( v, paste0) |> as.numeric())

eps = 1e-6  #add jitter in case roots are integers

tibble( time = races[[1]], dist = races[[2]]) |> 
  mutate( 
    root1 = (time - sqrt( time^2 - 4*dist))/2, 
    root2 = (time + sqrt( time^2 - 4*dist))/2,
    interval = floor( root2-eps) - ceiling( root1+eps) +1
  ) |> 
  summarize( answer = prod( interval ))




