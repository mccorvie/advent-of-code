#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 8
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

dim = length( input )
grid_t <- input |> map( \(ss) tibble( map = str_split_1( ss, ""), c=1:str_length(ss) )) |>
  map2( as.list( 1:length(input)), \(tt, idx) mutate( tt, r=idx)) |>
  reduce( add_row)

stations <- grid_t |> filter( map != ".")

harmonics <- 2  # part 1
harmonics <- 1:dim # part 2

stations |> rename( r2=r, c2=c) |> 
  left_join(stations, by = "map", relationship = "many-to-many") |> 
  filter( r != r2 | c != c2) |> 
  expand_grid( d = harmonics) |> 
  mutate( r_antinode = r + d * (r2-r), c_antinode = c + d* (c2-c)) |> 
  select( r_antinode, c_antinode ) |> 
  distinct() |> 
  filter( r_antinode <= dim, c_antinode <= dim, r_antinode >= 1, c_antinode >=1  ) |> 
  summarize( n())


