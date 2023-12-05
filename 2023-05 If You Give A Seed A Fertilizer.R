# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 5
raw <- read_advent(day = day, year=2023) |> head(-1)

# raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

use_test = T
input = if( use_test ) test else raw

blocks0 <- input |> keep( \(l)  l!="" ) |> str_extract_all( "\\d+") |> map( \(v) as.numeric(v))

seeds  <- blocks0[[1]]
blocks <- tail( blocks0, -1)

extract_from_map <- \(idx) blocks |> map_dbl( \(bb) if( length(bb)==0) NA else bb[idx])

map_t <- tibble( 
  source0 = extract_from_map(2), 
  source1 = source0 + extract_from_map(3), 
  offset = extract_from_map(1) -source0,
  level = cumsum( is.na( extract_from_map(1)))
) |> filter( !is.na(source0)) 

# add identity maps for gaps between rules
gaps <- map_t |> group_by(level) |> arrange( level,source0 ) |> 
  mutate( lag1 = lag(source1, default=0)) |> 
  filter( lag1 < source0 ) |> 
  mutate(
    source1 = source0,
    source0 = lag1,
    offset = 0
  ) |> select( -lag1 ) |>  ungroup()

# add identity map for large numbers
global_max <- max( map_t$source1 )
ends <- map_t |> group_by( level ) |> 
  summarize( source0 = max( source1 )) |> 
  filter( source0 < global_max ) |> 
  mutate(
    source1 = global_max,
    offset = 0
  ) |> ungroup()

map_t <- map_t |> add_row( gaps ) |> add_row( ends) |> arrange( level, source0)

# part 1
seed_map <- \( number, level=7 )
{
  if( level == 0 ) return( number )
  
  number <- seed_map( number, level-1)
  map_num <- map_t |> filter( level == !!level, number >= source0, number < source1 )
  if( nrow( map_num ) != 1) stop( "problem with mapping")
  return( number + map_num$offset )
}

seeds |> map_dbl( seed_map ) |> min()

#part 2
seed_map_segment <- \( number0, number1, level=7 )
{
  map_t |> 
    filter( level == !!level, number0  < source1, source0  < number1 ) |> 
    mutate( 
      number0 = pmax( source0, number0 ) + offset,
      number1 = pmin( source1, number1 ) + offset
    ) |> 
    select( number0, number1 )
}

seed_map_range <- \( range_t, level=7 )
{
  if( level == 0 ) return( range_t )
 
  range_t <- seed_map_range( range_t, level-1)
  1:nrow( range_t) |> 
    map( \(rr) seed_map_segment( range_t$number0[rr], range_t$number1[rr], level )) |> 
    reduce( add_row )
}

range_t <- matrix( seeds, ncol=2, byrow=T) |> 
  as_tibble() |> mutate( number1 = V1+V2) |>  
  select( number0=V1, number1) 

seed_map_range( range_t,7) |> pull( number0 ) |> min()


