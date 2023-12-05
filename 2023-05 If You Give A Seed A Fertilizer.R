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

use_test = F
input = if( use_test ) test else raw

blocks0 <- input |> keep( \(l)  l!="" ) |> str_extract_all( "\\d+") |> map( \(v) as.numeric(v))

seeds  <- blocks0[[1]]
blocks <- tail( blocks0, -1)

extract_from_map <- \(idx) blocks |> map_dbl( \(bb) if( length(bb)==0) NA else bb[idx])

map_t <- tibble( 
  dest = extract_from_map(1), 
  source = extract_from_map(2), 
  maplen = extract_from_map(3), 
  level = cumsum( is.na( extract_from_map(1))),
  offset = dest-source
) |> filter( !is.na(dest)) 

# add identity maps for gaps between rules
gaps <- map_t |> group_by(level) |> arrange( level,source ) |> 
  mutate( lagsource = lag(source+maplen, default=0)) |> 
  ungroup() |> 
  filter( lagsource < source ) |> 
  mutate(
    maplen = source-lagsource, 
    source = lagsource,
    dest   = lagsource,
    offset = 0
  ) |> select( -lagsource) 

# add identity map for large numbers
global_max <- max( map_t$source + map_t$maplen)
ends <- map_t |> group_by( level ) |> 
  summarize( source = max( source + maplen )) |> 
  filter( source < global_max ) |> 
  mutate(
    dest = source,
    offset = 0, 
    maplen = global_max - source
  )
  
map_t <- add_row( map_t, gaps ) |> add_row( ends) |> arrange( level, source)

seed_map <- \( number, level=7 )
{
  if( level == 0 ) return( number )

  number <- seed_map( number, level-1)
  map_num <- map_t |> filter( level == !!level, number >= source, number < source + maplen )
  return( number + map_num$offset )
}

seeds |> map_dbl( seed_map ) |> min()

seed_map_segment <- \( number, numlen, level=7 )
{
  endnum <- number+numlen
  
  map_range <- map_t |> 
    filter( level == !!level, number  < source + maplen,source  < number + numlen ) |> 
    mutate( 
      maplen = if_else( source < number, source+maplen - number, maplen ),
      source = pmax( source, number ),
      dest   = source + offset,
      maplen = if_else( source+maplen > endnum, endnum-source, maplen )
    )
  
  map_range |> mutate( number = dest, numlen = maplen ) |> select( number, numlen )
}

seed_map_range <- \( range_t, level=7 )
{
  if( level == 0 ) return( range_t )

  range_t <- seed_map_range( range_t, level-1)

  1:nrow( range_t) |> 
    map( \(rr) seed_map_segment( range_t$number[rr], range_t$numlen[rr], level )) |> 
    reduce( add_row )
}

range_t <- matrix( seeds, ncol=2, byrow=T) |> as_tibble() |> rename( number=V1, numlen=V2)

seed_map_range( range_t,7) |> pull( number ) |> min()


