#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 23
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = F
input = if( use_test ) test else raw

v1_v <- input |> str_split( ",") |> map_chr( \(s) str_sub(s, 1, 2))
v2_v <- input |> str_split( ",") |> map_chr( \(s) str_sub(s, 4, 5))
connections_all <- tibble( v1 = c(v1_v,v2_v),v2=c(v2_v,v1_v)) 
connections_ord <- connections_all |> filter( v1>=v2)

triangles <- connections_ord |> 
  left_join( connections_ord, by = c(v2="v1"), relationship = "many-to-many") |> 
  rename( v3= v2.y) |> 
  inner_join( rename( connections_all,v4=v2), by = c(v3="v1"), relationship = "many-to-many") |> 
  filter( v1==v4) 

# part 1 
triangles |> filter( str_starts(v1,"t") | str_starts(v2,"t") | str_starts(v3,"t")) |> nrow()

# huh it turns out every computer is connected to exactly 13 others
# I'll bet the largest clique is close to size 14 lets find it
size <- connections_all |> group_by( v1 ) |> summarize( n())

size12 <- \(target)
{
  neighbors <- connections_all |> filter( v1 == target ) |> pull(v2)
  # count the number of internal links by node or a subset of nodes
  clique_test <- connections_all |> 
    filter( v1 %in% c(target, neighbors )) |> mutate( link=1) |> 
    pivot_wider(names_from = v2, values_from = link, values_fill = 0) |> 
    summarize( across( !starts_with("v"), sum))
  tt <- clique_test |> slice(1) |> unlist() |> ta
  # if 12 of my neighbors are linked to by me and 12 other neighbors, its a 13-clique
  !is.na( tt["12"] ) && tt[ "12"] == 12  
}

# part 2
c(v1_v,v2_v) |> unique() |> keep( comps, size12 ) |> sort() |> paste0( collapse="," )
