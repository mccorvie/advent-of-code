#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 16
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "b" ))

use_test = F
input = if( use_test ) test else raw

grid_t <- input |> map( \(ss) tibble( map = str_split_1( ss, ""), c=1:str_length(ss) )) |>
  map2( as.list( 1:length(input)), \(tt, idx) mutate( tt, r=idx)) |>
  reduce( add_row)
grid_t



offset <- list( "^" = c(-1,0),  ">" = c(0,1), "v" = c(1,0), "<" = c(0,-1) )
clock  <- names(offset)
names( clock ) <- lag( clock, default=last(clock))
cclock  <- names(offset)
names( cclock ) <- lead( cclock, default=first(cclock))


states <- grid_t |> filter( map=="." | map == "E" | map =="S")   |> crossing( dir =names(offset))
states0 <- states |> filter( map != "E" ) |> select( -map )

neigh_move <- states0 |> 
   mutate( 
     c_n = c + map_dbl( dir, \(d) offset[[d]][2]),
     r_n = r + map_dbl( dir, \(d) offset[[d]][1])
   ) |> 
   inner_join( states, by = c( dir = "dir", c_n="c", r_n="r")) |> 
   mutate( dir_n = ifelse( map =="E", "E",dir), cost =1) |> 
   #filter( map.x != "E") |> 
   select( -starts_with( "map"))

neigh_rot1 <- states0 |> 
  mutate( c_n=c, r_n=r, dir_n = map_chr( dir, \(d) clock[[d]]), cost=1000 )

neigh_rot2 <- states0 |> 
  mutate( c_n=c, r_n=r, dir_n = map_chr( dir, \(d) cclock[[d]]), cost=1000 ) 

neighbors <- bind_rows( neigh_move, neigh_rot1, neigh_rot2 )

visited <- grid_t |> filter( map == "S") |> select( -map ) |> mutate( distance=0, dir = ">")
#active  <- visited
visited

while( nrow( filter( visited, dir == "E"))==0)
{
  visited <- visited  |> 
    left_join( neighbors, by = c("dir", "r", "c")) |> 
    mutate( distance_n = distance + cost ) |> 
    select( r=r_n, c=c_n, dir = dir_n, distance = distance_n ) |> 
    anti_join( visited, by = c( "r", "c", "dir") ) |> 
    group_by( r, c, dir ) |> 
    summarize( distance = min(distance), .groups="drop") |> 
    filter( distance == min( distance )) |> bind_rows( visited )
  #cat( nrow( visited ), "\n")

}

#visited_stash <- visited

closest_neighbors <- neighbors |> left_join( visited, by = c(dir_n ="dir",r_n = "r",c_n = "c")) |> 
  rename( distance_n = distance ) |> 
  left_join( visited, by = c( "r","c","dir")) |> 
  filter( distance_n == distance + cost) |> 
  select( r_p = r, c_p = c, dir_p = dir, distance_p = distance, r=r_n, c=c_n, dir = dir_n )

in_path <- visited |> filter( dir == "E")
all_nodes <- NULL
while( nrow( filter( in_path, distance == 0))==0)
{
  all_nodes <- bind_rows( in_path, all_nodes )
  in_path <- in_path |> inner_join( closest_neighbors, by = c("r","c","dir")) |> 
    select( r= r_p, c=c_p, dir = dir_p, distance = distance_p) |> 
    distinct()
  #cat( paste0(in_path$r, ",", in_path$c, " ", in_path$dir, "\n"), "\n")
}

all_nodes |> select( r,c) |>  distinct() |> nrow()

