#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 10
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "" ))

use_test = F
input = if( use_test ) test else raw

dim = length( input )
trailmap <- input |> map( \(ss) tibble( elev = as.numeric(str_split_1( ss, "")), c=1:str_length(ss) )) |>
  map2( as.list( 1:length(input)), \(tt, idx) mutate( tt, r=idx)) |>
  reduce( add_row) |> 
  mutate( label = paste0( "n",r,"-",c))

add_reachable_col <- \(tt, label0 ) tt |> mutate( {{label0}} := {{label0}}==label )
start_labels <- trailmap |> filter( elev == 0) |> pull( label) |> as.list()
reachable <- c( list( trailmap ), start_labels) |> reduce( add_reachable_col )


path_links <- trailmap |> expand_grid( dr = c(-1, 0, 1 ), dc = c( 1,0,-1)) |> 
  filter( abs(dr)!=abs(dc)) |> 
  mutate( r_n = r+dr, c_n = c+dc) |> 
  left_join( rename( trailmap, elev_n = elev, r_n = r, c_n = c, label_n = label ) ) |> 
  filter( !is.na(elev_n), elev_n+1 == elev) |> 
  select( label, elev, r,c,r_n,c_n)

path_links <- trailmap |>  anti_join( path_links) |> 
  mutate( r_n = r, c_n = c) |> 
  bind_rows( path_links )


for( elev in 1:9 )
  reachable <- path_links |> 
    left_join( reachable, by = c(r_n = "r", c_n = "c")) |> 
    group_by( r, c ) |> 
    summarize( across( starts_with( "n"), \(x) any(x) ), elev  =last(elev.x), label = last(label.x)) |> 
    ungroup()

reachable |> filter( elev == 9) |> summarize(  across( starts_with( "n"), \(x) sum(x) )) |> sum()

num_paths <- trailmap |> mutate( num = as.numeric(elev==0))


for( elev in 1:9 )
  num_paths <- path_links |> 
    left_join( num_paths, by = c(r_n = "r", c_n = "c")) |> 
    group_by( r, c ) |> 
    summarize( num = sum(num), elev  =last(elev.x), label = last(label.x)) |> 
    ungroup()

num_paths |> filter( elev == 9) |> summarize(sum(num))

