library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day20"
lines <- readLines( file.path( dir, ff))
line_groups <- split( lines, cumsum( lines==""))

side_key <- function( vv )
{
  sum( 2^(9:0) * (vv == "#") )
}


tile_table <- tibble()
tiles <- list()
for( cc in names( line_groups ))
{
  tile <- line_groups[[cc]]
  if( length( tile )==11)
    tile <- c( "", tile)
  if( length( tile) < 12 ) 
  {
    message( paste( "skipping tile", cc, "\n"))
    next
  }
  
  tt <- str_split( tile[3:12], "", simplify = T)
  tiles[[ tile[2]]] <- tt
  
  tile_table <- tibble( 
    name = rep(tile[2], 4), 
    sidet = c( "a","a","b","b"), 
    siden = c( 1,2,1,2 ),
    key1 = c( side_key( tt[1,]), side_key( tt[10,]), side_key( tt[,1]), side_key( tt[,10])),
    key2 = c( side_key( rev(tt[1,])), side_key( rev( tt[10,])), side_key( rev( tt[,1])), side_key( rev( tt[,10])))
  ) %>%
    mutate( side = paste0( sidet, siden), key = pmin( key1,key2)) %>%
    mutate( name_num = as.numeric(str_match(  name, "Tile (\\d+):")[,2] )) %>%
    bind_rows( tile_table )
}

edge_cnt <- tile_table %>% count( key)

corners <- left_join( tile_table, edge_cnt, by = "key") %>%
  filter( n == 1) %>%
  count( name ) %>%
  filter( n==2 ) %>%
  mutate( name_num = as.numeric(str_match(  name, "Tile (\\d+):")[,2] )) 

## answer 1
sprintf( "%.1000g",prod( corners$name_num))

follow_chain <- function( cur_edge, cur_tile )
{
  edge_chain <- cur_edge
  tile_chain <- cur_tile
  
  while( length( cur_tile ))
  {
    cur_sidet <- filter( tile_table, name_num == cur_tile, key == cur_edge ) %>% pull( sidet )
    cur_edge  <- filter( tile_table, name_num == cur_tile, sidet == cur_sidet, key != cur_edge ) %>% pull( key )
    edge_chain <- c( edge_chain, cur_edge )
    cur_tile <- filter( tile_table, key == cur_edge, name_num != cur_tile ) %>% pull( name_num )
    tile_chain <- c( tile_chain, cur_tile )
  }
  return( list( edge_chain = edge_chain, tile_chain = tile_chain ))
}

orient_tile <- function( cur_tile, top, left )
{
  tile <- tiles[[ sprintf( "Tile %d:", cur_tile )]]
  if( filter( tile_table, name_num == cur_tile, key == top) %>% pull( sidet) == "b")
    tile <- t( tile )

  if( filter( tile_table, name_num == cur_tile, key == top) %>% pull( siden) == 2 )
    tile <- tile[10:1,]

  if( filter( tile_table, name_num == cur_tile, key == left) %>% pull( siden) == 2 )
    tile <- tile[,10:1]

  tile[2:9,2:9]
}

start_tile  <- corners$name_num[1]
outer_edges <- filter( tile_table, name_num == start_tile) %>% left_join( edge_cnt ) %>% filter( n==1) %>%pull(key)

start_top  <- outer_edges[1]
start_left <- outer_edges[2]

left_chain <- follow_chain( start_top, start_tile)
left_tiles <- left_chain$tile_chain

left_edges = left_join( tile_table, edge_cnt, by = "key" ) %>% 
  filter( n==1, name_num %in% left_tiles, !( key %in% left_chain$edge_chain ) ) %>%
  left_join( tibble( order = 1:length( left_tiles ), name_num = left_tiles ), by = "name_num") %>%
  arrange( order) %>%
  pull( key )

top_chain <- follow_chain( start_left, start_tile )
top_tiles <- top_chain$tile_chain
top_edges <- left_join( tile_table, edge_cnt, by = "key" ) %>% 
  filter( n==1, name_num %in% top_tiles, !( key %in% top_chain$edge_chain ) ) %>%
  left_join( tibble( order = 1:length( top_tiles), name_num = top_tiles), by = "name_num") %>%
  arrange( order) %>%
  pull( key )


stitched <- NULL
tile_table0 <- tile_table
for( left_idx in 1:length( left_tiles ))
{
  top_chain <- follow_chain( left_edges[left_idx], left_tiles[left_idx ] )
  top_tiles <- top_chain$tile_chain
  
  stitched_row <- NULL
  for( top_idx in 1:length( top_tiles ))
    stitched_row <- cbind( stitched_row, orient_tile( top_tiles[top_idx], top_edges[top_idx], top_chain$edge_chain[top_idx]))

  # get the bottom edge of the
  top_edges <- tile_table  %>% 
    filter( name_num %in% top_tiles, !( key %in% c( top_chain$edge_chain, top_edges ) )) %>%
    left_join( tibble( order = 1:length( top_tiles), name_num = top_tiles), by = "name_num") %>%
    arrange( order) %>%
    pull( key )

  stitched <- rbind( stitched, stitched_row )
}

count_monsters <- function( sea_monster, stitched )
{
  count <- 0
  for( rr in 1:(nrow(stitched)-nrow(sea_monster)) )
    for( cc in 1:(ncol(stitched)-ncol(sea_monster)))
    {
      rrange <- rr:(rr+nrow(sea_monster)-1)
      crange <- cc:(cc+ncol(sea_monster)-1)
      if( all( stitched[ rrange,crange ] | !sea_monster) )
      {
        count <- count + 1
        all_monsters[ rrange, crange ] <<- all_monsters[ rrange,crange] | sea_monster 
      }
    }
  count
}

sea_monster <- matrix(c(
  F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,
  T,F,F,F,F,T,T,F,F,F,F,T,T,F,F,F,F,T,T,T,
  F,T,F,F,T,F,F,T,F,F,T,F,F,T,F,F,T,F,F,F
), nrow=3, byrow=T)



stitched_lgl <- stitched == "#"
all_monsters <- stitched_lgl &F


# test against 8 orientations of the sea monster

count <- count_monsters(sea_monster, stitched_lgl )
sea_monster <- sea_monster[3:1,]
count <- count + count_monsters(sea_monster, stitched_lgl )
sea_monster <- sea_monster[,20:1]
count <- count + count_monsters(sea_monster, stitched_lgl )
sea_monster <- sea_monster[3:1,]
count <- count + count_monsters(sea_monster, stitched_lgl )

stitched_lgl <- t( stitched_lgl )
all_monsters <- t( all_monsters )

count <- count + count_monsters(sea_monster, stitched_lgl )
sea_monster <- sea_monster[3:1,]
count <- count + count_monsters(sea_monster, stitched_lgl )
sea_monster <- sea_monster[,20:1]
count <- count + count_monsters(sea_monster, stitched_lgl )
sea_monster <- sea_monster[3:1,]
count <- count + count_monsters(sea_monster, stitched_lgl )

count
all_monsters

sum(stitched_lgl & !all_monsters)



