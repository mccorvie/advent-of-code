
require( tidyverse)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day24"
lines <- readLines( file.path( dir, ff))
#lines <- c( "", "e")

ref <- c(0,0,0)
dir_offset <- list( 
  "se" = c(1,-1,0), 
  "sw" = c(0,-1,1), 
  "ne"= c( 0,1,-1), 
  "nw" = c( -1,1,0),
  "e" = c(1,0,-1), 
  "w" = c(-1,0,1)
)

ww <- 80
hex <- list()

hex_grid <- expand_grid( x = -ww:ww, y=-ww:ww) %>%
  mutate( z= -x-y, color = 0 ) %>%
  filter( abs(z)<=ww)


for( path in lines )
{
  vv <- ref
  while( str_length(path))
  { 
    dir <- str_sub( path, 1,1)
    if( dir=="s" | dir=="n")  
      dir <- str_sub( path, 1,2)
    vv <- vv + dir_offset[[dir]]
    
    path <- str_sub( path, str_length(dir)+1 )  
  }
  
  hex_grid <- hex_grid %>% 
    mutate( 
      flip = if_else( x==vv[1] & y==vv[2] & z==vv[3], 1, 0 ),
      color = flip + color
    ) %>%
    select( -flip )
}

hex_grid %>% count( color )

# answer 1
hex_grid <- hex_grid %>%
  mutate( color = ( color%%2)==1)
hex_grid %>% pull( color ) %>% sum()

# Suppress summarize info
options(dplyr.summarise.inform = FALSE)

neighbors <- tribble(
  ~dx, ~dy, ~dz,
   1,  -1,   0,
  -1,   1,   0,
   1,   0,  -1,
  -1,   0,   1,
   0,   1,  -1,
   0,  -1,   1
)

for( idx  in 1:100)
{
  hex_grid <- expand_grid( hex_grid, neighbors) %>%
    mutate( nx = x+dx, ny = y+dy, nz = z+dz) %>%
    filter( abs(nx) <=ww, abs(ny)<=ww, abs( nz)<=ww) %>%
    left_join(rename(hex_grid, ncolor=color), by = c( nx="x", ny = "y", nz="z")) %>%
    group_by( x,y,z, color) %>%
    summarize( bneighbor = sum(ncolor)) %>%
    mutate( color = if_else( color,  bneighbor == 2 | bneighbor==1, bneighbor==2)) %>%
    select( -bneighbor)

  black_hex <- filter( hex_grid, color)
  
  if( idx <=10 || (idx%%10)==0)
    cat( idx, " ", sum( hex_grid$color), " ", max( c( abs(black_hex$x), abs(black_hex$y),abs( black_hex$z ))),"\n" )  
}




