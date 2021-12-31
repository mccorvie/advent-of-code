library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day11"
seating_m <-  str_split(readLines( file.path( dir, ff)), "", simplify = T)
seating <- crossing( row = 1:nrow(seating_m), col = 1:ncol( seating_m))

adjacent <- crossing( row = 1:nrow(seating_m), col = 1:ncol( seating_m)) %>%
  crossing( expand_grid( dr = -1:1, dc = -1:1) %>% filter( dr !=0 | dc !=0)) %>%
  mutate( adj_row = row+dr, adj_col = col+dc) %>%
  filter( adj_row >= 1, adj_row <= nrow( seating_m), adj_col >=1, adj_col <= ncol( seating_m))%>%
  select( -dr, -dc)

seating<- crossing( row = 1:nrow(seating_m), col = 1:ncol( seating_m)) %>%
  mutate( seat = map2_chr( row, col, ~ seating_m[.x,.y]))

changes <-T
updates <-0
while( changes )
{
  updates <- updates+1
  
  next_seating <- left_join( adjacent, seating, by = c( adj_row ="row", adj_col="col")) %>%
    group_by( row, col) %>%
    summarize( live = sum( seat == "#")) %>%
    left_join( seating, by = c( "row", "col")) %>%
    ungroup %>%
    mutate( 
      new_seat = ifelse( seat == "L" & live == 0, "#", NA ),
      new_seat = ifelse( seat == "#" & live >= 4, "L", new_seat ),
      seat = coalesce( new_seat, seat )
    ) 
  
  cat( updates, summarize( seating, occupied = sum( seat =="#"))$occupied, "\n")

  changes <- summarize( next_seating, changes = any( !is.na( new_seat )))$changes
  seating <- select( next_seating, row, col, seat )
}


answer1 <- summarize( seating, occupied = sum( seat =="#"))
# seating <- arrange( seating, row, col)
# for( idx in 1:max( seating$row ))
#   cat( paste0(filter( seating, row==idx)$seat, collapse=""),"\n")

seating<- crossing( row = 1:nrow(seating_m), col = 1:ncol( seating_m)) %>%
  mutate( seat = map2_chr( row, col, ~ seating_m[.x,.y]))

adjacent <- crossing( row = 1:nrow(seating_m), col = 1:ncol( seating_m)) %>%
  crossing( expand_grid( dr = -1:1, dc = -1:1) %>% filter( dr !=0 | dc !=0)) %>%
  crossing( dir_len = 1:max(nrow(seating_m), ncol(seating_m)))%>%
  mutate( adj_row = row+dr*dir_len, adj_col = col+dc*dir_len) %>%
  filter( adj_row >= 1, adj_row <= nrow( seating_m), adj_col >=1, adj_col <= ncol( seating_m)) %>%
  left_join( seating, by = c( adj_row ="row", adj_col="col")) %>%
  filter( seat == "L") %>%
  group_by( row, col, dr, dc) %>%
  arrange( dir_len ) %>%
  summarize( adj_row= first(adj_row), adj_col =first(adj_col), dir_len = first( dir_len)) %>%
  ungroup %>%
  select( row, col, adj_row, adj_col )

changes <-T
updates <-0
while( changes )
{
  updates <- updates+1
  
  next_seating <- left_join( adjacent, seating, by = c( adj_row ="row", adj_col="col")) %>%
    group_by( row, col) %>%
    summarize( live = sum( seat == "#")) %>%
    left_join( seating, by = c( "row", "col")) %>%
    ungroup %>%
    mutate( 
      new_seat = ifelse( seat == "L" & live == 0, "#", NA ),
      new_seat = ifelse( seat == "#" & live >= 5, "L", new_seat ),
      seat = coalesce( new_seat, seat )
    ) 
  
  cat( updates, summarize( seating, occupied = sum( seat =="#"))$occupied, "\n")
  
  changes <- summarize( next_seating, changes = any( !is.na( new_seat )))$changes
  seating <- select( next_seating, row, col, seat )
  seating <- arrange( seating, row, col)
  # for( idx in 1:max( seating$row ))
  #   cat( paste0(filter( seating, row==idx)$seat, collapse=""),"\n")
  # cat( "\n")  
}

answer2 <- summarize( seating, occupied = sum( seat =="#"))
answer2

# 
# seating <- arrange( seating, row, col)
# for( idx in 1:max( seating$row ))
#   cat( paste0(filter( seating, row==idx)$seat, collapse=""),"\n")
# 
# filter( adjacent, row==1, col==1)
# 
# 
# 
# adjacent <- crossing( row = 1:nrow(seating_m), col = 1:ncol( seating_m)) %>%
#   crossing( expand_grid( dr = -1:1, dc = -1:1) %>% filter( dr !=0 | dc !=0)) %>%
#   crossing( dir_len = 1:max(nrow(seating_m), ncol(seating_m))) %>%
#   mutate( adj_row = row+dr*dir_len, adj_col = col+dc*dir_len) %>%
#   filter( adj_row >= 1, adj_row <= nrow( seating_m), adj_col >=1, adj_col <= ncol( seating_m)) %>%
#   left_join( seating, by = c( adj_row ="row", adj_col="col"))
#   
# 
# aa <-filter( adjacent, row==1, col==1)
# 
# 
#   filter( seat == "L") %>%
#   group_by( row, col, dr, dc) %>%
#   arrange( dir_len ) %>%
#   summarize( adj_row= first(adj_row), adj_col =first(adj_col), dir_len = first( dir_len)) %>%
#   ungroup %>%
#   select( row, col, adj_row, adj_col )


