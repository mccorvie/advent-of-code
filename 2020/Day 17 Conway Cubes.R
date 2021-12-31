library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day17"
ccubes_m <-  str_split(readLines( file.path( dir, ff)), "", simplify = T)
ccubes   <- crossing( x = 1:ncol(ccubes_m), y = 1:nrow( ccubes_m), z=0) %>%
  mutate( active = map2_lgl( x, y, ~ ccubes_m[.y,.x]=="#"))

for( tt in 1:6)
{
  ccubes <- crossing( dx = -1:1, dy = -1:1, dz=-1:1) %>% 
    filter( dx!=0 | dy!=0 | dz!=0)%>%
    crossing( ccubes ) %>%
    rename( adj_active = active, adj_x = x, adj_y=y, adj_z=z ) %>%
    mutate( x = adj_x - dx, y=adj_y - dy, z=adj_z - dz) %>%
    left_join( ccubes, by = c("x","y","z")) %>%
    mutate( active = coalesce( active, F)) %>%
    group_by( x,y,z) %>%
    summarize( neighbors = sum( adj_active ), active = last(active)) %>%
    ungroup %>%
    mutate( 
      new_active = if_else( active, if_else( neighbors==2 | neighbors==3, T,F),NA),
      new_active = if_else( !active, if_else( neighbors==3, T,F), new_active)
    ) %>%
    mutate( active = new_active ) %>%
    select( x,y,z, active) 
  
}

answer1 <- summarize( ccubes, total_on = sum( active))

ccubes4   <- crossing( x = 1:ncol(ccubes_m), y = 1:nrow( ccubes_m), z=0,w=0) %>%
  mutate( active = map2_lgl( x, y, ~ ccubes_m[.y,.x]=="#"))

for( tt in 1:6)
{
  ccubes4 <- crossing( dx = -1:1, dy = -1:1, dz=-1:1, dw=-1:1) %>% 
    filter( dx!=0 | dy!=0 | dz!=0 | dw!=0)%>%
    crossing( ccubes4 ) %>%
    rename( adj_active = active, adj_x = x, adj_y=y, adj_z=z, adj_w=w ) %>%
    mutate( x = adj_x - dx, y=adj_y - dy, z=adj_z - dz, w=adj_w-dw) %>%
    left_join( ccubes4, by = c("x","y","z", "w")) %>%
    mutate( active = coalesce( active, F)) %>%
    group_by( x,y,z,w) %>%
    summarize( neighbors = sum( adj_active ), active = last(active)) %>%
    ungroup %>%
    mutate( 
      new_active = if_else( active, if_else( neighbors==2 | neighbors==3, T,F),NA),
      new_active = if_else( !active, if_else( neighbors==3, T,F), new_active)
    ) %>%
    mutate( active = new_active ) %>%
    select( x,y,z,w, active) 
  
}

answer2 <- summarize( ccubes, total_on = sum( active))
answer2
