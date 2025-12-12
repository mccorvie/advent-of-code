#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 9
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = T
input = if( use_test ) test else raw

coords <- input |> unglue_data( "{x},{y}", convert = T) |> as_tibble() |> mutate( id = 1:n())
nt <- nrow( coords)

area <- expand_grid( a=1:nt, b=1:nt) |> filter( a>b) |> 
  left_join( coords, by = join_by( a == id)) |> 
  rename( xa=x,ya=y ) |> 
  left_join( coords, by = join_by( b == id)) |> 
  rename( xb=x,yb=y ) |> 
  mutate( area = (abs(xa-xb)+1)*(abs(ya-yb)+1)) |> 
  arrange( -area )

area |> first() |> pull( area) # part 1

segments <- coords |> 
  rename( x1 = x, y1=y ) |> 
  mutate( next_id = id%%nt+1 ) |> 
  left_join( coords, by=join_by( next_id ==id )) |> 
  rename( x2=x, y2=y) |> 
  select( -next_id)

rect_analysis <- expand_grid( area, tt=1:nt) |> 
  left_join( segments, by = join_by( tt == id)) |> 
  group_by( a, b, xa, ya, xb, yb, area ) |> 
  mutate( 
    disjoint = max(xa, xb) <= min( x1,x2) | 
      max(x1,x2) <= min( xa,xb) |
      max(ya,yb) <= min( y1,y2) | 
      max(y1,y2) <= min( ya,yb)
    ) |> 
  summarize( covered = all( disjoint )) |> 
  filter( covered ) |> 
  arrange( -area )

  
            
            
            
            xt & xt < max(xa,xb) & min(ya, yb) < yt & yt < max(ya,yb)) 

filter( rect_analysis, xt==7, yt==3)




