library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/Dec 22"
ff  <- "input"
# ff<-"test2"
part1 <- F

input_raw <- readLines( file.path( dir, ff))
rg<- regex( "^(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)")

toggle <- str_match( input_raw, rg)[,2]
coords <- matrix(strtoi(str_match( input_raw, rg)[,3:8]), ncol=6)

if( part1 )
  coords <- coords[apply( abs(coords)<=50, 1, all),]

## part 1 easy algorithm
# coords <- coords + 51
# lights <- array( F, dim=c(101,101,101))
# for( rr in 1:nrow( coords))
#    lights[coords[rr,1]:coords[rr,2],coords[rr,3]:coords[rr,4],coords[rr,5]:coords[rr,6]] <- toggle[rr]=="on"
# sum( lights)

colnames( coords) <- c( "xmin", "xmax", "ymin","ymax","zmin","zmax")
fundamental_boxes <-as_tibble( coords) %>%
  mutate( level = 0, volume = (xmax-xmin+1)*(ymax-ymin+1)*(zmax-zmin+1))

all_boxes <- fundamental_boxes[1,]
for( rr in 2:nrow( fundamental_boxes))
{
  this_box <- fundamental_boxes[rr,]
  all_boxes <- all_boxes %>%
    mutate( 
      xmin0 = this_box$xmin, xmax0 = this_box$xmax, 
      ymin0 = this_box$ymin, ymax0 = this_box$ymax, 
      zmin0 = this_box$zmin, zmax0 = this_box$zmax, 
      xmin = pmax( xmin, xmin0 ), xmax = pmin( xmax, xmax0 ),
      ymin = pmax( ymin, ymin0 ), ymax = pmin( ymax, ymax0 ),
      zmin = pmax( zmin, zmin0 ), zmax = pmin( zmax, zmax0 ),
      level = level+1,
      volume = (xmax-xmin+1)*(ymax-ymin+1)*(zmax-zmin+1)
    ) %>%
    filter( xmin <= xmax, ymin <= ymax, zmin <= zmax ) %>%
    select( -xmin0, -xmax0, -ymin0, -ymax0, -zmin0, -zmax0 ) %>%
    bind_rows( all_boxes)

  if( toggle[rr] == "on")
    all_boxes <- bind_rows( all_boxes, this_box )

  #cat( rr, nrow( all_boxes ), "\n")
}

answer <- all_boxes %>%
  summarize( total_on = sum( volume * (-1)^level))

sprintf( "%.100g", answer$total_on)

# all_boxes %>% count( level )
# all_boxes %>% group_by( level) %>% summarize( total_on = sum( volume * (-1)^level))


