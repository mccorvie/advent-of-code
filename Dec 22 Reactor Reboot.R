library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/Dec 22"
ff  <- "input"
#ff<-"test2"
part1 <- F

input_raw <- readLines( file.path( dir, ff))
rg<- regex( "^(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)")

toggle <- str_match( input_raw, rg)[,2]
coords <- matrix(strtoi(str_match( input_raw, rg)[,3:8]), ncol=6)

if( part1 )
  coords <- coords[apply( abs(coords)<=50, 1, all),]

## part 1 easy algorithm
# coords <- coords + 50
# lights <- array( F, dim=c(100,100,100))
# for( rr in 1:nrow( coords))
#    lights[coords[rr,1]:coords[rr,2],coords[rr,3]:coords[rr,4],coords[rr,5]:coords[rr,6]] <- toggle[rr]=="on"
# sum( lights)

colnames( coords) <- c( "xmin", "xmax", "ymin","ymax","zmin","zmax")
coord_t <-as_tibble( coords) %>%
  mutate( level = 0, volume = (xmax-xmin+1)*(ymax-ymin+1)*(zmax-zmin+1))
coord_t

coord_universal <- coord_t[1,]
for( rr in 2:nrow( coord_t))
{
  this_box <- coord_t[rr,]
  coord_universal <- coord_universal %>%
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
    filter( xmin < xmax, ymin < ymax, zmin < zmax ) %>%
    select( xmin, xmax, ymin, ymax, zmin, zmax, level, volume ) %>%
    bind_rows( coord_universal)

  if( toggle[rr] == "on")
    coord_universal <- bind_rows( coord_universal, this_box )

  #cat( rr, nrow( coord_universal ), "\n")
}

answer <- coord_universal %>%
  summarize( total_on = sum( volume * (-1)^level))

sprintf( "%.100g", answer$total_on)


