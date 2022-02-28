library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day20"
lines <- readLines( file.path( dir, ff))
tiles <- split( lines, cumsum( lines==""))


for( cc in names( tiles ))
{
  tile <- tiles[[cc]]
  
}

tile
