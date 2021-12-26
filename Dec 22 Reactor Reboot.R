
library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/Dec 22"
ff  <- "input"
#ff<-"test"

input_raw <- readLines( file.path( dir, ff))
rg<- regex( "^(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)")

toggle <- str_match( input_raw, rg)[,2]
coords <- matrix(strtoi(str_match( input_raw, rg)[,3:8]), ncol=6)
coords <- coords[apply( abs(coords)<=50, 1, all),] + 50

lights <- array( F, dim=c(100,100,100))
for( rr in 1:nrow( coords))
{ 
   lights[coords[rr,1]:coords[rr,2],coords[rr,3]:coords[rr,4],coords[rr,5]:coords[rr,6]] <- toggle[rr]=="on"
}
sum( lights)
