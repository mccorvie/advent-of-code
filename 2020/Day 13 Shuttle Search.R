library( tidyverse)
library( numbers)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day13"
buses <- readLines( file.path( dir, ff))
first_depart <- strtoi(buses[1])
buses <- strtoi( str_subset( str_split( buses[2], ",")[[1]], regex( "\\d+")) )
waits <- buses - first_depart %% buses

waits[ waits==buses] <- 0
argmin <- which( waits == min(waits))
buses[argmin] * waits[argmin]

input <- readLines( file.path( dir, ff))
busline <- input[2]
# busline <- "17,x,13,19"
# busline <- "67,7,59,61"
# busline <- "67,x,7,59,61"
# busline <- "67,7,x,59,61"
# busline <- "1789,37,47,1889"
padded_buses <- strtoi(str_split( busline , ",")[[1]])
buses <- strtoi( str_subset( str_split( busline, ",")[[1]], regex( "\\d+")) )

offset <- 0
cycle  <- buses[1]

for( new_bus in buses[-1])
{
  arrival <- new_bus - (which( padded_buses ==new_bus)%%new_bus-1)
  
  offset_idx<- which( seq( from=offset, to=cycle*new_bus-1, by=cycle ) %% new_bus == arrival)
  offset <- seq( from=offset, to=cycle*new_bus-1, by=cycle )[ offset_idx]
  cycle <- as.numeric( cycle * new_bus)
  cat(new_bus, arrival, "->", offset, cycle , "\n")
}
sprintf( "%.100g",offset)
