library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day10"
joltages <- as.numeric(readLines( file.path( dir, ff)))

joltages <- sort( c(joltages,0, max( joltages)+3))
tt<-table( joltages[-1]-joltages[-length( joltages)])
answer1 <- tt[1]* tt[2]

num_chains <-rep(NA, max(joltages))
num_chains[ max(joltages)] <- 1

for( cur_joltage in sort(joltages, decreasing=T)[2:(length( joltages)-1)] )
  num_chains[ cur_joltage ]= sum( num_chains[ intersect( joltages, cur_joltage + 1:3) ])
sum( num_chains[ intersect( joltages, 1:3) ])


# num_adapter_strings <- function( cur_joltage )
# {
#   if( cur_joltage > 0 && !is.na( memo[cur_joltage]))
#     return( memo[cur_joltage])
#   #cat( cur_joltage, " ")
#   if( cur_joltage == max( joltages ))
#     return(1)
#   
#   next_joltage <- intersect( joltages, cur_joltage + 1:3)
#   memo[ cur_joltage ] <<- sum( map_dbl( next_joltage, num_adapter_strings ))
#   memo[ cur_joltage ]
# }
# 
# num_adapter_strings( 0 )


