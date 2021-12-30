library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day07"
input <-readLines( file.path( dir, ff))

id <- 0
type_id <- list()
get_id <- function( type )
{
  if( !is.null( type_id[[type]]))
    return( type_id[[type]])
  id <<- id+1
  type_id[[type]] <<- id
  id
}

#bag clause = D D bags
#q bag clause = N <bag clause>
#<bag clause> contain <q bag clause>[, <q bag clause>]*.
dep_matrix <- matrix( 0, nrow=594, ncol=594)
for( line_idx in 1:length(input))
{
  parseline <- input[line_idx]
  parseline <- str_split( str_replace_all( paste( parseline, collapse=" "), "\\.|\\,",""), " " )[[1]]
  from_type <- paste( parseline[1],parseline[2])
  dep_start <- seq( 5, to=length( parseline), by=4 )
  
  cat( line_idx, " == ", from_type, "(", get_id( from_type), ") --> " )
  for( dep in dep_start)
  {
    if( parseline[dep]=="no")
    {
      cat( "none")
      next
    }
    nn <- strtoi( parseline[dep])
    to_type <-  paste( parseline[dep+1],parseline[dep+2])
    cat( nn, to_type, "(", get_id( to_type), ")," )
    dep_matrix[ get_id( from_type), get_id( to_type )] = nn
  }
  cat( "\n" )
}

dep_all <- 0
dep_ll  <- dep_matrix
for( path_length in 1:20)
{
  dep_all <- dep_all + dep_ll
  dep_ll <-  dep_matrix %*% dep_ll
}

dep_all <- dep_all > 0
answer1 <- sum(dep_all[ , get_id( "shiny gold") ])

cumbags <- function( cur_bag )
{
  dep <- dep_matrix[cur_bag,]
  sum( dep[dep>0] * map_dbl( which( dep >0 ), cumbags ))+1
}

answer2 <- cumbags( get_id( "shiny gold"))-1
