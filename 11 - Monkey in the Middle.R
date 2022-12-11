#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

source( "11 - input.R")

monkeys= realmonkeys
modkey = prod(map_dbl( monkeys, ~ .$divtest))

partnum = 2

inspect_count = rep( 0, length( monkeys ))
rounds = if( partnum == 2 ) 10000  else 20
rounds
for( round in 1:rounds )
{
  cat( "round ", round, "\n")
  for( midx  in seq_along( monkeys) )
  {
    monkey = monkeys[[midx]]
    inspect_count[midx] = inspect_count[midx] + length( monkey$items )
    monkeys[[midx]]$items = NULL
#    cat( midx, " ", length( monkey$items ), "\n")
    for( item in monkey$items ) 
    {
      if( partnum ==1 )
        item <- monkey$worryop( item ) %/% 3
      else if( partnum ==2 )
        item <- monkey$worryop( item ) %% modkey
      else
        stop( paste( "what partnum is ", partnum ))
      
      toidx = if( item %% monkey$divtest == 0  ) monkey$throwT else monkey$throwF
      monkeys[[toidx+1]]$items = c( monkeys[[toidx+1]]$items, item )
#      cat( midx-1, " throwing ", item, " to ", toidx, "\n")
    }
  }
  cat( map_dbl( monkeys, ~ length( .$items )), "\n" )
}

prod( tail( sort( inspect_count), 2))
