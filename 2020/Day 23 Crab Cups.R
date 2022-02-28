
require( tidyverse)
require( plyr)
input <- c( 7, 8, 9, 4, 6, 5, 1, 2, 3 )
# input <- c( 3, 2, 4, 1, 5 )
#input <- c( 3,8,9,1,2,5,4,6,7)
succ

succ <- rep( NA, length(input))
for( idx in 1:(length( input)-1))
  succ[ input[idx]] <- input[idx+1]
succ[ input[length(input)]] <- input[1]

unwind <- function( cur )
{
  out <- cur
  nn <- succ[ cur ]
  while( nn != cur )
  {
    out <- c( out, nn )
    nn <- succ[ nn ]
  }
  out
}

cur <- input[1]
for( i in 1:100 )
{
  n1 <- succ[cur]
  n2 <- succ[n1]
  n3 <- succ[n2]
  n4 <- succ[n3]
  
  ins <- ifelse( cur-1 == 0, length( input ), cur-1 )
  while( ins %in% c( n1,n2,n3) )
    ins <- ifelse( ins-1 == 0, length( input ), ins-1 )
  #cat( "to ", ins, "\n")
  
  ins_succ <- succ[ ins ] 
  succ[ cur] <- n4
  succ[ ins ] <- n1
  # succ[ n1 ] <- n2
  # succ[ n2 ] <- n3
  succ[ n3 ] <- ins_succ
  
  cur <- succ[ cur ]
  
  #cat( paste0( unwind( cur ), collapse = ""), "\n")
}

#answer1
str_sub(paste0( unwind( 1 ), collapse = ""),2)


succ <- 2:1000001
for( idx in 1:(length( input)-1))
  succ[ input[idx]] <- input[idx+1]
succ[ input[length(input)]] = length( input )+1
succ[1000000] = input[1]

cur <- input[1]
rounds <- 10000000
for( i in 1:rounds )
{
  if( i%%10000 == 0)
    cat( i/rounds  * 100, " ")
  
  n1 <- succ[cur]
  n2 <- succ[n1]
  n3 <- succ[n2]
  n4 <- succ[n3]
  
  ins <- ifelse( cur-1 == 0, length( succ ), cur-1 )
  while( ins %in% c( n1,n2,n3) )
    ins <- ifelse( ins-1 == 0, length( succ ), ins-1 )
  #cat( "to ", ins, "\n")
  
  ins_succ <- succ[ ins ] 
  succ[ cur] <- n4
  succ[ ins ] <- n1
  # succ[ n1 ] <- n2
  # succ[ n2 ] <- n3
  succ[ n3 ] <- ins_succ
  
  cur <- succ[ cur ]
}
head( succ,1000)
succ[1]
succ[succ[1]]
#answer 2
succ[1] * succ[succ[1]]
