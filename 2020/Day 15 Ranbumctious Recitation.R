library(tidyverse)

nums<- c(0,14,1,3,7,9)

while( length( nums) < 2020)
{
  ww <- which( nums==last( nums))
  if( length( ww)==1)
  {
    nums <- c(nums,0)
  } else {
    nums <- c( nums, length( nums ) - ww[ length(ww)-1])
  }
}
answer1 <- last(nums)

last_turn <-30000000
pos <- as.list(rep(NA,last_turn))
pos[[1]]  <- 1
pos[[15]] <- 2
pos[[2]]  <- 3
pos[[4]]  <- 4
pos[[8]]  <- 5
cur <- 9

# pos[[4]] <-1
# pos[[2]] <- 2
# cur <- 2
first_turn <- max(unlist( pos), na.rm=T) +1
for( turn in first_turn:(last_turn-1))
{
  if( turn >= last_turn-100)
    cat( "***", turn, cur, "\n" )
  
  if( turn%%100000 == 0)
    cat( turn/100000, " ")
  
  last <- pos[[cur+1]]
  pos[[cur+1]] <- turn
  
  if( is.na( last ))
    cur <- 0
  else
    cur <- turn-last
}

answer <-cur
answer

