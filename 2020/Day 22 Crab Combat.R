library( tidyverse)


p1<-c(47,19,22,31,24,6,10,5,1,48,46,27,8,45,16,28,33,41,42,36,50,39,30,11,17)
p2<-c(4,18,21,37,34,15,35,38,20,23,9,25,32,13,26,2,12,44,14,49,3,40,7,43,29)

#test
# p1<- c( 9,2,6,3,1)
# p2<- c( 5,8,4,7,10)
# 
# round<-1
# while( length(p1) && length(p2))
# {
#   if( p1[1] > p2[1])
#   {
#     p1 <- c(p1[-1], p1[1], p2[1])
#     p2 <- p2[-1]
#   }
#   else
#   {
#     p2 <- c(p2[-1], p2[1], p1[1])
#     p1 <- p1[-1]
#   }
#   round<-round+1
#   if( round==100)
#     cat( round, ")", paste(p1,collapse="-"), "***", paste(p2,collapse="-"), "\n")
# }
# pp <-c( p1, p2)
# sum( pp* length(pp):1)
# 

memo <- list()

recursive_game <- function( p1, p2, level =1 )
{
  repeat_tracker <- list()
  while( length(p1) && length(p2))
  {
    key = paste( paste( p1, collapse="-"), paste(p2, collapse="-"), sep="|" )
    
    if( !is.null( memo[[key]]))
    {
      memo[names( repeat_tracker )] <- memo[[key]] 
      return( memo[[key]])
    }
    
    #cat( key, "\n")
    if( !is.null( repeat_tracker[[key]]))
    {
      # player 1 wins infinite games
      memo[names(repeat_tracker)] <- 1
      return( 1 )
    }
              
    repeat_tracker[[key]] = T
    
    if( p1[1] < length( p1) & p2[1]<length(p2))
      winner <- recursive_game( head( p1[-1],p1[1]), head( p2[-1], p2[1]),level+1)
    else
      winner <- if_else(p1[1]>p2[1], 1,2)

    if( winner ==1 )
    {
      p1 <- c(p1[-1], p1[1], p2[1])
      p2 <- p2[-1]
    }
    else if( winner==2)
    {
      p2 <- c(p2[-1], p2[1], p1[1])
      p1 <- p1[-1]
    }
    else
      stop( paste("invalid winner", winner))
    
    round<<-round+1
    if( round%%10000==0)
      cat( round, level, "\n")
  }
  
  if( level==1 )
  {
    pp <-c( p1, p2)
    return( sum( pp* length(pp):1))
  }
  
  if( length( p1))
    winner=1
  else if( length( p2))
    winner=2
  else
    stop( "empty decks")
  
  memo[names( repeat_tracker )] <<- winner
  winner
}

#inf loop
# p1<-c( 43, 19)
# p2<-c( 2, 29, 14)
round<-1
recursive_game(p1,p2)
length(memo)
