library( tidyverse)

#  3 5 7 9 
#12345678901#
#############
#...........#
###D#A#A#D###
  #D#C#B#A#
  #D#B#A#C#
  #C#C#B#B#
  #########

trapped <- list( NULL,NULL, c( "D", "D", "D", "C"), NULL,c( "A", "C", "B", "C"), NULL,c( "A", "B", "A", "B"), NULL, c( "D", "A", "C", "B"), NULL,NULL)
#trapped <- list( NULL,NULL, c( "D", "C"), NULL,c( "A",  "C"), NULL,c( "A",  "B"), NULL, c( "D",  "B"), NULL,NULL)
depth <- length(trapped[[3]])
state <- rep(".",11)
cost  <-  c( "A"= 1, "B"=10, "C"=100, "D"=1000)
home  <- c( "A"= 3, "B"= 5, "C"=7, "D"=9)
resting <- c( 1,2,4,6,8,10,11)

initial_cost <- sum(map_dbl( trapped,  ~ sum(unlist(cost[.]) * 1:depth)))
final_cost   <- sum(sum(1:depth)*unlist( cost))

memo <- list()

recurse_num<-0
dfs <- function( trapped, state, depth )
{
  out <- Inf
  state_summary <- sapply( trapped, length )
  state_summary[ state_summary==0] <- state[state_summary==0] 
  state_summary <- paste0( state_summary,collapse="")
  if( !is.null(memo[[ state_summary]]))
    return(memo[[ state_summary ]] )
  
  occupied <- which( state != ".")
  
  recurse_num<<-recurse_num+1
  if( recurse_num%%10000 ==0)
  {
    cat( recurse_num,  depth, sum(sapply( trapped, length))+length( occupied ), paste0( state_summary, collapse=""),"\n")
  }
  
  if( sum(sapply( trapped, length))+length( occupied )==0)
    return( 0 )

  # bring 'em home
  for( ss in occupied )
  {
    amphipod <- state[ss]
    if( length(trapped[[home[amphipod]]]) ==0 && !any( ss < occupied & occupied < home[amphipod]  | ss > occupied & occupied > home[amphipod] ))
    {
      new_state <- state
      new_state[ss] <- "."
      out<- min( out, dfs( trapped, new_state, depth+1 ) + abs(ss- home[amphipod])* cost[amphipod])
    }
  }
  
  # take 'em out
  is_trapped <- which(sapply( trapped, length ) >0)
  for( ss in is_trapped)
  {
    amphipod <- trapped[[ss]][1]

    if( length(trapped[[home[amphipod]]]) ==0 && !any( ss < occupied & occupied < home[amphipod]  | ss > occupied & occupied > home[amphipod] ))
    {
      new_trapped <- trapped
      new_trapped[[ss]] <- new_trapped[[ss]][-1]
      out<- min( out, dfs( new_trapped, state, depth+1 ) + abs(ss- home[amphipod])* cost[amphipod])
    }
    
    for( rr in setdiff(resting, occupied))
      if( !any( ss < occupied & occupied < rr  | ss > occupied & occupied > rr ))
      {
        new_state <- state
        new_state[rr] <- amphipod
        new_trapped <- trapped
        new_trapped[[ss]] <- new_trapped[[ss]][-1]
        out<- min( out, dfs( new_trapped, new_state, depth+1 ) + abs(rr-ss)* cost[amphipod])
      }
  }
  memo[state_summary] <<- out
  out
}

min_cost <- dfs( trapped, state,1 )

min_cost + initial_cost+final_cost
