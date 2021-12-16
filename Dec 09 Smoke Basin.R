library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 09"
ff <- "input"
#ff<-"test"

input_raw <- readLines( file.path( dir, ff))

heights <- matrix(strtoi(str_split( input_raw, "", simplify = T)), nrow=100, ncol=100)
# add a frame of high numbers
heights <- rbind( rep( 10, 102 ), cbind( rep(10,100), heights, rep(10,100)), rep( 10, 102))

risk=0
for( i in 2:101)
  for( j in 2:101)
  {
    if( heights[i,j] < min( c( heights[i-1, j], heights[i+1, j], heights[i,j-1], heights[i, j+1])))
      risk = heights[i,j]+1+risk
  }
risk

sizes <- NULL
for( i in 2:101)
  for( j in 2:101)
  {
    if( heights[i,j] < 0 || heights[i,j] >= 9)
      next
    
    check_list <- list( c(i,j))
    size= 0
    while( length(check_list)>0)
    {
      # pop the top point
      i <- check_list[[1]][1]
      j <- check_list[[1]][2]
      
      #cat( i,j,":", heights[i,j],"\n")
      check_list <- tail( check_list, length( check_list)-1)
      
      if( heights[i,j] < 0 || heights[i,j] >= 9)
        next
      
      size = size+1
      heights[i,j] <- -1
      
      check_list <- c( check_list,list( c( i+1,j), c(i-1,j), c(i,j-1), c(i,j+1)))
      #cat( "rows:",nrow( check_list), "\n" )
    }
    cat( i,j, "->", size, "\n")
    sizes <- c( sizes, size )
  }

prod(tail( sort( sizes), 3))
