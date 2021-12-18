library( tidyverse)
library(stringr)

dir <- "~/Desktop/Advent-Code-2021/Dec 18"
ff  <- "input"
lines <- readLines( file.path( dir, ff))

sf_parse <- function( cc )
{
  rr <- regex( "^(\\[|\\]|\\d+|,)" )
  sf_math <- list()
  new_tok <- ","
  while( !is.na( new_tok))
  {
    if( str_detect( new_tok, "\\d"))
      new_tok <- strtoi( new_tok)
    if( new_tok != ",")
      sf_math <- c( sf_math, new_tok )
    new_tok <- str_extract( cc, rr )
    cc <- str_remove( cc, rr)
  }
  sf_math
}

sf_explode <- function()
{
  exp_pos <- detect_index(accumulate( map( sf_math,  ~ if_else( .== "[", 1, if_else( . == "]",-1,0))), sum ),~ .>4 )
  if( !exp_pos)
    return( F)

  ll <- sf_math[1:(exp_pos-1)]
  rr <- sf_math[(exp_pos+4):length(sf_math)]
  idx_l <- detect_index( ll, is.numeric, .dir = "backward")
  if( idx_l )
    ll[[idx_l]] <- ll[[idx_l]]+sf_math[[exp_pos+1]]
  idx_r <- detect_index( rr, is.numeric,.dir="forward")
  if( idx_r )
    rr[[idx_r]] <- rr[[idx_r]]+ sf_math[[exp_pos+2]]
  sf_math <<- c( ll, 0, rr)
  return( T )
}


sf_split <- function()
{
  exp_pos <- detect_index( sf_math,  ~ .>=10 )
  if( !exp_pos)
    return( F)
  sf_math <<- c( sf_math[1:(exp_pos-1)], "[", floor(sf_math[[exp_pos]]/2),ceiling(sf_math[[exp_pos]]/2), "]",sf_math[(exp_pos+1):length(sf_math)])
  return( T )
}

sf_addition <- function( aa, bb)
{
  sf_math <<- c( "[", aa, bb, "]" )
  while( sf_explode() || sf_split())
  {
    #cat( length( sf_math), "\n")
  }
  #cat( "===> ", paste0( sf_math, collapse ="."), "\n") 
  sf_math
}

sf_accumulate <- function( sf_expr )
{
  if( length( sf_expr ) ==1)
    return( sf_expr[[1]])
  sf_expr <- sf_expr[2:(length(sf_expr)-1)]
  split_pos <- detect_index( accumulate( map( sf_expr, ~ if_else( .== "[", 1, if_else( . == "]",-1,0))), sum ),~ !. )
  return( 3* sf_accumulate( sf_expr[1:split_pos] ) + 2*sf_accumulate( sf_expr[(split_pos+1):length( sf_expr)] ) )
}

# reduce( map( lines, sf_parse),sf_addition)
# paste0( sf_math, collapse =".")
# sf_accumulate( sf_math)

cnt <- 0
mm <- 0
for( line1 in lines)
  for( line2 in lines)
  {
    cnt <- cnt+1
    if( !( cnt%%10))
      cat( cnt, "/ 10000\n")
    sf_addition( sf_parse( line1), sf_parse(line2))
    mm <- max( mm, sf_accumulate(sf_math))
  }
mm



# sf_math1 <- sf_parse( "[[[[4,3],4],4],[7,[[8,4],9]]]" )
# sf_math2 <- sf_parse( "[1,1]" )
# 
# sf_explode()
# 
# cc <- "[[[99,[33,88]],[[0,9],6]],[[[3,7],[4,9]],3]]"
# cc <- "[[[[[9,8],1],2],3],4]"
# 
# cc<-"[[[[[9,8],1],2],3],4]"
# cc<-"[7,[6,[5,[4,[3,2]]]]]"
# cc<-"[[6,[5,[4,[3,2]]]],1]"
# cc<-"[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
# cc<-"[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
# cc<-"[[[[0,7],4],[15,[0,13]]],[1,1]]"
# 
# sf_math <- sf_parse( cc)
# paste0( sf_math, collapse =".")
# sf_split()
# paste0( sf_math, collapse =".")
# sf_explode()


# 
# lines <-c(
# "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
# "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
# "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
# "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
# "[7,[5,[[3,8],[1,4]]]]",
# "[[2,[2,2]],[8,[8,1]]]",
# "[2,9]",
# "[1,[[[9,3],9],[[9,0],[0,7]]]]",
# "[[[5,[7,4]],7],1]",
# "[[[[4,2],2],6],[8,7]]"
# )
# 
# cc <- "[[1,2],[[3,4],5]]" # becomes 143.
#  cc <-"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" #becomes 1384.
#  cc<-"[[[[1,1],[2,2]],[3,3]],[4,4]]" #becomes 445.
#  cc<-"[[[[3,0],[5,3]],[4,4]],[5,5]]" #becomes 791.
#  cc<-"[[[[5,0],[7,4]],[5,5]],[6,6]]" #becomes 1137.
#  cc<-"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" #becomes 3488.
# 
# xx <- 
# xx

