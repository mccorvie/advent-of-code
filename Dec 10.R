library( tidyverse )

dir <- "~/Desktop/Advent-Code-2021/Dec 10"
ff <- "input"
#ff<-"test"

input_raw <- readLines( file.path( dir, ff))


syn_open  <- c( "(", "[", "{", "<" )
syn_close <- c( ")", "]", "}", ">" )
syn_err_score<- c( ")"= 3, "]" = 57, "}" = 1197, ">"= 25137)
syn_complete_score <- c( ")"= 1, "]" = 2, "}" = 3, ">"= 4 )

error_score= 0
complete_scores = NULL

for( line_num in 1:length( input_raw) )
{
  line = input_raw[line_num]
  syn_expect <- NULL
  cat( "checking", line_num, "\n")
  for( pos in 1:str_length( line))
  {
    cc <- str_sub( line, pos, pos)
    cat( cc )
    if( cc %in% syn_open )
    {
      syn_expect <- c( syn_close[ syn_open == cc ] , syn_expect )
      cat( "+" )
    }
    else if( cc == syn_expect[1])
    {
      syn_expect = syn_expect[-1]
      cat( "-" )
    }
    else
    {
      cat( "X\nline", line_num, ": Expected",syn_expect[1], "found",  cc, "\n")
      error_score = error_score + syn_err_score[[ cc ]]
      syn_expect = NULL
      break
    }
  }
  
  if( length( syn_expect))
  {
    cat( "ac:", syn_expect, "\n")
    place_value <- 5^((length( syn_expect )-1):0)
    ac_score = sum( sapply( syn_expect, function(x) syn_complete_score[[x]] ) * place_value )
    cat( length(syn_expect), ac_score, "\n")
    complete_scores = c( complete_scores, ac_score)
  }
}

answer1 = error_score
answer2 = median(complete_scores)

