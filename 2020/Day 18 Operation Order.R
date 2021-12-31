library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day18"

get_tok <- function()
{
  if( loc > str_length( expr))
    return( ")" )
  tok <- str_sub( expr, loc, loc)
  loc <<- loc+1
  tok
}

math_expressions <-  str_replace_all( readLines( file.path( dir, ff)), " ","")

get_term <- function()
{
  accumulator <-0 
  operator    <- "+"
  while(T)
  {
    tok <- get_tok()
    if( tok == "(")
      term = get_term()
    else
      term = strtoi( tok )
    
    if( operator == "+")
      accumulator <- accumulator + term
    else if( operator == "*")
      accumulator <- accumulator * term
    else
      stop( paste( "unknown operator", operator ))
    operator <- get_tok()

    if( operator == ")")
      break
  }
  accumulator
}

eval_expr <- function( ex )
{
  expr <<- ex 
  loc<<-1
  get_term()
}

sprintf( "%.100g", sum( map_dbl( math_expressions, eval_expr )))

# expr <- "1 + 2 * 3 + 4 * 5 + 6"
# expr <- "1 + (2 * 3) + (4 * (5 + 6))"
# expr <-"2 * 3 + (4 * 5)"
# expr <-"5 + (8 * 3 + 9 + 3 * 4 * 3)"
# expr <-"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
# expr <-"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"


`+` <- function( a, b) prod( a,b)
`*` <- function( a,b) sum( a,b)
answer2<-0
for( expr in math_expressions)
{
  expr <- str_replace_all( expr, "\\+","\\#")
  expr <- str_replace_all( expr, "\\*","\\+")
  expr <- str_replace_all( expr, "\\#","\\*")
  answer2 <- sum( answer2, eval( str2lang( expr)))
}
rm( `+`)
rm( `*`)
sprintf( "%.100g", answer2)

