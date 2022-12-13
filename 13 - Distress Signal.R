#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 13, year=2022) |> head(-1)
input <- raw

test <- readLines( "13 test.R")
input <- test


is_ordered <- function( s1, s2 )
{
  if( length(s1) == 0 && length( s2 ) > 0 )
    return( T )
  if( length( s2 ) == 0 && length( s1 ) > 0  )
    return( F )
  if( length( s1 ) == 0 && length( s2  ) == 0)
    return( NA )
  
  v1 = first( s1 )
  v2 = first( s2 )
  
  if( typeof( v1)  == "double" && typeof( v2 ) == "double" )
  {
    if( v1 < v2 )
      return( T )
    if( v2 < v1 ) 
      return( F )
  } else {
    if( typeof( v1) == "double")
      v1 = list( v1 )
    if( typeof( v2 ) == "double")
      v2 = list( v2 )
    rr <- is_ordered( v1, v2)
    if( !is.na( rr ))
      return( rr )
  }

  is_ordered( tail( s1, -1 ), tail( s2, -1))
}

r_code <- input |> str_replace_all( "\\[", "list(") |> str_replace_all( "\\]" ,")")

ordered <- NULL
for( lnum in seq( 1, length( input), by=3 ))
{
  cat( lnum, " ")
  sig1 <- eval( parse( text =r_code[lnum]))
  sig2 <- eval( parse( text =r_code[lnum+1]))
  ordered <- c( ordered, is_ordered( sig1, sig2))
}

# part 1
sum( which( ordered ))

r_code2 <- c( r_code[ r_code != "" ], "list(list(6))", "list(list(2))" )

for( i in 1:(length( r_code2)-1))
  for( j in (i+1):length( r_code2))
  {
    sig1 <- eval( parse( text =r_code2[i]))
    sig2 <- eval( parse( text =r_code2[j]))
    
    if( !is_ordered( sig1, sig2))
    {
      temp <- r_code2[i]
      r_code2[i] <- r_code2[j]
      r_code2[j] <- temp
    }
  }

# part 2
match( "list(list(6))", r_code2 ) *  match(  "list(list(2))", r_code2)
