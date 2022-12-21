#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)
options(digits=20)

day = 21
#raw  <- read_advent(day = day, year=2022) |> head(-1)
raw <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

test
use_test = F
input = if( use_test ) test else raw

monkeys <- unglue_data(input, "{name}: {formula}") |> 
  pull( formula, name = name )

calculate <- \( var )
{
  expr = monkeys[ var ]
  if( expr != "NA" && is.na( strtoi( expr ) ))
  {
    vars = list( str_sub( expr, 1,4),  str_sub( expr, length( expr)-5))
    map_dbl( vars, calculate  )
  }
  
  out = eval( parse( text = paste0( var, "<<-", expr )))
  return( out )
}

#part1
xx <- calculate( "root")
print( xx, digits=20)


inv_calc <- \( var )
{
  expr = monkeys[ var ]
  if( expr != "NA" && is.na( strtoi( expr ) ))
  {
    vars = list( str_sub( expr, 1,4),  str_sub( expr, length( expr)-5))
    sub = map_dbl( vars, inv_calc  )
    if( any( is.na( sub )))
    {
      op = str_sub( expr, 6,6)
      freevar = which( is.na( sub ))

      # print the inverse formula
      cat( vars[[ freevar ]], " = ")
      if( op == "+")
        cat( var, "-", sub[3-freevar ])
      else if( op == "*")
        cat( var, "/", sub[3-freevar ])
      else if( op == "-" && freevar ==1 )
        cat( var, "+", sub[3-freevar ])
      else if( op == "-" && freevar ==2 )
        cat(  sub[3-freevar ], "-", var )
      else if( op == "/" && freevar ==1 )
        cat( var, "*", sub[3-freevar ])
      else if( op == "/" && freevar ==2 )
        cat(  sub[3-freevar ], "/", var )
      cat( "\n")
    }
  }
  
  out = eval( parse( text = paste0( var, "<<-", expr )))
  return( out )
}

monkeys["humn"] = "NA"
monkeys
inv_calc( "root")
