#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 6
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

mm <- str_split( str_trim( input ), " +", simplify=T)
ops <- mm[nrow(mm),]
mm <- mm[1:(nrow(mm)-1),] |>  apply( 2, as.numeric)
ss <- mm[, ops=="+"] |> apply( 2, sum) |> sum()
pp <- mm[, ops=="*"] |> apply( 2, prod) |>  sum()
ss+pp



read_numbers <- \(mm) apply( mm, 2, \(x) as.numeric(paste0( x, collapse="")))
mm <- str_split( input, "", simplify=T)

ops <- mm[nrow(mm),]
col_break <- c( which( ops!=" "), length( ops)+2)

total = 0
for( prob_n in 1:(length( col_break)-1))
{
  text_block <- mm[ 1:(nrow(mm)-1), col_break[prob_n]:(col_break[prob_n+1]-2)]
  nums <- read_numbers( text_block )
  res <- ifelse( ops[col_break[prob_n]] == "*", prod( nums ), sum( nums ))
  cat( res, "\n")
  total <- total+res
}
total

