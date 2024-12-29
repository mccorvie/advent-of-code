#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 21
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = T
input = if( use_test ) test else raw

dir <- list( "v" = c(1,0), ">"= c(0,1), "^" = c(-1,0), "<" = c(0,-1))

alpha_keys <- c( "7","4","1",NA,"8","5","2","0","9","6","3","A") |> matrix( ncol=3)
alpha_keys
arrow_keys <- c( NA, "<", "^", "v", "A", ">") |> matrix( nrow =2)
arrow_keys
alpha <- sort( alpha_keys)
arrow <- sort( arrow_keys )
alpha
from <- "A"
to   <- "5"

alpha_seq <- \(from,to)
{
#  debugtext <- paste0( from, "->", to, " " )
  debugtext <- ""
  dd <- which( alpha_keys == to, arr.ind=T )-which( alpha_keys == from, arr.ind=T )
  vert  <- rep( ifelse( dd[1] < 0, "^", "v"), abs( dd[1])) |> paste0(collapse="")
  horiz <- rep( ifelse( dd[2] < 0, "<", ">"), abs( dd[2])) |> paste0(collapse="")
  if( dd[1] <0 ) return( paste0( debugtext, vert, horiz, "A"))
  return( paste0( debugtext, horiz,vert, "A"))
}

map( alpha, \(to) map_chr( alpha, \(from) alpha_seq(to,from) ))
alpha

arrow_seq <- \(from,to)
{
  dd <- which( arrow_keys == to, arr.ind=T )-which( arrow_keys == from, arr.ind=T )
  vert  <- rep( ifelse( dd[1] < 0, "^", "v"), abs( dd[1])) |> paste0(collapse="")
  horiz <- rep( ifelse( dd[2] < 0, "<", ">"), abs( dd[2])) |> paste0(collapse="")
  if( dd[1] <0 ) return( paste0( debugtext, horiz, vert, "A"))
  return( paste0( debugtext, vert, horiz, "A"))
}

map( arrow, \(to) map_chr( arrow, \(from) arrow_seq(to,from) ))

expand_seq <- \(seq, map_fun) 
{
  seq <- c("A", str_split_1( seq, "" ))
  map_chr( 1:(length(seq)-1), \(n) map_fun( seq[n], seq[n+1])) |> paste0(collapse="")
}

complexity <- \(seq)
{
  in_seq <- seq |> expand_seq( alpha_seq ) |> expand_seq( arrow_seq ) |> expand_seq(arrow_seq)
  a <- str_length( in_seq )
  b <- as.numeric( str_sub( seq, 1,-2 ))
  cat( seq, "->", a, " * ", b, "=", a*b, "\n")
  a*b
}

input |> map_dbl( complexity) |> sum()

68 * 29 + 60 * 980 + 68 * 179 + 64 * 456 + 64 * 379

debugtext <- paste0( from, " -> ", to, ": " )
debugtext <- ""

execute_arrow <- \(seq,alpha=F)
{
  if( alpha ) {
    mm <- alpha_keys
    pos <- c(4,3) |> matrix( nrow=1)
  } else {
    mm <- arrow_keys
    pos <- c(1,3) |> matrix( nrow=1)
  }
  seq <- str_split_1(seq,"")
  out <- ""
  for( ss in seq )
  {
    if( ss != "A") {
      pos <- pos + dir[[ss]]
    } else {
      out <- paste0( out, mm[pos] )
    }
  }
  out
}
seq <- "379A"
#in_seq <- 

  seq |> expand_seq( alpha_seq) |> expand_seq( arrow_seq) |> expand_seq(arrow_seq)
in_seq
in_seq |> str_split_1( "") |> table()
mm <- arrow_keys
seq <- "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
seq <- "v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A"
execute_arrow( seq, F ) |> execute_arrow( F) |> execute_arrow( T)

xor(T,F)

str_length(in_seq)
