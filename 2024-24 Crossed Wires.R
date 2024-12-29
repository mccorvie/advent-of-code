#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 24
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "b" ))

use_test = F
input = if( use_test ) test else raw

inputgap <- which( input == "" )
input_bits <- input[1:(inputgap-1)] |> str_match( "([a-z0-9]{3}): (\\d)")
map_chr( 1:nrow(input_bits), \(n) paste0( "aoc_",input_bits[n,2], "<<- ", input_bits[n,3]))  |> 
  walk( \(rcode ) eval( parse( text = rcode )))

device <- input[(inputgap+1):length(input)] |> str_match( "([a-z0-9]{3}) ([A-Z]+) ([a-z0-9]{3}) -> ([a-z0-9]{3})")
instructions <- list()
1:nrow(device) |> walk( \(n) instructions[[device[n,5]]] <<- device[n,2:5])

and <- \(x,y) x&&y
or  <- \(x,y) x||y
operate_device <- \(var)
{
  instr <- instructions[[var]]
  if( !exists( paste0( "aoc_",instr[1]))) operate_device( instr[1])
  if( !exists( paste0( "aoc_", instr[3]))) operate_device( instr[3])
  rcode <- paste0( "aoc_",instr[4], "<<-", str_to_lower( instr[2]),"( aoc_", instr[1], ", aoc_", instr[3], ")")
  eval( parse( text = rcode ))
}

output_bits <- names( instructions) |> str_extract( "^z\\d+") |> sort() |> map_lgl( operate_device )
sum(2^(0:(length(output_bits)-1)) * output_bits )
output_bits

