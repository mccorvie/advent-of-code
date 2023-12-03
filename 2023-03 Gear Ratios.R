# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 3
raw <- read_advent(day = day, year=2023) |> head(-1)

# raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

part_num_loc <- str_locate_all( input, "\\d+") |> 
  map2( 1:length(input), \(part_num0,linenum) as_tibble( part_num0 ) |> mutate( linenum=linenum)) |> 
  reduce( add_row ) |> 
  mutate( value = as.numeric( str_sub( input[linenum], start,end)))

adj_num <- \( target ) 
  part_num_loc |> filter( 
    linenum <= target[1]+1, start <=target[2]+1,  
    linenum >= target[1]-1, end >=target[2]-1
  )

# part1
symbol_loc <-  str_split( input, "" , simplify=T) |> 
  str_detect( "(\\d|\\.)", negate=T) |> 
  matrix( nrow = length(input)) |> which( arr.ind=T)

1:nrow( symbol_loc) |> 
  map( \(idx) adj_num( symbol_loc[idx,] )) |> 
  reduce( add_row) |> 
  unique() |> pull( value ) |> sum()

# part 2
star_loc <- str_split( input, "" , simplify=T) |> 
  str_detect( "\\*") |> 
  matrix( nrow = length(input)) |> which( arr.ind=T)

1:nrow( star_loc) |> 
  map( \(idx) adj_num( star_loc[idx,] )) |> 
  keep( \(adj) nrow(adj)==2) |> 
  map_dbl( \(adj) adj |> pull( value ) |> prod() ) |> 
  sum()





# version 1 for part 1
mm <- str_split( input, "" , simplify=T) 
part_num_loc <- str_locate_all( input, "\\d+")

total <- 0
for( linenum in seq_along(part_num_loc))
{
  
  part_num_line <- part_num_loc[[linenum]]
  
  for( loc in seq_along(part_num_line[,1]))
  {
    pn_start <- part_num_line[loc,"start"]
    pn_end   <- part_num_line[loc,"end"]
    value    <-  str_sub( input[linenum], pn_start,pn_end ) |> as.numeric()
    
    box_rr <- max(1,linenum-1):min(nrow(mm),linenum+1)
    box_cc <- max(1,pn_start-1):min( ncol(mm), pn_end+1 )
    
    if( any(str_detect(  mm[box_rr, box_cc], "(\\d|\\.)", negate=T)))
    {
      cat( linenum, " adding ", value, "\n")  
      total <- total + value
    } else {
      cat( linenum, " skipping ", value, "\n")  
    }
  }
}

total



