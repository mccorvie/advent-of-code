
library( tidyverse )
library( adventr)
#dir <- "~/Desktop/aoc-input"
#ff  <- "input01"


raw <- read_advent(day = 5, year = 2022 ) |> head(-1)

stacks0 <- raw[1:8]
stacks <- map( seq( from = 2, by  =4, length.out=9), ~ str_sub( stacks0, .,.) ) |>
  map( ~ rev( .[.!= " "])) 
stacks

moves0 <- raw[11:(length( raw)-1)] |> str_split( " ", simplify = T)

moves  <- cbind( strtoi( moves0[,2]),strtoi( moves0[,4]),strtoi( moves0[,6]))
colnames( moves ) <- c( "crates", "from", "to")

moves  <- tibble( raw = raw[11:length( raw)]) |>
  extract( raw, c("ncrates", "from", "to"), "move (\\d+) from (\\d+) to (\\d+)", convert = T )

aoc_part <- 1
for( row in 1:nrow( moves ))
{
  move <- moves[row,]
  
  crate_list            <- tail( stacks[[move$from]], move$ncrates )
  stacks[[ move$from ]] <- head( stacks[[move$from]], -move$ncrates)
  
  if( aoc_part ==1 )
    crate_list <- rev( crate_list )

  stacks[[ move$to ]] <- c( stacks[[ move$to ]], crate_list )
}

stacks |> map_chr(last) |> paste( collapse="")
