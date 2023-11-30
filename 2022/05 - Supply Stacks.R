
library( tidyverse )
library( adventr)

raw <- read_advent(day = 5, year = 2022 ) |> head(-1)

stacks <- map( seq( from = 2, by  =4, length.out=9), ~ str_sub( raw[1:8], .,.) ) |>
  map( ~ rev( .[.!= " "])) 

moves  <- tibble( raw = raw[11:length( raw)]) |>
  extract( raw, c("ncrates", "from", "to"), "move (\\d+) from (\\d+) to (\\d+)", convert = T )

for( row in 1:nrow( moves ))
{
  move <- moves[row,]
  
  crate_list            <- tail( stacks[[move$from]], move$ncrates )
  stacks[[ move$from ]] <- head( stacks[[move$from]], -move$ncrates)
  
  stacks[[ move$to ]] <- c( stacks[[ move$to ]], rev( crate_list )) # part 1
  #stacks[[ move$to ]] <- c( stacks[[ move$to ]], crate_list ) # part 2
}

stacks |> map_chr(last) |> paste( collapse="")
