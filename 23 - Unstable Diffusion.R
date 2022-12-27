#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day = 23
# adventr requests are being blocked
#raw  <- read_advent(day = day, year=2022) |> head(-1)
raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

test <- c(
  ".....",
  "..##.",
  "..#..",
  ".....",
  "..##.",
  "....."
)



use_test = F
input = if( use_test ) test else raw

print_pos <- \(pos1)
{
  pos1<-pos
  mincol = min( pos1$col )-1
  minrow = min( pos1$row )-1
  
  plot <- pos1 |> mutate( row = row - min(row)+1, col = col - min(col)+1)
  dots <- expand_grid( row=1:max( plot$row), col=1:max( plot$col)) |> 
    anti_join( plot ) |> 
    mutate( tile = ".")
  plot <- plot |> 
    mutate( tile = "#") |> 
    bind_rows( dots ) |> 
    arrange( row, col )
  
  for( rr in 1:max(plot$row))
    ll <- plot |> filter( row==rr ) |> pull( tile ) |> paste( collapse="") |> cat( "\n")
  
  cat( "\n")
}

pos0 <- str_split( input, "") 
pos  <- map_dfr( 1:length( pos0 ), \(row) tibble( row = row,  tile = pos0[[row]] ) |> mutate( col = row_number() ) ) |> 
  filter( tile == "#") |> 
  select( -tile )

row_offsets  = list( -1, 1, c( -1,0,1), c(-1,0,1))
col_offsets  = list(  c( -1,0,1), c(-1,0,1), -1, 1 )
get_move = \(mm ) if( length( mm )==1 ) mm else 0

tt<-0
repeat
{
  tt<-tt+1
  # if( tt > 10 ) break # part 1
  
  proposed <- NULL
  for( dir in 1:4 )
  {
    blocked <- expand_grid( pos, dcol = col_offsets[[dir]], drow = row_offsets[[dir]] ) |> 
      mutate( nrow = row + drow, ncol = col + dcol )  |> 
      inner_join( pos, by = c( "nrow" = "row", "ncol" = "col")) |> 
      select( row, col ) 
    
    proposed <- pos |>  
      anti_join( blocked, by = c( "row", "col") ) |> 
      mutate( nrow = row + get_move( row_offsets[[dir]] ), ncol = col + get_move( col_offsets[[dir]] )) |> 
      bind_rows( proposed )
  }
  
  row_offsets <- c( tail( row_offsets,-1), head( row_offsets,1 ))
  col_offsets <- c( tail( col_offsets,-1), head( col_offsets,1 ))

  # if you have 4 proposed moves, don't move, otherwise take the first move found
  pos1 <- proposed |> 
    group_by( row, col ) |>
    filter( n()<4) |> 
    summarize( across( everything(), ~ last(.)), .groups = "drop") 
  
  if( nrow( pos1 ) == 0) break # part 2
  
  if( tt%%10 == 0)
    cat( tt, " ", nrow( pos), " ", nrow( pos1), "\n")
  
  # eliminate proposed moves which conflict, add in default (no move), select move
  pos <- pos1 |> 
    group_by( nrow, ncol ) |> 
    filter( n()==1) |> 
    bind_rows( mutate( pos, nrow=row, ncol=col )) |> 
    group_by( row, col ) |> 
    summarize( across( everything(), first ), .groups = "drop") |> 
    select( row=nrow, col=ncol)
}
# part 1
(max(pos$col)-min(pos$col)+1) * (max(pos$row)-min( pos$row)+1) - nrow( pos)
# part 2
tt




