library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 04"
ff <- "input"

parse_board <- function( rows )
{
  if( length( rows) != 5)
    stop( "Wtf")
  out <-NULL
  rowsplit = strsplit( rows, " ")

  for( idx in 1:length(rowsplit) )
  {
    rr <- rowsplit[[idx]]
    rr = rr[ rr != "" ]
    out <- rbind( out, strtoi(rr ))
  }
  out
}

board <- matrix( 1:25, nrow=5, ncol=5)
winner <- function( board, caller = NULL)
{
  for( rr in 1:nrow(board))
  {
    if( length( setdiff(board[rr,], caller))== 0)
        return( T )
  }
  
  for( cc in 1:ncol(board))
  {
    if( length( setdiff(board[,cc], caller)) == 0)
      return( T )
  }
  return(F)
}


raw_file <- read_file( file.path( dir, ff))

lines <- strsplit( raw_file, "\n")[[1]]
all_caller  <- strtoi(str_split( lines[1],",")[[1]])

board_start <- seq( 3, length(lines), by=6)
num_boards  <- length( board_start)
boards <- vector( "list", num_boards)

for( idx in 1:num_boards)
{
  bs <- board_start[idx]
  be <- bs + 4
  boards[[idx ]] <- parse_board( lines[bs:be])
}

boards

first_winner <- NULL
last_winner  <- NULL

winners <- rep(F, length(boards))

for( call_num in 1:length( all_caller ))
{
  caller <- head( all_caller, call_num)
  prev_winners <- winners
  winners      <- sapply( boards, winner, caller=caller)

  if( sum(winners ) > 0 && is.null(first_winner))
  {
    first_winner <- which( winners )
    first_win_time <- call_num
  }
  
  if( sum(winners)==length( boards) && is.null( last_winner))
  {
    last_winner <- which(winners & !prev_winners)
    last_win_time <- call_num
  }
  
  cat( call_num, sum(winners), "\n" )
}

first_win_time
last_win_time

caller<- head(all_caller, first_win_time)
score <- sum(setdiff(as.vector(boards[[first_winner]]), caller)) * last(caller)
score

caller<- head(all_caller, last_win_time)
score <- sum(setdiff(as.vector(boards[[last_winner]]), caller)) * last(caller)
score


