#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 9, year=2022) |> head(-1) 
test1 <-c( 
"R 4",
"U 4",
"L 3",
"D 1",
"R 4",
"D 1",
"L 5",
"R 2"
)

test2 <- c(
"R 5",
"U 8",
"L 8",
"D 3",
"R 17",
"D 10",
"L 25",
"U 20"
)

input <- raw

moves <- str_split( input, " ") |>
  map( ~rep( .[1], strtoi( .[2]))) |>
  reduce( c)

##
## Part 1
##

U = c( 0, 1 )
D = c( 0, -1 )
L = c( -1, 0 )
R = c( 1, 0 )

head = c(1,1)
tail = c(1,1)
visited = list( tail )

for( move in moves )
{
  head  <- head  + switch( move, U=U, D=D, L=L, R=R)
  cat( move, head, " ")
  
  if( abs( head[1]-tail[1])<=1 && abs( head[2]-tail[2])<=1)
    next
  
  tail <- tail + c( sign( head[1]-tail[1] ), sign( head[2]-tail[2]))
  visited <- c( visited, list( tail ))
  
  cat( tail, "\n")
}

visited
length( unique(visited)) # part1

##
## Part 2
##

rope <- list()
for( i in 1:10)
  rope <- c( rope, list( c(1,1)))

visited <- list( last(rope))

for( move in moves )
{
  rope[[1]]  <- rope[[1]]  + switch( move, U=U, D=D, L=L, R=R)
  
  for( link in 2:length( rope ))
  {
    dx = rope[[link-1]][1]-rope[[link]][1]
    dy = rope[[link-1]][2]-rope[[link]][2]

    if( abs( dx )<=1 && abs( dy )<=1)
      break
    
    rope[[link]] <- rope[[link]] + c( sign( dx ), sign( dy ))
  }
  visited <- c( visited,  list( last( rope ) ) )
}

visited
length( unique(visited)) # part2
