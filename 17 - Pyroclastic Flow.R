#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)
library( lubridate)

raw <- read_advent(day = 17, year=2022) |> head(-1)
test <- ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

input <- raw
input <- test

rocks <- list()
rocks[[1]] <- matrix( T, nrow=1, ncol = 4)
rocks[[2]] <- matrix( F, nrow = 3,  ncol = 3)
rocks[[2]][2,1:3]<-T
rocks[[2]][1:3,2]<-T
rocks[[3]]<- matrix( F, nrow=3, ncol=3)
rocks[[3]][1,1:3]<-T
rocks[[3]][1:3,3]<-T
rocks[[4]] <- matrix( T, nrow=4, ncol=1)
rocks[[5]] <- matrix( T, nrow=2, ncol=2)
rocks


dir <- list( "<" = c(0,-1), ">" = c(0,1))
move_vec <- map_chr(1:str_length(input), ~ str_sub(input,.,.))

print_mm <- \(mm)
{
  map( nrow(mm):1, ~ paste0( if_else( mm[.,], "#" , "."),collapse="")) |> 
    walk( ~ cat( ., "\n"))
  cat( "\n")
}

board_state <- \(mm, h)
{
  map( (h+7):max(h-10,1), ~ paste0( if_else( mm[.,], "#" , "."),collapse="")) |> paste0( collapse = "|")
}


coord_range <- \(c1,rock)
{
  c2 <- c1 + dim( rock ) + c(-1,-1)
  list( c1[1]:c2[1], c1[2]:c2[2] )  
}

collision <-\( cursor,rock, chamber)
{
  cr <- coord_range( cursor, rock )
  #cat( "c", unlist( cr ), "\n")
  any( chamber[cr[[1]], cr[[2]]] & rock )
}

superimpose <- \(cursor, rock,chamber )
{
  
  cr <- coord_range( cursor, rock )
  #cat( "s", unlist( cr ), "\n")
  chamber[cr[[1]],cr[[2]]] <- rock | chamber[cr[[1]],cr[[2]]]
  chamber
}

height_hist[2023]

cwidth <- 9
chamber <- matrix( F, 1500, cwidth )
chamber[1,] <- chamber[,1] <- chamber[,cwidth] <-T
height <- 1
move   <- 0
t1=now()
extrah = 0
dhmax<-0
height_hist <-NULL

board_cache = list()

for( t in 1:10000)
{
  rock   <- rocks[[ (t-1) %% length(rocks)+1]]
  if( t%%1000 == 0)
    cat( t, " ")
  
  if( height > 1000 )
  {
    height = height -500
    extrah = extrah +500
    chamber[1:1000,] = chamber[501:1500,]
    chamber[1001:1020,2:8]<-F
    #cat( "chop\n")
  }
  
  cursor <- c(  height + 4 , 4)
  height
  snap <- superimpose( cursor, rock, chamber ) |> board_state( height ) |> paste0( "-", move )
  board_cache[[snap]] <- c( board_cache[[snap]], t)

  if( length( board_cache[[snap]])==2)
  {
#    height_hist[t+1] <- height + extrah -1
#    break
    
  }
  
  names(board_cache)
  
  dh <- 0
  repeat
  {
    move   <- move%%length(move_vec) +1
    #cat( move, " ", move_vec[move], "\n")
    dh<-dh+1
    if( !collision( cursor + dir[[move_vec[move]]], rock, chamber ))
      cursor <-cursor + dir[[move_vec[move]]]

    if( collision( cursor + c(-1,0), rock, chamber ))
      break
    
    cursor <- cursor + c(-1,0)
    
#    cat( "++++++++++++++++++++++++++\n\n")
  }  
  chamber <- superimpose( cursor, rock,chamber  )
  #print_mm( superimpose(  cursor, rock,chamber  )  )
  dhmax <- max( dh, dhmax )
  
  height  <- apply( chamber[,2:(cwidth-1)], 1, any) |> which() |> max()
  height_hist[t+1] <- height + extrah -1
}



table( board_cache)
loop <- board_cache[[snap]]
loop
pre <- loop[1]
cycle <- loop[2]-loop[1]


calc_height <- function( t )
{
  cycle <- 4826-3101
  pre   <- 159
  
  
  ct <- (t-pre)%/%cycle
  cr <- (t-pre)%%cycle
  
  inc_height <- height_hist - height_hist[pre]
  dh_dcycle  <- height_hist[ cycle + pre] -height_hist[pre]
  
  
  height_hist[pre] + ct * dh_dcycle + inc_height[ cr+pre ]
}

height_hist[2022]

# 1566376811582
tt <- 1000000000001 
tt <- 20000
hh<-calc_height( tt )
print( hh, digits=12)

height_hist[tt]

ct <- (1000000000000-pre)%/%cycle
cr <- (1000000000000-pre)%%cycle
inc_height <- height_hist - height_hist[loop[1]]

hh <-height_hist[pre] + ct* inc_height[loop[2]-1] + inc_height[ cr+pre ]

print( hh, digits=12)
length( inc_height)

input


height_hist[loop]
board_cache
dhmax
t2=now()

height_hist[0]
s1 = 54 
s2 = 1769

cycle <- 4826-3101
pre   <- 159

h1 = 93
h2 = 2773

cycle_len = s2-s1
dh = h2-h1

(1000000000000-s1)%/%cycle

height + extrah -1
length( move_vec)

1000000000000/2000 * 4.97

print_mm(chamber)
height+extrah-1

unname(unlist(board_cache[bc_len==1]))

bc_len <- map_dbl( board_cache, length)
table( bc_len)
bc_len[ bc_len == max(bc_len)]
