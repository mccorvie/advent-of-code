# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 16
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw


grid <-str_split( input, "",simplify = T)
#/ \
#^>v<
inc <- list( "^"= c(-1,0), ">"=c(0,1), "v"=c(1,0), "<"=c(0,-1))
mirror <- matrix(
  c( ">", "^", "<","v",
     "<", "v", ">", "^"),
  ncol=2
)

rownames( mirror ) <-  c("^",">","v","<")
colnames( mirror ) = c("/", "\\")
trace_light <- \(r,c,dir)
{
  
  while(T)
  {
    if( r<1 || c<1 || c>ncol(energized) || r>nrow(energized))
      return()
    
    #cat( r, c, dir, grid[r,c],"\n")
    
    key = paste0( r,"-",c,"-",dir)
    if( !is.null( traced[[key]]))
      return()
    
    traced[[key]] <<-T
    energized[r,c]<<-T
    
    #splitters
    if( grid[r,c]=="|" && (dir==">"||dir=="<"))
    {
      trace_light(r-1,c,"^")
      trace_light(r+1,c,"v")
      return()
    }
    
    if( grid[r,c]=="-" && (dir=="^" || dir=="v"))
    {
      trace_light(r,c-1,"<")
      trace_light(r,c+1,">")
      return()
    }
    
    # mirror
    if( grid[r,c]=="\\" || grid[r,c]=="/")
      dir <- mirror[dir,grid[r,c]]
    
    r<-r+inc[[dir]][1]
    c<-c+inc[[dir]][2]
  }

}

count_energized <- \(r,c,dir)
{
  cat( r,c,dir,"\n")
  traced    <<- list()
  energized <<- matrix( F, nrow=nrow(grid), ncol=ncol(grid))
  trace_light(r,c,dir)
  sum(energized)
}

rr <- 1:nrow(grid) |> map_dbl( \(r) count_energized(r,1,">")) |> max()
ll <- 1:nrow(grid) |> map_dbl( \(r) count_energized(r,ncol(grid),"<")) |> max()
dd <- 1:nrow(grid) |> map_dbl( \(c) count_energized(1,c,"v")) |> max()
uu <- 1:nrow(grid) |> map_dbl( \(c) count_energized(ncol(grid),c,"^")) |> max()
max(uu,dd,ll,rr)
