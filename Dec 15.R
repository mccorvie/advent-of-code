library( tidyverse)

library(crayon)
dir <- "~/Desktop/Advent-Code-2021/Dec 15"
ff  <- "input"

part2 <- F
input_raw <- str_split( readLines( file.path( dir, ff)), "", simplify=T)
dim=nrow( input_raw)
risk <- matrix( strtoi( input_raw), nrow = dim )

if( part2)
{
  risk0 <- risk
  risk <- matrix(0,nrow=dim*5, ncol=dim*5)
  for( roffset in 0:4)
    for( coffset in 0:4)
      risk[ (1:dim + roffset*dim),(1:dim+coffset*dim)] = risk0 + roffset + coffset
  
  risk[ risk>9] = risk[risk>9]-9
  if( any(risk>9))
    stop( "too big")
  
  dim <- dim*5    
}
risk[1,1] <- 0 # the risk of ths element doesn't matter for the total cost

min_path <- matrix( Inf, nrow=dim, ncol=dim)
min_path[dim,dim] = risk[dim,dim]
path_dir = matrix( "X", ncol=dim, nrow = dim)

neighbor_dir <- c( "V", "^", ">", "<", "X")
neighbors    <- matrix( c(1,-1,0,0,0,0,1,-1), ncol=2)
converged <- FALSE

while( !converged )
{
  cat( "\nlowest risk", min_path[1,1], "\n")
  converged <- TRUE

  
  for( rr in dim:1)
    for( cc in dim:1)
    {
      loc <- c(rr,cc)
      coords <- matrix(rep( loc, 4), ncol=2, byrow=T) + neighbors
      valid_coords <- apply( coords >=1 & coords <=dim, 1, all)
      neighbor_dir0 <- neighbor_dir[ valid_coords]
      coords <- coords[valid_coords,]

      if( min( min_path[ coords ] ) + risk[rr, cc] < min_path[ rr, cc] )
      {
        converged <- FALSE
        min_path[ rr, cc ] <- risk[rr,cc] + min( min_path[ coords ] ) 
        best_neighbor <- first( which( min( min_path[ coords ] )  == min_path[ coords ]))
        if( rr==1 && cc==1)
          cat( "(", rr, cc, ")", path_dir[ rr, cc ], " -> ", neighbor_dir0[best_neighbor], "|")
        path_dir[ rr, cc ] <- neighbor_dir0[ best_neighbor]
      }
    }
}

in_path <- matrix( F, nrow=dim, ncol=dim)
rr<-cc<-1
while( rr < dim || cc < dim)
{
  in_path[ rr, cc]<-T
  if(  path_dir[ rr, cc ] == ">")
    cc <- cc+1
  else if(  path_dir[ rr, cc ] == "<" )
    cc <- cc-1
  else if(  path_dir[ rr, cc ] == "^" )
    rr <- rr-1
  else if(  path_dir[ rr, cc ] == "V" )
    rr<-rr+1
  else
    stop( paste("wtf", rr, cc))
}


in_path
answer <- min_path[1,1]

for( rr in 1:dim)
{
  for( cc in 1:dim)
  {
    if( in_path[rr,cc])
      cat( red( path_dir[ rr,cc]))
    else
      cat( path_dir[ rr,cc])
    
  }
  cat( "\n")  
}
    
#cat( paste( path_dir[rr,], collapse="" ), "\n")
