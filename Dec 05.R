library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 05"
ff <- "input"

file_raw <- read_file( file.path( dir, ff))
lines    <- str_split( file_raw ,"\n", simplify = T)
lines <- lines[-501]

segments <- str_split( lines ,"->", simplify = T)
seg1 <- str_split(segments[,1], ",", simplify=T)
seg2 <- str_split(segments[,2], ",", simplify=T)

seg1

coords<-tibble( 
  x1 = strtoi( str_trim(seg1[,1])), 
  y1 = strtoi( str_trim(seg1[,2])),
  x2 = strtoi( str_trim(seg2[,1])), 
  y2 = strtoi( str_trim(seg2[,2]))
)

max( coords$y1)
any(is.nan(coords$x2))


ccc <- coords %>% filter( x1==x2 | y1==y2)

dd <- 1000
MM <- matrix( 0, nrow = dd, ncol = dd)
inc <- matrix( 0, nrow = dd, ncol = dd)

for( idx in 1:nrow( ccc))
{
  cat( idx, " ")
  
  inc <- inc*0
  inc[  ccc$x1[idx]:ccc$x2[idx] , ccc$y1[idx]:ccc$y2[idx]] <-1
  MM = MM + inc
}


dd <- 1000
MM <- matrix( 0, nrow = dd, ncol = dd)
inc <- matrix( 0, nrow = dd, ncol = dd)

for( idx in 1:nrow( coords))
{
  cat( idx, " ")
  
  x1 <- coords$x1[idx]
  y1 <- coords$y1[idx] 
  x2 <- coords$x2[idx]
  y2 <- coords$y2[idx]
  
  inc <- inc*0
  
  
  if( x1 == x2 || y1 == y2)
  {
    inc[ x1:x2, y1:y2 ] <-1
  }
  else if(  abs(x2-x1) == abs(y2-y1))
  {
    xrange <- x1:x2
    yrange <- y1:y2
    for( jj in 1:length(xrange))
    {
      inc[ xrange[jj], yrange[jj]] <-1
    }
  } 
  else
  {
    stop( paste( "malformed coords", x1, y1, x2, y2 ))
  }
  MM = MM + inc
}

sum( MM > 1)

mmt <- tibble()

mmt <- tibble(
  rr =rep( 1:dd, each=dd),
  cc=rep( 1:dd, times=dd),
  value=as.vector(MM)
)

mmt$value <- factor( mmt$value)
ggplot( mmt, aes( x=rr, y=cc, fill = value)) + 
  geom_raster() +
  scale_fill_brewer( "BuPu") +
  ggtitle( "Advent of Code Day 5") +
  theme_minimal()
