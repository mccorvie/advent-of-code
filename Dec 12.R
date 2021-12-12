
dir <- "~/Desktop/Advent-Code-2021/Dec 12"
ff  <- "input"
input_raw <- readLines( file.path( dir, ff))

tunnels <- str_split( input_raw, "-", simplify=T)
caves   <- c( "start", setdiff( unique( sort(tunnels )), c("start","end")), "end" )

adj <- matrix( F, nrow=length( caves), ncol=length(caves))
rownames( adj) <- colnames(adj)<- caves
for( rr in 1:nrow( tunnels ))
  adj[ tunnels[rr,1], tunnels[rr,2] ] <- adj[ tunnels[rr,2], tunnels[rr,1] ] <- T

is.lower <- function ( cc) cc == tolower(cc)

all_paths <- list()

part <- 1
#part <- 2

find_paths <- function( cur_path )
{
  cur_cave <- last( cur_path )
  if( cur_cave == "end")
    return( list(cur_path ))
  
  exclude_caves <- cur_path[ is.lower( cur_path )]
  if( part ==2 && all(table( exclude_caves)==1))
    exclude_caves <- c( "start")
  
  next_cave  <- names( which( adj[ cur_cave, ]) )
  next_cave  <- setdiff(next_cave, exclude_caves )
  paths_from_here <- list()
  for( cave in next_cave)
    paths_from_here <- c( paths_from_here, find_paths( c(cur_path, cave)))
  return( paths_from_here )
}
paths <- find_paths( "start")
length(paths)
