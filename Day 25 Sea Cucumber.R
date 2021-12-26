dir <- "~/Desktop/Advent-Code-2021/Dec 25"
ff <- "input"
#ff <- "test"


next_floor <- sea_floor <- str_split( readLines(file.path( dir, ff )), "", simplify=T)
cat( map_chr(1:nrow( sea_floor), ~ paste0( c(sea_floor[.,], "\n"), collapse="")), "\n", sep="")

steps <- 0
while( steps==0 || moving )
{
  if( steps%%100 == 0)
    cat( steps, " ")
  moving<-F
  for( rr in 1:nrow( sea_floor))
    for( cc in 1:ncol( sea_floor ))
    {
      cc0 <- ifelse( cc-1 > 0, cc-1, ncol(sea_floor))
      if( sea_floor[rr,cc]=="." && sea_floor[rr,cc0] == ">" )
      {
        moving<-T
        next_floor[ rr,cc0] = "."
        next_floor[ rr,cc] = ">"
      }
    }
  sea_floor <- next_floor
  #cat( map_chr(1:nrow( sea_floor), ~ paste0( c(sea_floor[.,], "\n"), collapse="")), "\n", sep="")
  
  for( rr in 1:nrow( sea_floor))
    for( cc in 1:ncol( sea_floor ))
    {
      rr0 <- ifelse( rr-1 > 0, rr-1, nrow(sea_floor))
      if( sea_floor[rr,cc]=="." && sea_floor[rr0,cc] == "v" )
      {
        moving<-T
        next_floor[ rr0,cc] = "."
        next_floor[ rr,cc] = "v"
      }
    }
  sea_floor <- next_floor
  #cat( map_chr(1:nrow( sea_floor), ~ paste0( c(sea_floor[.,], "\n"), collapse="")), "\n", sep="")
  
  steps <- steps+1
}

#sea_floor
steps
