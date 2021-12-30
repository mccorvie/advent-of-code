library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day09"
code <- as.numeric(readLines( file.path( dir, ff)))

buffer <- code[1:25]
for( loc in 26:length( code))
{
  buffer <- code[(loc-25):(loc-1)]
  addends <- intersect( buffer, code[loc] - buffer )
  if( length(addends) < 2)
  {
    answer1 <- code[loc]
    break
  }
  #cat( loc, ")", addends, " : ", length( addends ), "\n")
}
answer1

for( start in 1:length( code))
{
  end <- start
  while( end <= length( code ) && sum(code[start:end])<answer1)
    end<-end+1
  if( sum(code[start:end])== answer1 )
    cat( start, end, max(code[start:end])+min(code[start:end]), sum(code[start:end])== answer1, "\n")
}

