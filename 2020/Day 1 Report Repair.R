library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day01"
# ff<-"test2"
input <- as.numeric(strtoi(readLines( file.path( dir, ff))))
complement <- 2020-sort( input)
pair <- intersect( input, complement)
answer1 <- pair[1]*pair[2]
answer1

cc<-crossing( a=input, b=input, c=input) %>%
  mutate( ss = a+b+c, pp = a*b*c) %>%
  filter( ss==2020)
answer2 <- first( cc$pp)
answer2
