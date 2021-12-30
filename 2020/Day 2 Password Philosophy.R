library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day02"
rg<- regex( "^(\\d+)-(\\d+) ([a-z]+): ([a-z]+)$")
input <- str_match( readLines( file.path( dir, ff)), rg )[,2:5]
colnames( input )<- c("lower", "upper", "letter", "password")
answer1 <- as_tibble( input)%>%
  mutate( 
    upper = strtoi( upper), 
    lower=strtoi(lower), 
    tt = sapply( str_split( cnts$password, ""), table ),
    nn = map2_dbl( tt, cnts$letter, ~ .x[.y] ),
    valid = lower <= nn & nn <= upper
  )%>%
  filter( valid ) %>%
  count()

answer1


answer2 <- as_tibble( input)%>%
  mutate( 
    upper = strtoi( upper), 
    lower=strtoi(lower), 
    let1 = str_sub( password, lower, lower),
    let2 = str_sub( password, upper, upper),
    both = let1 == letter & let2 == letter,
    any = let1 == letter | let2 == letter,
    valid = any & !both
  ) %>% 
  filter( valid) %>%
  count

answer2   
