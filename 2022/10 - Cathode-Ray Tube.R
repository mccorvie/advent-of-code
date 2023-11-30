#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 10, year=2022) |> head(-1)
test <- readLines( "test10")

input <- raw

add <- map( input, ~ if( . == "noop" )  0 else c( 0, strtoi( str_sub( .,6)))) |>
  unlist()
reg <- cumsum( c( 1, add))

ss <- seq(from=20, to = length( reg ),by=40)
sum( reg[ss] * ss ) # part 1

cursor <- seq_along( reg) %% 40 
cursor[ cursor==0 ] <- 40

display <- ( reg <=cursor & cursor<=reg+2 ) |> ifelse( "#", ".") 

# part 2
map( seq( from=1, to=201, by=40 ), ~ .:(.+39)) |>
  map( ~paste0( display[.], collapse = "")) |>
  walk( ~ cat( ., "\n"))

