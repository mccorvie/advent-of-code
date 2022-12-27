#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day = 25
# adventr requests are being blocked
#raw  <- read_advent(day = day, year=2022) |> head(-1)
raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

digit_value = c( "2"=2,"1"=1,"0"=0,"-"=-1, "="=-2)
val = strsplit( input, "") |> map_dbl( \(xx) sum( 5^((length(xx)-1):0) * map_dbl( xx, \(digit) digit_value[digit]))) |> sum()

xx <- val
rep = NULL
while( xx > 0 )
{
  dig <- (xx+2)%%5 - 2
  rep <- c( dig, rep  )
  xx  <- xx %/% 5
  if( dig < 0)
    xx <- xx+1
  
}

rep |> map_chr( \(x) case_when( x>=0 ~ paste0( x), x==-1 ~ "-", x==-2 ~ "=")) |> paste0( collapse="")


