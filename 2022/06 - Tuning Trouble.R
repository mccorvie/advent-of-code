#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 6, year=2022) |> head(-1)
buf <- str_split( raw,"")[[1]]

##
## Part 1
## 

tibble( buf=buf) |>
  mutate( 
    n = 1:length( buf),
    buf1 = lag( buf, 1),
    buf2 =lag( buf, 2),
    buf3 =lag( buf, 3)
  ) |>
  rowwise() |>
  mutate( ll = length( unique( c( buf, buf1, buf2, buf3)))) |>
  filter( ll ==4)
  

##
## Part 2
## 

buf <- read_advent(day = 6, year=2022) |> head(-1) |>
  str_split( raw,"")[[1]]

# could set nn = 4 for part 1
nn = 14

aa <- map( 1:length(buf ), ~ buf[max(.x-nn+1,1):.x]) |>
  map_dbl( ~ length( unique( .))) 

which( aa==nn) |> first()

