#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 9
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

evens <- \(v) v[ seq(from = 2,to=length(v),by=2)]
odds  <- \(v) v[seq(to=length(v),by=2)]

blocks <- str_split_1( input, "") |> as.numeric()
files  <- odds( blocks )
space  <- evens( blocks )

sum <- 0
block <- 0
file_front <- 1
file_back  <- length( files)

checksum <- \( block_start, block_len, fileidx)
{
  if( block_len==0)
    cat( "-->", fileidx, "\n")
  ss <- sum( (block_start:(block_start+block_len-1))* (fileidx-1) )
  cat( "(",block_start, "->", block_start+block_len-1, ") *", fileidx-1, "=", ss, "\n")
  ss  
}

checksum_fwd <- \( block_len, fileidx)
{
  ss <- checksum( block, block_len, fileidx)
  block <<- block + block_len
  ss
}


while( file_front < file_back )
{
  sum   <- sum + checksum_fwd( files[file_front], file_front )

  while( files[file_back] <= space[file_front])
  {
    sum   <- sum + checksum_fwd( files[file_back], file_back )

    space[file_front] <- space[file_front] - files[file_back]
    file_back <- file_back-1
  }

  if( (file_front < file_back) && space[file_front] > 0 )
  {
    sum   <- sum + checksum_fwd( space[file_front], file_back )

    files[file_back]  <- files[file_back] - space[file_front]
    space[file_front] <- 0
  }
  file_front <- file_front+1
  
}

if( file_front == file_back )
  sum <- sum + checksum_fwd( files[file_back], file_back )

sum

sum <- 0
blocks <- str_split_1( input, "") |> as.numeric()
files  <- odds( blocks )
space  <- evens( blocks )
block_start <- c(0, cumsum( blocks))
files_start  <- odds( block_start )
space_start  <- evens( block_start )

for( file in length(files):1 )
{
  moveto <- which( space >= files[file ] ) |> first()
  if( is.na( moveto) || moveto >= file )
  {
    sum <- sum + checksum( files_start[file], files[file], file )  
    next
  }  
  
  sum <- sum + checksum( space_start[moveto], files[file ], file )
  space_start[moveto] <- space_start[moveto] + files[file]
  space[ moveto]      <- space[moveto] - files[file]
}

sum
