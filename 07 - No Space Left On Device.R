#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 7, year=2022) |> head(-1)

shell_output = raw
#output= readLines( "test07")

path = NULL
files <- tibble()
all_dirs <- NULL
for( line in shell_output )
{
  if( str_detect( line, "^\\$ cd \\.\\.")) {
    path <- head( path,-1)    
  } else if( str_detect( line, "^\\$ cd")) {
    path <- c( path, str_remove( line, "\\$ cd " ))
    path_str <- paste( path, collapse = "/")
    all_dirs <- c( all_dirs, path_str )
  } else if( str_detect( line, "^\\d")) {
    path_str <- paste( path, collapse = "/")
    entry <- tibble( line = line, path = path_str ) |>
      separate( line, c( "size", "filename" ), sep = " ", convert=T)
    files <- bind_rows( files, entry )
    cat( path_str, line, "\n")
  } 
}

nested_size <- tibble()
for( dir in all_dirs  )
{
  dir_size <- files |>
    filter( str_detect( path, dir )) |>
    summarize( size = sum( size)) |>
    pull( size)
  
  nested_size <- nested_size |>
    bind_rows( tibble( path = dir, size = dir_size ))
}

# part 1
nested_size |> 
  filter( size <= 100000) |>
  summarize( size  = sum( size))

total_used = nested_size |> pull( size ) |> max()
available <- 70000000 - total_used
needed    <- 30000000 - available

# part 2
nested_size |> 
  filter( size >= needed) |>
  summarize(size = min(size)) 


