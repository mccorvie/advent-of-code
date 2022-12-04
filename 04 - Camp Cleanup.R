library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input04"


input <- read_csv( file.path( dir, ff),col_names = c( "e1", "e2" ))
  
elf1 <- str_split( input$e1, "-", simplify = T ) 
colnames( elf1) <- c( "elf1.l", "elf1.u")
elf2 <- str_split( input$e2, "-", simplify = T )
colnames( elf2) <- c( "elf2.l", "elf2.u")

aa <- bind_cols( elf1, elf2) |>
  mutate( 
    across( starts_with( "elf"), strtoi),
    min_len = pmin( elf1.u-elf1.l, elf2.u-elf2.l ) +1,
    inner.l = pmax( elf1.l, elf2.l),
    inner.u = pmin( elf1.u, elf2.u), 
    inner_len = pmax( 0, inner.u-inner.l+1 )
  ) 

# part1
aa |> summarize( sum( inner_len == min_len))

# part2 
aa |> summarize( sum( inner_len>0))

