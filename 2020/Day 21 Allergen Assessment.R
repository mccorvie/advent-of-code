require( tidyverse)
require( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day21"
lines <- readLines( file.path( dir, ff))

mm <- str_match( lines, "(.*) \\(contains (.*)\\)$")

foreign  <- tibble()
allergens <- tibble()
for( rr in 1:nrow(mm))
{
  foreign <- bind_rows( foreign, tibble(  food = rr, ff = str_split( mm[rr,2], " " )[[1]] ))
  allergens <- bind_rows( allergens, tibble(  food = rr, aa = str_split( mm[rr,3], ", " )[[1]] ))
}

unique( foreign$ff)
foreign_list <- list()
for( foreign_word in unique( foreign$ff))
  foreign_list[[ foreign_word ]] <- filter( foreign, ff==foreign_word) %>% pull( food )


allergen_list <- list()
for( word in unique( allergens$aa))
  allergen_list[[ word ]] <- filter( allergens, aa==word) %>% pull( food )

translate <- list()
while( length( allergen_list ) > 0 )
{
  possible <- matrix( F,nrow=length(foreign_list), ncol=length( allergen_list))
  colnames( possible ) <- names( allergen_list)
  rownames( possible ) <- names( foreign_list)
  
  for( foreign_word in names( foreign_list ))
    for( word in names( allergen_list))
      possible[ foreign_word, word ] <-  length( setdiff(allergen_list[[word]], foreign_list[[foreign_word]] )) == 0
  
  ww <- apply( possible, 2, sum )
  allergen_id = names(ww)[ ww == 1]
  
  translate[ map_chr( allergen_id, ~ names( which(possible[,.])) ) ] = allergen_id
  
  allergen_list[ allergen_id ] <- NULL
  foreign_list[ names( translate )] <- NULL
}

foreign %>% filter( !( ff %in% names( translate ) )) %>% nrow()

                    