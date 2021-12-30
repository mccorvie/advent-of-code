library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day04"
input <- str_split( readLines( file.path( dir, ff)), " " )

expected <- c( "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" )
# "cid" not expected

passport_entries <- map( input, ~ map_chr( ., ~ str_sub(., 1,3)))
valid <- 0
passport_fields <- NULL
for( idx in 1:length(passport_entries ))
{
  if( passport_entries[[idx]][1]!= "")
  {
    passport_fields <- c( passport_fields, passport_entries[[idx]] )
    if( idx < length(passport_entries))
      next
  }
  
  cat( passport_fields, " ||| " )
  if( length( passport_fields ) && !length( setdiff( expected, passport_fields )))
      valid <- valid+1
  else
    cat("missing", setdiff( expected, passport_fields ))

  cat( "\n")  
  passport_fields <- NULL
}
answer1<-valid
answer1
     

passports <- tibble( raw = unlist( input)) %>%
  mutate( input_block = cumsum( raw=="")+1) %>%
  filter( raw!="") %>%
  mutate( field_name = str_sub( raw, 1,3), field_value = str_sub( raw, 5)) %>%
  filter( field_name != "cid") %>%
  mutate(
    field_valid = F,
    field_valid = if_else( field_name == "byr", str_detect( field_value, "^\\d{4}$") & field_value >= "1920" & field_value <= "2002", field_valid ),
    field_valid = if_else( field_name == "iyr", str_detect( field_value, "^\\d{4}$") & field_value >= "2010" & field_value <= "2020", field_valid ),
    field_valid = if_else( field_name == "eyr", str_detect( field_value, "^\\d{4}$") & field_value >= "2020" & field_value <= "2030", field_valid ),
    height = strtoi(str_match(field_value, "\\d+")),
    height_unit = str_match(field_value, "cm|in"),
    height_range_valid = height_unit == "cm" & height >= 150 & height <= 193 | height_unit =="in" &  height >= 59 & height <= 76,
    field_valid = if_else( field_name == "hgt", str_detect( field_value, "^\\d+(cm|in)$" ) & height_range_valid, field_valid ),
    field_valid = if_else( field_name == "hcl", str_detect( field_value, "^\\#([0-9a-f]){6}$" ), field_valid ),
    field_valid = if_else( field_name == "ecl", str_detect( field_value, "^(amb|blu|brn|gry|grn|hzl|oth)$" ), field_valid ),
    field_valid = if_else( field_name == "pid", str_detect( field_value, "^\\d{9}$" ), field_valid )
  ) 

answer2 <- passports %>%
  group_by(input_block)%>%
  summarize( num_valid = sum( field_valid)) %>%
  mutate( valid_passport = num_valid == 7) %>%
  summarize( sum( valid_passport))
answer2
