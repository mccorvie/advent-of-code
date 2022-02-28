library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day19"
lines <- readLines( file.path( dir, ff))
rg <- regex( "^(\\d+): (\\d+) (\\d+)( \\| (\\d+) (\\d+))*$")
rules_raw <- str_match( lines, rg )
rules <- rbind( rules_raw[,2:4],rules_raw[,c(2,6:7)])
colnames(rules ) <- c( "rule_num", "left_rule", "right_rule" )
rules <- as_tibble( rules) %>% 
  filter( !is.na( rule_num), !is.na(left_rule), !is.na(right_rule))

# annoying singleton rules
rg <- regex( "^(\\d+): (\\d+)( \\| (\\d+))*$")
rules_raw1 <- str_match( lines, rg )
rules1 <- rbind( rules_raw1[,2:3],rules_raw1[,c(2,5)])
colnames(rules1) <- c( "rule_num", "left_rule" )
rules1 <- as_tibble( rules1) %>% 
  filter( !is.na( rule_num), !is.na(left_rule)) %>%
  mutate( right_rule = "X")
rules <- bind_rows( rules, rules1)

rg2 <- regex( '^(\\d+): "([a-z])"$')
rules_resolved <- str_match( lines, rg2 )[,2:3]
colnames(rules_resolved ) <- c( "rule_num", "resolved" )
rules_resolved <- as_tibble( rules_resolved)%>%
  filter( !is.na( rule_num) & !is.na( resolved ))  %>%
  bind_rows( tibble( rule_num = "X", resolved="" ))
rules_resolved

rules_raw
while( nrow( rules) > 0)
{
  joined <- rules %>%
    inner_join( rename( rules_resolved, left_resolved  = resolved), by = c(left_rule  = "rule_num")) %>%
    inner_join( rename( rules_resolved, right_resolved = resolved), by = c(right_rule = "rule_num")) 
  
  if( !nrow( joined))
    stop( "can't resolve any more rules")
  
  rules_resolved <- joined %>%
    mutate( resolved = paste0( left_resolved, right_resolved)) %>%
    select( rule_num, resolved ) %>%
    bind_rows( rules_resolved )
  
  
  rules <- anti_join( rules, joined, by = c( "rule_num", "left_rule", "right_rule"))
  cat( nrow(rules ), nrow( rules_resolved ),"\n")  
}

filter( rules_resolved, rule_num == "0")
messages <- tibble( rule_num = "0", resolved = lines[134:length( lines)])
nn <- inner_join( messages, rules_resolved)
nrow( nn )

rules_resolved %>% nrow
rules_resolved %>% distinct() %>% nrow  


# count( filter( rules_resolved, rule_num == "0"), aa = str_length(resolved))
# count( messages, aa = str_length(resolved))
# 
# 11 + 117 * 2 + 3
# 
# paste0( "|",rules$right_rule,"|")
# lines
