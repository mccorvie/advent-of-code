library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day06"

input <- str_split( readLines( file.path( dir, ff)), "" )
input

group_num <- cumsum( map_dbl( input, ~ length(.)==0))
groups <- split( 1:length( input), group_num)

any_answer <- rep(0, max( group_num))
for( group_idx in 1:length( groups))
  any_answer[group_idx] <- length(unique(unlist(input[ unlist(groups[group_idx])])))
answer1 <- sum(any_answer)

all_answer <- rep(0, max( group_num))
for( group_idx in 1:length( groups))
{
  this_group <- keep( input[ unlist(groups[group_idx])], ~ length(.)>0)
  all_answer_list <- this_group[[1]]
  for( idx in 1:length( this_group))
    all_answer_list <- intersect( all_answer_list, this_group[[idx]])
  all_answer[group_idx] <- length( all_answer_list)
}

answer2 <- sum(all_answer)
