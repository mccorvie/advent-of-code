library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 06"
ff <- "input"

lines <- readLines( file.path( dir, ff))
lantern_ages0 <- strtoi(str_split( lines, ",", simplify=T))


lantern_ages <- lantern_ages0

for( tt in 1:80)
{
  lantern_ages <- lantern_ages -1
  spawn <- sum( lantern_ages <0)
  lantern_ages <- c(if_else( lantern_ages < 0, 6, lantern_ages ),rep( 8, spawn))
}

length( lantern_ages )


lantern_by_age= tibble( age = 0:8 ) %>%
  left_join( count( lantern_list, age) ) %>%
  mutate( n = coalesce( n, 0))

lantern_by_age <- lantern_by_age$n
spawn_hist <- NULL
for( tt in 1:256)
{
  spawn_num <- lantern_by_age[1]
  spawn_hist <- c(spawn_hist,spawn_num)
  lantern_by_age <- c(lantern_by_age[-1], spawn_num ) 
  lantern_by_age[7] = lantern_by_age[7] + spawn_num
}

answer = sum( lantern_by_age)

ggplot( tibble( time=1:256, `new spawn`=spawn_hist), aes(x=time,y=`new spawn`)) +
  geom_line( color="purple") +
  scale_y_continuous(trans='log10') +
  ggtitle( "Lanternfish", subtitle = "Advent of Code Day 6") +
  theme_minimal()
