library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input02"

# input_raw <- read_delim( file.path( dir, ff) ) 
rps0 <- readLines( file.path( dir, ff)) %>%
  str_split( " ", simplify = T) 

rps1 <-  tibble( elf = rps0[,1], me0 = rps0[,2]) %>%
  mutate( 
    me = case_when(
      me0 == "X" ~ "A",
      me0 == "Y" ~ "B",
      me0 == "Z" ~ "C"
    ),
    shape_score = case_when(
      me == "A" ~ 1,
      me == "B" ~ 2,
      me == "C" ~ 3
    ),
    outcome = case_when(
      me == elf ~ "draw",
      elf == "A" & me == "B"~ "win",
      elf == "B" & me == "C"~ "win",
      elf == "C" & me == "A"~ "win",
      T ~ "lose"
    ),
    outcome_score = case_when(
      outcome == "lose" ~ 0,
      outcome == "draw" ~ 3,
      outcome == "win" ~ 6
    ),
    score = outcome_score + shape_score
  )

sol1 <- summarize( rps1, sum( score))


win = tribble(
  ~elf, ~win,
  "A",  "B",
  "B",  "C",
  "C",  "A"
)

lose = tribble(
  ~elf, ~lose,
  "A",  "C",
  "B",  "A",
  "C",  "B"
)


rps2 <-  tibble( elf = rps0[,1], outcome0 = rps0[,2]) %>%
  mutate(
    outcome = case_when(
      outcome0 == "X" ~ "lose",
      outcome0 == "Y" ~ "draw",
      outcome0 == "Z" ~ "win"
    ),
    outcome_score = case_when(
      outcome == "lose" ~ 0,
      outcome == "draw" ~ 3,
      outcome == "win" ~ 6
    ),
  ) %>%
  left_join(lose) %>%
  mutate( draw = elf ) %>%
  left_join(win) %>%
  mutate(
    me = case_when(
      outcome == "win" ~ win,
      outcome == "draw" ~draw,
      outcome == "lose" ~lose
    ),
    shape_score = case_when(
      me == "A" ~ 1,
      me == "B" ~ 2,
      me == "C" ~ 3
    ),
    score = outcome_score + shape_score
  )

sol2 <- summarize( rps2, sum( score))

##
##  Slicker approach
##

rps0 <- read_delim( file.path( dir, ff), col_names = c( "elf", "strategy"))

win_chart <- tribble(
  ~elf,  ~lose, ~draw,  ~win, 
  "A",   "C",   "A",   "B",   
  "B",   "A",   "B",   "C",   
  "C",   "B",   "C",   "A"
)

rps1 <- rps0 %>%
  left_join( win_chart ) %>%
  mutate(
    me = case_when(
      strategy == "X" ~ "A",
      strategy == "Y" ~ "B",
      strategy == "Z" ~ "C"
    ),
    shape_score = case_when(
      me == "A" ~ 1,
      me == "B" ~ 2,
      me == "C" ~ 3
    ),
    outcome_score = case_when(
      me == win ~ 6,
      me == draw ~ 3,
      me == lose ~ 0
    ),
    score = outcome_score + shape_score
  )

sol1 <- summarize( rps1, sum( score))
sol1


rps2 <- rps0 %>%
  left_join( win_chart ) %>%
  mutate(
    me = case_when(
      strategy == "X" ~ lose,
      strategy == "Y" ~ draw,
      strategy == "Z" ~ win
    ),
    shape_score = case_when(
      me == "A" ~ 1,
      me == "B" ~ 2,
      me == "C" ~ 3
    ),
    outcome_score = case_when(
      strategy == "X" ~ 0,
      strategy == "Y" ~ 3,
      strategy == "Z" ~ 6
    ),
    score = outcome_score + shape_score
  )

sol2 <- summarize( rps2, sum( score))
sol2

