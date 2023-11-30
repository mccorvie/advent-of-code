library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input02"

##
##  Actual code used ------
##

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
sol1

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
##  Cleaned up and abstracted ------
##

win_chart <- tribble(
  ~elf,  ~lose, ~draw,  ~win, 
  "A",   "C",   "A",   "B",   
  "B",   "A",   "B",   "C",   
  "C",   "B",   "C",   "A"
)

read_delim( file.path( dir, ff), col_names = c( "elf", "strategy")) |>
  left_join( win_chart ) |>
  mutate(
    part1_me = case_when(
      strategy == "X" ~ "A",
      strategy == "Y" ~ "B",
      strategy == "Z" ~ "C"
    ),
    part2_me = case_when(
      strategy == "X" ~ lose,
      strategy == "Y" ~ draw,
      strategy == "Z" ~ win
    ),
    me = part1_me, # change for part 1 vs part 2
    shape_score = case_when(
      me == "A" ~ 1,
      me == "B" ~ 2,
      me == "C" ~ 3
    ),
    outcome_score = case_when(
      me == win  ~ 6,
      me == draw ~ 3,
      me == lose ~ 0
    ),
    score = outcome_score + shape_score
  ) |>
  summarize( sum( score))



##
## Using a matrix where the moves are row/col names ------
## Emil Hvitfeld
##

input <- readLines( file.path( dir, ff))


dict <- c(A = "rock", B = "paper", C = "scissors",
          X = "lose", Y = "tie", Z = "win")

you <- dict[substr(input, 1, 1)]
me <- dict[substr(input, 3, 3)]


mat <- matrix(
  c("scissors", "rock", "paper",
    "paper", "scissors", "rock",
    "rock", "paper", "scissors"),
  byrow = TRUE, ncol = 3,
  dimnames = list(c("lose", "win", "tie"),
                  c("rock", "paper", "scissors"))
)
mat

scores <- c(win = 6, tie = 3, lose = 0, rock = 1, paper = 2, scissors = 3)

sum(scores[mat[cbind(me, you)]]) + sum(scores[me])



##
## Computing the RPS outcome with modular arithmetic -----
## Antione Fabri
##


input <- read.delim( file.path( dir, ff), header=F, sep=" ") |>
  transform( V1 = match( V1, LETTERS), V2 = match( V2,LETTERS)-23)

# part1 
part1 <- sum( with( input, V2 + ((1+V2-V1)%%3)*3 ))

# part2
part2 <- sum(  with( input, ((V1+V2)%%3)+1 +3*(V2-1)))

# "Much too hard for a 2nd day!"



