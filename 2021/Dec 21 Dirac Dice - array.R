#
#
#  Buggy array implementation of backward induction
#
#


library( tidyverse)
library( lubridate)
library( roll)

# state
# p1, p2, s1, s2, winner
# 1..10, 1..10, 0..31, 0..31, 1..2

#p1,p2,s1,s2,1 = sum( wt * (  p1+3:9 %% 10, p2, s1+p1+3:9%%10, s2, 2 ) )


dirac <- array( NA, dim = c(10,10,31,31,2))

# Player 1 in win zone, Player 2 is not
dirac[,,22:31,1:21,1] <- 1
dirac[,,22:31,1:21,2] <- 0

# Player 2 in win zone, Player 1 is not
dirac[,,1:21,22:31,1] <- 0
dirac[,,1:21,22:31,2] <- 1

# Both players in win zone: player 1 wins by first mover
dirac[,,22:31,22:31,1] <- 1
dirac[,,22:31,22:31,2] <- 0

board_transition <- expand_grid( p1=1:10, p2=1:10, r1=1:3, r2=1:3, r3=1:3, r4=1:3,r5=1:3,r6=1:3) %>%
  mutate( roll1 = r1+r2+r3, roll2 = r4+r5+r6) %>%
  group_by( p1, p2, roll1, roll2 ) %>%
  summarize( weight = n()) %>%
  mutate(
    p1_new = (p1+roll1) %%10,
    p1_new = if_else( p1_new==0, 10, p1_new ),
    p2_new = (p2+roll2) %%10,
    p2_new = if_else( p2_new==0, 10, p2_new )
  ) %>%
  ungroup


tt1<-tibble( a=1:5, b=1:5)
expand_grid(tt, c=1:6)

board_states <- expand_grid( p1 = 1:10, p2=1:10)

score_states <- expand_grid( s1=1:21, s2=1:21) %>%
  mutate( mm = pmin(s1,s2)) %>%
  arrange( -mm, -s1,-s2) %>%
  select( s1, s2 )

for( winner in 1:2)
  for( score_idx in 1:nrow(score_states ))
  {
    score_state <- as_vector( score_states[score_idx,] )
    cat( score_state, "\n")
    for( board_idx in 1:nrow( board_states))
    {
      board_state <- as_vector( board_states[board_idx,])
      bt  <- board_transition %>% filter( p1==board_state["p1"], p2==board_state["p2"] )

      next_state <- bt %>%
          select( p1_new, p2_new ) %>%
          mutate( s1_new = score_state["s1"]+p1_new, s2_new = score_state["s2"] + p2_new) %>%
          as.matrix()

      dirac[ c( board_state, score_state, winner ) ] <- sum( dirac[cbind( next_state, winner)] * bt$weight)
    }
  }

dirac[p1,p2,1,1,1]