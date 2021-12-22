library( tidyverse)
library( lubridate)
library( roll)
p1 <- 6
p2 <- 8

#test
# p1 <- 4
# p2 <- 8


rolls <- seq(2,5000, by=3)*3
p1_rolls <- rolls[ seq(1, to=length( rolls), by=2)]
p2_rolls <- rolls[ seq(2, to=length( rolls), by=2)]

p1_pos <- (p1+cumsum(p1_rolls)) %%10
p1_pos[ p1_pos ==0] <- 10
p1_scores <- cumsum( p1_pos )
p1_win <- first(which(p1_scores>=1000))

p2_pos <- (p2+cumsum(p2_rolls)) %%10
p2_pos[ p2_pos ==0] <- 10
p2_scores <- cumsum( p2_pos)
p2_win <-first(which(p2_scores>=1000))
answer1 <- (3*(2*p1_win-1))*p2_scores[p1_win-1]

board_states <- expand_grid( p1 = 1:10, p2=1:10 )

# Player 1 in win zone, Player 2 is not
dirac <- expand_grid( board_states, s1=22:31, s2=1:21) %>%
  mutate( win1 = 1, win2 = 0 )

# Player 2 in win zone, Player 1 is not
dirac <- expand_grid( board_states, s1=1:21, s2= 22:31) %>%
  mutate( win1 = 0, win2 = 1 ) %>%
  bind_rows( dirac )

# Both players in win zone: player 1 wins by first mover
dirac <- expand_grid( board_states, s1=22:31, s2= 22:31) %>%
  mutate( win1 = 1, win2 = 0 ) %>%
  bind_rows( dirac )


board_transition<- expand_grid( board_states, r1=1:3, r2=1:3, r3=1:3, r4=1:3,r5=1:3,r6=1:3) %>%
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

score_states <- expand_grid( s1=1:21, s2=1:21) 

for( tranche in 21:1)
{
  cat( tranche, "\n")
  
  dirac <- score_states %>% filter( pmin(s1, s2) == tranche) %>%
    expand_grid( board_states) %>% 
    left_join( board_transition, by = c("p1","p2") ) %>%
    mutate( s1_new = s1+p1_new, s2_new = s2+p2_new ) %>%
    left_join( dirac, by = c( p1_new="p1", p2_new="p2", s1_new="s1", s2_new = "s2")) %>%
    group_by( p1, p2, s1, s2) %>%
    summarize( win1 = sum( weight * win1), win2 = sum( weight*win2)) %>%
    ungroup %>%
    bind_rows( dirac )
}


win1 = 444356092776315 
win2 = 341960390180808 
win <- filter( dirac, p1==!!p1, p2==!!p2, s1==1, s2==1)
sprintf( "player 1 wins %.100g  player 2 wins %.100g",round(win$win1/27), win$win2   ) 
#If player1 wins, then player2 doesn't roll, so divide by 27 universes

