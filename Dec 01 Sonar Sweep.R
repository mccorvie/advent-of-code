library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 01"
ff <- "input"

depth_readings <- read_csv( file.path( dir, ff), col_names= "depth" ) %>%
  mutate( idx = 1:2000, diff = depth-lag(depth), deeper = diff >0 ) %>%
  mutate( avg3 = depth + lag( depth ) + lag( depth,2), diff_avg3 = avg3 - lag(avg3), deeper_avg3 =diff_avg3>0 )

ggplot( depth_readings, aes( x=idx,y=depth)) +
  geom_point( size=0.5, color="purple") +
  theme_minimal()

sum( depth_readings$deeper, na.rm=T )

sum( depth_readings$deeper_avg3, na.rm=T )
