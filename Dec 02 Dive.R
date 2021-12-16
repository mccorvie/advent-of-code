

library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 02"
ff <- "input"

input_file <- read_csv( file.path( dir, ff), col_names= "command" ) %>%
  mutate( idx = 1:1000 )

submarine_path <- input_file %>%
  mutate( 
    command_direction = str_split( command, " ", simplify=T )[,1],
    command_magnitude = as.numeric(str_split( command, " ", simplify=T )[,2]),
    x_direction = recode( command_direction, "forward" = 1, "backward" = -1,.default = 0),
    y_direction = recode( command_direction, "up" = -1, "down" = 1, .default =0),
    x_magnitude = command_magnitude * x_direction,
    y_magnitude = command_magnitude * y_direction,
    x_pos = cumsum( x_magnitude),
    y_pos = cumsum( y_magnitude),
    answer = y_pos * x_pos
  )

last( submarine_path$answer)

ggplot( submarine_path, aes(x=x_pos, y=y_pos)) +
  geom_path( color = "purple") + 
  theme_minimal()

unique(submarine_path$command_direction)

submarine_path <- input_file %>%
  mutate( 
    command_direction = str_split( command, " ", simplify=T )[,1],
    command_magnitude = as.numeric(str_split( command, " ", simplify=T )[,2]),
    is_forward = command_direction == "forward",
    aim_direction = recode( command_direction, "up" = -1, "down" = 1, .default =0),
    aim_change = aim_direction * command_magnitude,
    aim = cumsum( aim_change ),
    depth_change = if_else( is_forward, aim* command_magnitude, 0 ),
    x_pos = cumsum( is_forward * command_magnitude ),
    depth = cumsum( depth_change),
    answer = x_pos * depth
  )

last( submarine_path$answer)

ggplot( submarine_path, aes(x=x_pos, y=depth)) +
  geom_path( color = "darkred") + 
  theme_minimal()


ggplot( submarine_path, aes(x=idx, y=aim)) +
  geom_path( color = "darkred") + 
  theme_minimal()


