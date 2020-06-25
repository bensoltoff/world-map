# source: https://arthurwelle.github.io/RayshaderWalkthrough/index.html

library(tidyverse)
library(png) # image handler
library(viridis) # for color pallete
library(scales) # scales
# library(magick) # making gif
library(rayshader) # for all the fun
library(here)

theme_set(theme_void())

# read image
img <- png::readPNG(here("data", "mapa LEGO 50 X 50.png"))

# create the matrix for the whole map
world.matrix <- matrix(data = NA, nrow = 96, ncol = 192)

# populate a matrix
for (i in 0:(nrow(world.matrix) - 1)) { # iterates rows
  for (j in 0:(ncol(world.matrix) - 1)) { # iterates columns
    world.matrix[i, j] <- paste((trunc(256 * (img[(i * 50 + 25), (j * 50 + 25), ]))[1]), # R
      (trunc(256 * (img[(i * 50 + 25), (j * 50 + 25), ]))[2]), # G
      (trunc(256 * (img[(i * 50 + 25), (j * 50 + 25), ]))[3]), # B
      sep = ""
    )
  }
}

# colors become numbers
world.matrix[world.matrix == "10105174"] <- 1 # for light blue
world.matrix[world.matrix == "108152201"] <- 2 # for dark blue
world.matrix[world.matrix == "5013173"] <- 3 # for light green
world.matrix[world.matrix == "16518869"] <- 4 # for dark green
world.matrix[world.matrix == "24420641"] <- 5 # yellow
world.matrix[world.matrix == "16310053"] <- 6 # blow light
world.matrix[world.matrix == "1056439"] <- 7 # brown
world.matrix[world.matrix == "253256252"] <- 8 # for white\
world.matrix[world.matrix == "256256256"] <- 8 # white
world.matrix[world.matrix == "1964027"] <- 9 # red

# transform in a data frame, and take last row line and last column line out
d <- as_tibble(world.matrix[1:95, 1:191]) %>%
  # add row numbers
  mutate(row = -row_number()) %>%
  # gather all the columns
  pivot_longer(cols = c(-row),
               names_to = "column",
               values_to = "value",
               names_transform = list(column = parse_number),
               values_transform = list(value = parse_number))

# need to manually fix some of the interpolation
d <- d %>%
  mutate(
    # bottom row should be all white
    value = ifelse(row == min(row), 8, value),
    # get rid of legend and replace with all white
    value = ifelse(row <= -86 & column > 130 & column < 167, 8, value),
    # fix greenland to be ice covered
    value = ifelse(column > 47 & column < 100 & row > -20 & value == 6, 8, value),
    # fix last column white squares which recorded latitude
    value = ifelse(column == max(column) & row > -90 & value == 8, 1, value)
    )

# create graph
g <- ggplot(d, aes(x = column, y = row, fill = value)) +
  geom_tile(color = "gray") +
  # set each tile to be squared, not rectangular
  coord_equal(expand = FALSE, xlim = c(0, 192), ylim = c(-96, 0)) +
  labs(x = NULL, y = NULL) + # take out the axis legends
  scale_fill_gradientn(
    colors = c(
      "#6C98C9",
      "#0A69AE",
      "#A5BC45",
      "#328349",
      "#F4CE29",
      "#A36435",
      "#694027",
      "#FFFFFF",
      "#C4281B"
    ),
    values = rescale(c(1, 2, 3, 4, 5, 6, 7)),
    guide = "colorbar"
  )

# Theme, margins manually edited
g1 <- g + theme(
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  legend.position = "none"
)
g1

# save output
ggsave(filename = here("output", "lego-map.svg"), plot = g1, width = 10, height = 5, units = "in")
ggsave(filename = here("output", "lego-map.png"), plot = g1, width = 10, height = 5, units = "in")

# experiment ray
plot_gg(g1, width = 10, height = 4.41, scale = 30, offset_edges = TRUE)

# africa
render_camera(
  fov = 70,
  zoom = 0.1,
  theta = 58,
  phi = 18
)
render_snapshot(clear = FALSE)
render_depth(focus = 0.72, focallength = 250)




