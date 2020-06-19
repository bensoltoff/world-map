# source: https://arthurwelle.github.io/RayshaderWalkthrough/index.html

library(tidyverse)
library(png) # image handler
library(viridis) # for color pallete
library(scales) # scales
library(magick) # making gif
library(rayshader) # for all the fun
library(here)

# read image
img <- png::readPNG(here("data", "mapa LEGO 50 X 50.png"))

# create the matrix for the whole map
world.matrix <- matrix(data = NA, nrow = 84, ncol = 192)

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
world.matrix[world.matrix == "5013173"] <- 3 # for light greeb
world.matrix[world.matrix == "253256252"] <- 3 # for white, for now, white would be light green
world.matrix[world.matrix == "16518869"] <- 4 # for dark green
world.matrix[world.matrix == "24420641"] <- 5 # yellow
world.matrix[world.matrix == "16310053"] <- 6 # blow light
world.matrix[world.matrix == "1056439"] <- 7 # brown

# tranform in a data frame, and take last row line and last column line out (they were gray... I dont know, lol).
d <- as.data.frame(world.matrix[1:83, 1:191])

# names in the colums
colnames(d) <- c(paste(rep("V", 190), 100:289, sep = ""))

# add a column with row names, now stating in 11
d$Row <- c(paste(rep("R", 83), 182:100, sep = ""))

# gather
d <- tidyr::gather(d, "Hight", "Value", 1:190)
d$Value <- as.numeric(d$Value)

# create graph
g <- ggplot2::ggplot(d, aes(y = Row, x = Hight, fill = Value)) +
  geom_tile(color = "gray") +
  coord_equal() + # set each tile to be squared, not rectangular
  labs(x = NULL, y = NULL) + # take out the axis legends
  scale_fill_gradientn(
    colours = c(
      "#6C98C9",
      "#0A69AE",
      "#A5BC45",
      "#328349",
      "#F4CE29",
      "#A36435",
      "#694027"
    ),
    values = rescale(c(1, 2, 3, 4, 5, 6, 7)),
    guide = "colorbar"
  )

# Theme, margins manually edited
g1 <- g + theme(
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  legend.position = "none"
)
g1

# save output
ggsave(filename = here("output", "lego-map.svg"), plot = g1, width = 10, height = 4.41, units = "in")
ggsave(filename = here("output", "lego-map.png"), plot = g1, width = 10, height = 4.41, units = "in")

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




