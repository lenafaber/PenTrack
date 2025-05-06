# penguin tracks 

library(xml2)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load XML file
xml_file <- read_xml("data/annotations_v3.xml") 

# Extract all tracks
tracks <- xml_find_all(xml_file, ".//track")

# Function to extract points from a single track
extract_points <- function(track) {
  id <- xml_attr(track, "id")  # Get track ID
  label <- xml_attr(track, "label")  # Get label (optional)
  points_nodes <- xml_find_all(track, ".//points")
  
  data <- tibble(
    track_id = id,
    frame = as.integer(xml_attr(points_nodes, "frame")),
    points = xml_attr(points_nodes, "points"),
    label = label
  ) %>%
    tidyr::separate(points, into = c("x", "y"), sep = ",", convert = TRUE)
  
  return(data)
}

# Apply extraction function to all tracks
track_data <- map_dfr(tracks, extract_points)

# Optional: create a readable 'penguin' label
track_data <- track_data %>%
  mutate(
    penguin = paste("Penguin", as.integer(track_id)),
    x = as.numeric(x),
    y = as.numeric(y)
  )

# Plot the tracks using ggplot
track_data |> 
  filter(frame %in% 0:300) |>  
  ggplot(aes(x = x, y = y, color = penguin, group = penguin)) +
  geom_path(size = 1) +  # Draws the path of each penguin
  geom_point(size = 2, alpha = 0.6) +  # Adds points for each frame
  scale_color_viridis_d() +  
  theme_minimal() +
  labs(title = "Penguin Tracking (First 500 Frames)", x = "X Coordinate", y = "Y Coordinate", color = "Penguin") +
  coord_fixed()

#plot movement over time
ggplot(track_data, aes(x = frame, y = y, color = penguin, group = penguin)) +
  geom_path(size = 1) +  # Draws the path of each penguin
  geom_point(size = 2, alpha = 0.6) +  # Adds points for each frame
  scale_x_continuous(limits = c(0,300)) +
  scale_color_viridis_d() + 
  theme_minimal() +
  labs(title = "Penguin Tracking", x = "frame", y = "delta X", color = "Penguin") 

#animation? 
library(gganimate)
#sim plotting
gganim <- track_data |> 
  filter(frame <= 400) |>  # Limit to first 500 frames (or remove this if you want all)
  ggplot(aes(x = x, y = y, color = penguin, group = penguin)) +
  geom_path(size = 1, alpha = 0.3) +         # Trail up to current frame
  geom_point(size = 3) +                     # Penguin at current frame
  scale_color_viridis_d() + 
  scale_y_reverse() + 
  theme_minimal() +
  labs(
    title = "Penguin Tracking Simulation",
    subtitle = "Frame: {frame_along}", 
    x = "X Coordinate", y = "Y Coordinate", color = "Penguin"
  ) +
  coord_fixed() +
  transition_reveal(frame)  # This reveals paths + points over time

#save as GIF
anim_save("penguin_tracking_2.gif", gganim, renderer = gifski_renderer())

#save track data
write.csv(track_data, 
          file = "~/Documents/CU_Boulder/Courses/BioMultiAgent/PenProj/data/tracks_2.csv",
          quote = F,
          row.names = F)
