# calc vectors per frame per penguin 
#packages
library(dplyr)
library(tidyr)

#data
tracks <- read.csv("data/tracks_2.csv")
tracks <- tracks %>% filter(frame <= 400)
tracks$coord <- paste(tracks$x, ",", tracks$y)

#calc vect
tracks_with_orientation <- tracks %>%
  arrange(penguin, frame) %>%  # ensure correct order
  group_by(penguin) %>%
  mutate(
    dx = lead(x) - x,
    dy = lead(y) - y,
    orientation_angle = atan2(dy, dx)* 180 / pi,
  ) %>%
  ungroup()

#calc pairwise distances per penguin
#df that indicates id of 'self'
tracks_self <- tracks_with_orientation %>%
  select(frame, x, y, penguin) %>%
  rename(x_other = x, y_other = y, penguin_other = penguin)

#calc distances
tracks_with_distances <- tracks_with_orientation %>%
  left_join(tracks_self, by = "frame") %>%
  filter(penguin != penguin_other) %>%  # exclude self
  mutate(
    distance = sqrt((x - x_other)^2 + (y - y_other)^2),
    penguin_other_id = gsub("Penguin ", "", penguin_other),  # for column name
    dist_col = paste0("dist_pen_", penguin_other_id)
  ) %>%
  select(frame, penguin, dist_col, distance)

#pivot table distances
distance_wide <- tracks_with_distances %>%
  distinct() %>%
  pivot_wider(
    names_from = dist_col,
    values_from = distance
  )

#join to original table
final_tracks <- tracks_with_orientation %>%
  left_join(distance_wide, by = c("frame", "penguin"))

#save vectors with orientation and distances  
write.csv(final_tracks, 
          file = "~/Documents/CU_Boulder/Courses/BioMultiAgent/PenProj/data/tracks_w_ori_dis.csv",
          quote = F,
          row.names = F)

#plot the penguins as a vector per frame
library(viridis)
library(ggplot2)
library(gganimate)

# Define arrow length
arrow_length <- 10  # Adjust to desired visual scale

# Compute arrow endpoints based on orientation angle
arrow_data <- final_tracks %>%
  filter(frame <= 500) %>%
  mutate(
    xend = x + arrow_length * cos(orientation_angle * pi / 180),
    yend = y + arrow_length * sin(orientation_angle * pi / 180)
  )

# Plot orientation vectors
gganim <- ggplot(arrow_data, aes(x = x, y = y, color = penguin, group = penguin)) +
  geom_segment(aes(xend = xend, yend = yend), arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.2) +
  scale_color_viridis_d() +
  scale_y_reverse() + 
  theme_minimal() +
  labs(
    title = "Penguin Orientation Vectors",
    subtitle = "Frame: {frame}",
    x = "X Coordinate", y = "Y Coordinate", color = "Penguin"
  ) +
  coord_fixed() +
  transition_manual(frame)

# Save as GIF
anim_save("penguin_orientation_vectors.gif", gganim, renderer = gifski_renderer())

