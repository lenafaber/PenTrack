# calc vectors per frame per penguin 
#packages
library(dplyr)
library(tidyr)
library(zoo)
library(scales)

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

# plot vectors over time
arrow_data |> 
  filter(frame %in% 0:300) |>  
  ggplot(aes(x = x, y = y)) +
  geom_spoke(
    aes(angle = atan2(dy, dx), radius = sqrt(dx^2 + dy^2) * 0.1, color = frame),
    arrow = arrow(length = unit(0.15, "cm")),
    size = 0.7
  ) +
  scale_color_viridis_c() +
  coord_fixed() +
  scale_y_reverse(limits = c(250,125)) +
  xlim(c(250,500)) +
  theme_minimal() +
  labs(title = "", x = "X", y = "Y", color = "Frame")

# calc alignment
alignment_per_frame <- arrow_data %>%
  filter(frame %in% 0:300) %>% 
  group_by(frame) %>%
  summarise(
    mean_dx = mean(dx / sqrt(dx^2 + dy^2), na.rm = TRUE),
    mean_dy = mean(dy / sqrt(dx^2 + dy^2), na.rm = TRUE),
    alignment = sqrt(mean_dx^2 + mean_dy^2),
    n_penguins = n()
  )
ggplot(alignment_per_frame, aes(x = frame, y = alignment)) +
  geom_point(color = "coral") +
  geom_smooth(size = 1, color = "steelblue") +
  theme_minimal() +
  labs(title = "",
       x = "Frame",
       y = "Alignment") +
  ylim(c(0,1))

# smoothing 
alignment_per_frame <- alignment_per_frame %>%
  mutate(alignment_smooth = rollmean(alignment, k = 10, fill = NA))

ggplot(alignment_per_frame, aes(x = frame)) +
  geom_line(aes(y = alignment), color = "grey80") +
  geom_line(aes(y = alignment_smooth), color = "steelblue", size = 1) +
  theme_minimal() +
  labs(title = "",
       x = "Frame", y = "Alignment") +
  ylim(c(0,1))

#overlay
y_min <- 150
y_max <- 250

alignment_per_frame$alignment_rescaled <- rescale(alignment_per_frame$alignment, to = c(y_min, y_max))
alignment_per_frame$alignment_smooth_rescaled <- rescale(alignment_per_frame$alignment_smooth, to = c(y_min, y_max))

# Function to reverse rescaling (for secondary axis)
inv_rescale <- function(y) rescale(y, from = c(y_min, y_max), to = c(0, 1))

# Plot
ggplot() +
  # Overlay rescaled alignment
  geom_line(data = alignment_per_frame, 
            aes(x = frame,y = alignment_smooth_rescaled), 
            color = "coral", size = 1.2, alpha = 0.6) +
  # Penguin trajectories
  geom_path(data = track_data, aes(x = frame, y = y, color = penguin, group = penguin), size = 1) +
  geom_point(data = track_data, aes(x = frame, y = y, color = penguin, group = penguin), size = 2, alpha = 0.6) +
  

  
  scale_x_continuous(limits = c(0, 300)) +
  scale_y_continuous(
    name = "Penguin Y Coordinate",
    limits = c(y_min, y_max),
    sec.axis = sec_axis(~ inv_rescale(.), 
                        name = "Alignment") 
  ) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "",
       x = "Frame", color = "Penguin") +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black")
  )
