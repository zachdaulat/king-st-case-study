library(tidyverse)
library(purrr)
library(gamlss)
library(gamlss.dist)
library(lobstr)
library(qs2)
library(readxl)
library(sf)
library(ggcorrplot)


segments <- read_sf("data/raw/bluetooth-travel-time-segments-wgs84")
bt_tts <- bind_rows(
    read_csv("data/raw/detailed-bluetooth-travel-time-2017.gz"),
    read_csv("data/raw/detailed-bluetooth-travel-time-2018.gz")
)
ttc_tts <- read_csv("data/raw/ttc-disaggregate-travel-times.csv")



# --------------- Streetcar-Traffic Cross-modal counterfactual validity ------------

# 1. Defining intersecting streets on treatment corridor to filter segments
king_ints <- c("Jarvis", "Yonge", "University", "Spadina", "Bathurst")
king_segments <- segments |>
    filter(
        street == "King",
        from_inter %in% king_ints,
        to_interse %in% king_ints
    )


# 2. Processing TTC Streetcar data
ttc_agg <- ttc_tts |>
  mutate(
    dt_start = ymd_hms(paste0(ObservedDate, "T", TripTime)),
    time_bucket = floor_date(dt_start, "30 mins")
  ) |>
  filter(RunningTime > 0, time_bucket < ymd("2017-11-12")) |>
  summarise(
    mean_streetcar_time = mean(RunningTime, na.rm = TRUE),
    streetcar_trips = n(),
    .by = c("time_bucket", "Direction")
  )

# 3. Processing Bluetooth General Traffic data
bt_agg_prep <- bt_tts |>
  inner_join(
    st_drop_geometry(king_segments) |> select(segment_na, direction), 
    by = join_by(result_id == segment_na)
  ) |>
  mutate(
    time_bucket = floor_date(datetime_bin, "30 mins"),
    Direction = case_when(
      direction == "EB" ~ "EAST",
      direction == "WB" ~ "WEST",
      TRUE ~ NA_character_
    )
  ) |>
  filter(time_bucket < ymd("2017-11-12"))

bt_agg_split <- bt_agg_prep|>
  summarise(
    mean_traffic_time = mean(tt, na.rm = TRUE),
    traffic_obs = n(),
    .by = c("time_bucket", "result_id", "Direction")
  )

# 4. Joining Streetcar and Traffic datasets
king_tts_split <- inner_join(
    ttc_agg,
    bt_agg_split,
    by = join_by(time_bucket, Direction)
)

# 5. Calculating Segment-by-Segment Correlations
corr_results_30 <- king_tts_split |>
  summarise(
    pearson_r = cor(
      mean_streetcar_time,
      mean_traffic_time,
      use = "pairwise.complete.obs",
      method = "pearson"
    ),
    valid_time_buckets = n(),
    .by = "result_id"
  )

corr_results_30

# ---------- Total Corridor correlation ----------
bt_agg_total <- bt_agg_prep |>
  # STEP 1: Mean travel time for each individual segment
  summarise(
    mean_seg_time = mean(tt, na.rm = TRUE),
    .by = c("time_bucket", "result_id", "Direction")
  ) |>
  # STEP 2: Sum segment times to get the total corridor time
  summarise(
    total_traffic_time = sum(mean_seg_time, na.rm = TRUE),
    n_segments = n(),
    .by = c("time_bucket", Direction)
  ) |>
  filter(n_segments == 4)


king_tts_total <- inner_join(
    ttc_agg,
    bt_agg_total,
    by = join_by(time_bucket, Direction)
)

corr_results_total_30 <- king_tts_total |>
  summarise(
    pearson_r = cor(
      mean_streetcar_time,
      total_traffic_time,
      use = "pairwise.complete.obs",
      method = "pearson"
    ),
    time_buckets = n(),
    .by = "Direction"
  )

corr_results_total_30





mode_corr_data <- king_tts_total |>
  select(time_bucket, Direction, mean_streetcar_time, total_traffic_time) |>
  mutate(total_traffic_time = total_traffic_time / 60) |>
  pivot_longer(
    cols = c("mean_streetcar_time", "total_traffic_time"),
    names_to = "mode",
    values_to = "travel_time"
  ) |>
  mutate(
    mode = if_else(mode == "mean_streetcar_time", "TTC Streetcar", "General Traffic")
  )

mode_corr_plot <- mode_corr_data |>
  ggplot(aes(x = time_bucket, y = travel_time, colour = mode)) +
  # geom_point() +
  geom_line(alpha = 0.3, linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 1.2) +
  facet_grid(Direction ~ .) +
  scale_colour_manual(
    values = c("TTC Streetcar" = "#DA251D", "General Traffic" = "#00B0F0")
  ) +
  labs (
    x = "Date",
    y = "Total Travel Time (minutes)",
    colour = "Transit Mode"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

mode_corr_plot



time_series_plot <- ggplot(mode_corr_data, aes(x = time_bucket, y = travel_time, colour = mode)) +
  # Small, transparent points prevent overplotting while showing exact data presence
  geom_point(alpha = 0.2, size = 0.8, stroke = 0) +
  # A tight span on the smooth line tracks the local daily/weekly trends
  geom_smooth(method = "loess", se = FALSE, span = 0.05, linewidth = 1) +
  facet_grid(Direction ~ .) +
  scale_colour_manual(
    values = c("TTC Streetcar" = "#da291c", "General Traffic" = "#004c9b")
  ) +
  labs(
    title = "Pre-Treatment Corridor Travel Times Over Time",
    subtitle = "King Street: Bathurst to Jarvis (30-Minute Intervals)",
    x = "Date",
    y = "Total Travel Time (Minutes)",
    colour = "Transit Mode"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

time_series_plot




scatter_plot <- king_tts_total |>
  # Convert both to minutes for a clean axis
  mutate(total_traffic_time = total_traffic_time / 60) |>
  ggplot(aes(x = total_traffic_time, y = mean_streetcar_time)) +
  # Point alpha helps show density where many buckets had similar traffic
  geom_point(alpha = 0.3, colour = "#004c9b", size = 1.5) +
  # A linear model line directly visualises the Pearson relationship
  geom_smooth(method = "lm", colour = "#da291c", se = FALSE, linewidth = 1.2) +
  facet_wrap(~ Direction, ncol = 2) +
  labs(
    title = "Cross-Modal Travel Time Correlation (Pre-Treatment)",
    subtitle = "King St: Bathurst to Jarvis (30-min aggregates)",
    x = "General Traffic Travel Time (Minutes)",
    y = "TTC Streetcar Travel Time (Minutes)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

scatter_plot







# Summing lengths and dividing by two because segments include both directions
king_length <- sum(king_segments$length / 2)


travel_data <- bt_tts |>
    left_join(
        select(st_drop_geometry(segments), segment_na, length),
        join_by(result_id == segment_na)
    ) |>
    mutate(
        # Calculating inverse speed (seconds per 100 meters)
        pace = (tt / length) * 100,

        # Defining Pre/Post treatment
        period = if_else(datetime_bin >= ymd("2017-11-12"), "Post-Treatment", "Pre-Treatment"),
        period = factor(period, levels = c("Pre-Treatment", "Post-Treatment")),

        # Classify Time of Day to TTC definitions
        hour_of_day = hour(datetime_bin),
        time_of_day = case_when(
            hour_of_day >= 7 & hour_of_day < 10 ~ "AM Peak",
            hour_of_day >= 16 & hour_of_day < 19 ~ "PM Peak",
            TRUE ~ "Off-Peak"
        ),
        time_of_day = factor(time_of_day, levels = c("AM Peak", "PM Peak", "Off-Peak"))
    ) |>
    # Filter out erroneous negative travel times or zero distances
    filter(tt > 0, length > 0)






# Filtering to pre-treatment general traffic and pivoting wide
# each column is a segment
donor_pool <- travel_data |>
    filter(period == "Pre-Treatment") |>
    select(datetime_bin, result_id, pace) |>
    left_join(
        select(st_drop_geometry(segments), result_id = segment_na, street),
        by = "result_id"
    ) |>
    filter(!is.na(street)) # Ensuring no orphan segments
   
# Master ordering dataframe
segment_ordering <- donor_pool |>
    distinct(result_id, street) |>
    arrange(street, result_id)

ordered_segments <- segment_ordering$result_id

donor_wide <- donor_pool |>
    select(datetime_bin, result_id, pace) |>
    pivot_wider(
        names_from = result_id, 
        values_from = pace, 
        values_fn = mean
    ) |>
    select(-datetime_bin)

donor_wide <- donor_wide[, ordered_segments]

# Calculate correlation matrix
corr_matrix <- cor(donor_wide, use = "pairwise.complete.obs", method = "pearson")















# 1. Select your primary parallel substitute streets
# target_streets <- c("Adelaide", "Richmond", "Queen")

# 2. Filter, Join, and Pivot Wide
corridor_data <- travel_data |>
  filter(period == "Pre-Treatment") |>
  left_join(
    select(st_drop_geometry(segments), result_id = segment_na, street), 
    by = "result_id"
  ) |>
  # filter(street %in% target_streets) |>
  select(datetime_bin, result_id, street, pace)

# 3. Create a function to generate a plot for a single street
plot_street_corr <- function(street_name) {
  street_wide <- corridor_data |>
    filter(street == street_name) |>
    select(-street) |>
    pivot_wider(names_from = result_id, values_from = pace, values_fn = mean) |>
    select(-datetime_bin)
  
  # Calculate matrix
  corr_mat <- cor(street_wide, use = "pairwise.complete.obs")
  
  # Generate clean plot
  ggcorrplot(
    corr_mat, 
    hc.order = TRUE, 
    type = "lower",
    lab = TRUE, # Turn labels ON because the matrix is now small enough!
    lab_size = 3,
    title = paste(street_name, "Internal Correlation"),
    colors = c("white", "white", "#E69F00"), # Force scale to show high positive
    show.legend = FALSE
  ) +
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
}

# 4. Generate the plots
plot_adelaide <- plot_street_corr("Adelaide")
plot_richmond <- plot_street_corr("Richmond")
plot_king <- plot_street_corr("King")
plot_queen <- plot_street_corr("Queen")
plot_spadina <- plot_street_corr("Spadina")
plot_dundas <- plot_street_corr("Dundas")
plot_uni <- plot_street_corr("University")
plot_bathurst <- plot_street_corr("Bathurst")

plot_adelaide
plot_richmond
plot_king
plot_queen
plot_spadina
plot_dundas
plot_uni
plot_bathurst





