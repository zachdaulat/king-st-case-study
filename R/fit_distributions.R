library(tidyverse)
library(purrr)
library(gamlss)
library(gamlss.dist)
library(lobstr)
library(sf)

segments <- read_sf("data/raw/bluetooth-travel-time-segments-wgs84")
bt_tts <- bind_rows(
    read_csv("data/raw/detailed-bluetooth-travel-time-2017.gz"),
    read_csv("data/raw/detailed-bluetooth-travel-time-2018.gz")
)
ttc_tts <- read_csv("data/raw/ttc-disaggregate-travel-times.csv")



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

ttc_processed <- ttc_tts |>
    mutate(
        # Parsing TTC TimePeriod into standardized buckets
        time_of_day = case_when(
            str_starts(TimePeriod, "1-AM") ~ "AM Peak",
            str_starts(TimePeriod, "3-PM") ~ "PM Peak",
            TRUE ~ "Off-Peak"
        ),
        time_of_day = factor(time_of_day, levels = c("AM Peak", "PM Peak", "Off-Peak")),
        
        # Defining Pre/Post treatment period
        period = if_else(ObservedDate >= ymd("2017-11-12"), "Post-Treatment", "Pre-Treatment"),
        period = factor(period, levels = c("Pre-Treatment", "Post-Treatment")),
    ) |>
    filter(RunningTime > 0)

# Preparing macro cells for TTC and General Traffic
ttc_nested <- ttc_processed |>
    mutate(data_source = "TTC Streetcar", pace = RunningTime) |>
    select(data_source, period, time_of_day, pace) |>
    group_by(data_source, period, time_of_day) |>
    nest()

bt_nested_macro <- travel_data |>
    mutate(data_source = "General Traffic (Pooled)") |>
    select(data_source, period, time_of_day, pace) |>
    group_by(data_source, period, time_of_day) |>
    nest()

bt_nested_micro <- travel_data |>
  filter(
    (result_id == "KN_YO_KN_UN" & period == "Post-Treatment" & time_of_day == "AM Peak") |
    (result_id == "KN_SP_QU_SP" & period == "Post-Treatment" & time_of_day == "PM Peak") |
    (result_id == "AD_UN_AD_YO" & period == "Post-Treatment" & time_of_day == "Off-Peak") |
    (result_id == "RM_YO_RM_UN" & period == "Pre-Treatment"  & time_of_day == "PM Peak")
  ) |>
  mutate(data_source = paste0("Segment: ", result_id)) |>
  select(data_source, period, time_of_day, pace) |>
  group_by(data_source, period, time_of_day) |>
  nest()

all_cells <- bind_rows(ttc_nested, bt_nested_macro, bt_nested_micro)

dist_fits <- all_cells |>
  mutate(
    # Mapping gamlss::fitDist to the pace column of each group
    best_fit = map(data, \(x) {
      # tryCatch prevents a single cell convergence failure from breaking the whole loop
      tryCatch({
        # Calculating BIC penalty
        penalty <- log(length(x$pace))
        
        gamlss::fitDist(x$pace, k = penalty, type = "realplus")
      }, error = function(e) return(NULL))
    }),
  )


dist_fits_summary <- dist_fits |>
  mutate(
    family_short = map_chr(best_fit, \(x) x$family[1]),
    family_full = map_chr(best_fit, \(x) x$family[2]),
    aic = map_dbl(best_fit, \(x) x$aic),
    bic = map_dbl(best_fit, \(x) x$sbc),
    mu = map_dbl(best_fit, \(x) x$mu),
    sigma = map_dbl(best_fit, \(x) x$sigma),
    nu = map_dbl(best_fit, \(x) x$nu),
    tau = map_dbl(best_fit, \(x) if(!is.null(x$tau)) x$tau else NA_real_)
  ) |>
  select(-data, -best_fit) |>
  ungroup()


write_csv(dist_fits_summary, "data/processed/dist_fits_bic.csv")







