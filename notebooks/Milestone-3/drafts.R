library(tidyverse)
library(tidymodels)
library(statz)
library(bamlss)
library(sf)


segments <- read_sf("data/raw/bluetooth-travel-time-segments-wgs84")
bt_tts <- bind_rows(
    read_csv("data/raw/detailed-bluetooth-travel-time-2017.gz"),
    read_csv("data/raw/detailed-bluetooth-travel-time-2018.gz")
)
ttc_tts <- read_csv("data/raw/ttc-disaggregate-travel-times.csv")



bt_prep <- bt_tts |>
  left_join(
    select(st_drop_geometry(segments), segment_na, length),
    join_by(result_id == segment_na)
  ) |>
  mutate(
    pace = (tt / length) * 100,
    treatment = if_else(datetime_bin >= ymd("2017-11-12"),
                     "Post-Treatment", "Pre-Treatment"),
    treatment = factor(treatment, levels = c("Pre-Treatment", "Post-Treatment")),
    hod = hour(datetime_bin),
    tod = case_when(
      hod >= 7  & hod < 10 ~ "1-AM",
      hod >= 10 & hod < 16 ~ "2-MID",
      hod >= 16 & hod < 19 ~ "3-PM",
      hod >= 17 & hod < 20 ~ "4-EVENING",
      hod >= 20            ~ "5-LATE",
      hod <  7             ~ "0-EARLY"
    ),
    tod = factor(
      tod, 
      levels = c("0-EARLY", "1-AM", "2-MID", "3-PM", "4-EVENING", "5-LATE"),
      ordered = TRUE
    )
  ) |>
  filter(tt > 0, length > 0)





ttc_prep <- ttc_tts |>
  mutate(
    treatment = if_else(ObservedDate >= ymd("2017-11-12"),
                     "Post-Treatment", "Pre-Treatment"),
    treatment = factor(treatment, levels = c("Pre-Treatment", "Post-Treatment")),
    tod = case_when(
      str_starts(TimePeriod, "0-EARLY")   ~ "0-EARLY",
      str_starts(TimePeriod, "1-AM")      ~ "1-AM",
      str_starts(TimePeriod, "2-MID")     ~ "2-MID",
      str_starts(TimePeriod, "3-PM")      ~ "3-PM",
      str_starts(TimePeriod, "4-EVENING") ~ "4-EVENING",
      str_starts(TimePeriod, "5-LATE")    ~ "5-LATE",
    ),
    tod = factor(
      tod, 
      levels = c("0-EARLY", "1-AM", "2-MID", "3-PM", "4-EVENING", "5-LATE"),
      ordered = TRUE
    )
  ) |>
  filter(RunningTime > 0, Speed > 0)

unique(ttc_tts$TimePeriod)



library(knitr)
library(kableExtra)

dist_results <- read_csv("data/processed/dist_fits_bic.csv")

dist_results |>
  select(data_source, period, time_of_day, family_short, family_full, bic) |>
  mutate(
    bic = formatC(bic, format = "f", digits = 0, big.mark = ","),
    family_full = str_remove(family_full, " \\(.*")
  ) |>
  rename(
    `Data Source` = data_source, `Period` = period,
    `Time of Day` = time_of_day, `Distribution` = family_short,
    `Family Name` = family_full, `BIC` = bic
  ) |>
  kable(format = "latex", booktabs = TRUE, linesep = "") |>
  kable_styling(
    latex_options = c("striped", "scale_down"),
    stripe_color = "gray!6"
  )






























