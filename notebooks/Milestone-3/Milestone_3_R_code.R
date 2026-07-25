# # Install from GitHub Repo
# pak::pkg_install("zachdaulat/statz")

library(tidyverse)
library(tidymodels)
library(sf)

segments <- read_sf("data/raw/bluetooth-travel-time-segments-wgs84")
bt_tts <- bind_rows(
    read_csv("data/raw/detailed-bluetooth-travel-time-2017.gz"),
    read_csv("data/raw/detailed-bluetooth-travel-time-2018.gz")
)
ttc_tts <- read_csv("data/raw/ttc-disaggregate-travel-times.csv")

write_csv(
  read_csv("data/raw/detailed-bluetooth-travel-time-2017.gz"),
  "data/raw/bt_tts.csv"
)

bt_prep <- bt_tts |>
  left_join(
    select(st_drop_geometry(segments), segment_na, length),
    join_by(result_id == segment_na)
  ) |>
  mutate(
    pace = (tt / length) * 100,
    post = if_else(datetime_bin >= ymd("2017-11-12"),
                     "Post-Treatment", "Pre-Treatment"),
    post = factor(
      post, 
      levels = c("Pre-Treatment", "Post-Treatment"),
      ordered = FALSE
    ),
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
  filter(tt > 0)

ttc_prep <- ttc_tts |>
  mutate(
    Direction = factor(Direction, levels = c("EAST", "WEST"), ordered = FALSE),
    post = if_else(ObservedDate >= ymd("2017-11-12"),
                     "Post-Treatment", "Pre-Treatment"),
    post = factor(post, levels = c("Pre-Treatment", "Post-Treatment"), ordered = FALSE),
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
    ),
    # `Speed` units are km/h
    pace = 360 / Speed
  ) |>
  filter(RunningTime > 0, Speed > 0)

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

# "#DA251D"
ttc_red <- rgb(218, 37, 29, maxColorValue = 255)
ttc_blue <- "#00B0F0"

plot_ttc <- ggplot(ttc_prep, aes(x = RunningTime, fill = post)) +
  geom_density(alpha = 0.5, colour = NA) +
  facet_grid(tod ~ Direction) +
  scale_fill_manual(values = c("Pre-Treatment" = ttc_blue,"Post-Treatment" = ttc_red)) +
  # Truncate extreme outliers for a cleaner visual of the main distribution body
  coord_cartesian(xlim = c(0, 40)) + 
  labs(
    x = "Travel Time (minutes)",
    y = "Density",
    fill = "Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plot_ttc

target_segments <- c(
    "AD_UN_AD_YO", # Adelaide: Univ to Yonge (EB)
    "RM_YO_RM_UN", # Richmond: Yonge to Univ (WB)
    "KN_UN_KN_YO", # King: Univ to Yonge (EB)
    "KN_YO_KN_UN", # King: Yonge to Univ (WB)
    "KN_SP_QU_SP", # Spadina: King to Queen (NB)
    "QU_SP_KN_SP", # Spadina: Queen to King (SB)
    "DU_YO_DU_UN", # Dundas: Yonge to Univ (WB)
    "DU_UN_DU_YO", # Dundas: Univ to Yonge (EB)
    "QU_UN_QU_YO", # Queen: Univ to Yonge (EB)
    "QU_YO_QU_UN"  # Queen: Yonge to Univ (WB)
)

bt_sample <- bt_prep |>
  filter(result_id %in% target_segments) |>
  mutate(
    segment_label = case_when(
      result_id == "KN_UN_KN_YO" ~ "King (EB)",
      result_id == "KN_YO_KN_UN" ~ "King (WB)",
      result_id == "AD_UN_AD_YO" ~ "Adelaide (EB)",
      result_id == "RM_YO_RM_UN" ~ "Richmond (WB)",
      result_id == "QU_UN_QU_YO" ~ "Queen (EB)",
      result_id == "QU_YO_QU_UN" ~ "Queen (WB)",
      result_id == "DU_UN_DU_YO" ~ "Dundas (EB)",
      result_id == "DU_YO_DU_UN" ~ "Dundas (WB)",
      result_id == "KN_SP_QU_SP" ~ "Spadina (NB)",
      result_id == "QU_SP_KN_SP" ~ "Spadina (SB)",
    ),
    segment_label = factor(segment_label, levels = c(
      "King (EB)", "King (WB)",
      "Adelaide (EB)", "Richmond (WB)",
      "Queen (EB)", "Queen (WB)",
      "Dundas (EB)", "Dundas (WB)",
      "Spadina (NB)", "Spadina (SB)"
    ))
  )

plot_bt <- ggplot(bt_sample, aes(x = pace, fill = post)) +
  geom_density(alpha = 0.5, colour = NA) +
  # Facet grid allows us to compare segments (columns) across times of day (rows)
  facet_grid(tod ~ segment_label) + 
  scale_fill_manual(values = c("Pre-Treatment" = ttc_blue, "Post-Treatment" = ttc_red)) +
  coord_cartesian(xlim = c(0, 100)) + 
  labs(
    x = "Pace (Seconds / 100m)",
    y = "Density",
     fill = "Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold")
  )

plot_bt

spillover_segments <- c(
  "AD_JA_AD_PA", "AD_UN_AD_YO", "AD_YO_AD_JA", "AD_SP_AD_UN", "AD_PA_EA_BV", # Adelaide segments
  "KN_BA_QU_BA", "QU_BA_KN_BA", "KN_BA_FR_BA", "FR_BA_KN_BA",                # Bathurst segments except King
  "FR_JA_FR_PA", "FR_YO_FR_JA", "FR_PA_FR_JA", "FR_SP_FR_UN", # Front St segments (most)
  "FR_UN_FR_SP", "FR_UN_FR_YO", "FR_YO_FR_UN", 
  "QU_UN_QU_SP", "QU_UN_QU_YO", "QU_YO_QU_JA", "QU_BA_QU_SP", # Queen St, Bathurst to Jarvis 
  "QU_JA_QU_YO", "QU_YO_QU_UN", "QU_SP_QU_BA", "QU_SP_QU_UN",
  "KN_SP_FR_SP", "KN_SP_QU_SP", "FR_SP_KN_SP", "QU_SP_KN_SP", # Spadina connecting to King
  "QU_UN_KN_UN", "KN_UN_FR_UN", "KN_UN_QU_UN", "FR_UN_KN_UN" # University connecting to King
)
spillover_streets <- c("King", "Jarvis", "Richmond", "Wellington", "Yonge", "York")

ridge_segments <- st_drop_geometry(segments) |>
  filter(!segment_na %in% spillover_segments, !street %in% spillover_streets) |>
  pull(segment_na)

bt_ridge <- bt_prep |>
  # anti_join(ridge_segments, by = join_by(result_id == segment_na)) |>
  filter(
    result_id %in% ridge_segments,
    post == "Pre-Treatment",
    tod == "3-PM"
  )

ttc_ridge <- ttc_prep |>
  filter(
    Direction == "WEST",
    post == "Pre-Treatment",
    tod == "3-PM"
  )

data_ridge <- bind_rows(
  # Target (TTC Streetcars)
  ttc_ridge |>
    select(pace) |>
    mutate(
      unit = "King St (Target)",
      status = "Target"
    ),
  
  # Donors (Bluetooth sensor pairs)
  bt_ridge |>
    select(pace, result_id) |>
    rename(unit = result_id) |>
    mutate(status = "Donor")
) |>
  mutate(
    unit = factor(unit, ordered = FALSE),
    unit = fct_reorder(unit, pace, .fun = median, na.rm = TRUE),
    unit = fct_relevel(unit, "King St (Target)", after = Inf)
  )


library(ggridges)

plot_ridge <- ggplot(
  data_ridge,
  aes(x = pace, y = unit, fill = status, colour = status, alpha = status)
) +
  geom_density_ridges(scale = 4, rel_min_height = 0.005) +

  # Controling limits to handle outliers
  coord_cartesian(xlim = c(0, 80)) +

  # Mapping aesthetics for visual hierarchy
  scale_fill_manual(values = c("Target" = ttc_red, "Donor" = "grey60")) +
  scale_colour_manual(values = c("Target" = "black", "Donor" = NA)) + # NA removes donor borders
  scale_alpha_manual(values = c("Target" = 0.9, "Donor" = 0.5)) +

  theme_minimal(base_size = 14) +
  theme(
    # Remove all y-axis text and ticks to create the "Envelope" effect
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    
    # Remove horizontal grid lines so the ridges float cleanly
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Position the legend cleanly
    legend.position = "right",
    legend.title = element_blank()
  ) +
  labs(
    # title = "Pre-Treatment Delay Distributions: PM Peak (Westbound)",
    # subtitle = "King Street Target vs. Candidate Donor Pool Convex Hull",
    x = "Pace (Seconds per 100m)",
    # caption = "Donor pool spatially filtered to exclude contiguous spillover segments."
  )

plot_ridge

rm(bt_ridge, ttc_ridge, data_ridge, bt_sample, target_segments, 
plot_bt, plot_ttc, plot_ridge, dist_results, bt_tts, ttc_tts)

ttc_clean <- ttc_prep |>
  filter(
    Direction == "WEST",
    post == "Pre-Treatment",
    tod  == "3-PM"
  ) |>
  mutate(
    unit_id = "ttc_west",
    bucket = yday(ObservedDate)
  ) |>
  select(unit_id, bucket, pace)

bt_clean <- bt_prep |>
  filter(
    result_id %in% ridge_segments,
    post == "Pre-Treatment",
    tod  == "3-PM"
  ) |>
  mutate(bucket = yday(datetime_bin)) |>
  select(unit_id = result_id, bucket, pace)

panel_raw <- bind_rows(ttc_clean, bt_clean) |>
  filter(!is.na(pace))

expected_units <- n_distinct(panel_raw$unit_id)

panel_balanced <- panel_raw |>
  filter(n_distinct(unit_id) == expected_units, .by = bucket) |>
  arrange(bucket, unit_id)

# Temporal train-test splitting
unique_buckets <- unique(panel_balanced$bucket)
split_idx <- floor(length(unique_buckets) * 0.8)

# Isolating the training and testing buckets
train_buckets <- unique_buckets[1:split_idx]
test_buckets  <- unique_buckets[(split_idx + 1):length(unique_buckets)]

# Filter the panel based on those bucket assignments
panel_train <- panel_balanced |> filter(bucket %in% train_buckets)
panel_test  <- panel_balanced |> filter(bucket %in% test_buckets)

# n_distinct(panel_raw$bucket)
# n_distinct(panel_balanced$bucket)
# n_distinct(panel_train)
# n_distinct(panel_train$bucket)
# n_distinct(panel_test)
# n_distinct(panel_test$bucket)
# 
# min(ttc_prep$ObservedDate)
# min(bt_prep$datetime_bin)
# 
# bucket_density <- panel_train |>
#   count(bucket, unit_id) |>
#   arrange(n)
# 
# write_csv(bucket_density, "data/processed/panel_train_bucket_density.csv")
# 
# # Installation from source
# # pak::pkg_install("zachdaulat/statz")

# Loading the `statz` package
library(statz)

# Fitting the Distributional Synthetic Control
dsc_model <- z_dsc(
  data = panel_train,
  response = pace,
  unit_id = unit_id,
  treated_unit = "ttc_west",
  bucket = bucket,
  center = TRUE,
  n_quantiles = 100L,
  lambda = 0.1,
  max_iter = 1e4L,
  tol = 1e-8
)

# Temporarily strip ANSI codes that break LaTeX
options(cli.num_colors = 1, cli.unicode = FALSE)
rm(ttc_clean, bt_clean, panel_raw, panel_balanced, expected_units, split_idx, unique_buckets,
train_buckets, test_buckets, ridge_segments, spillover_streets, spillover_segments)

print(dsc_model)

summary(dsc_model)

# Restore default CLI behaviour for the rest of the session
options(cli.num_colors = NULL, cli.unicode = TRUE)

#' Constructing the Synthetic Barycenter and Evaluating Fit on the holdout data/test set.
#' 
#' @param model "z_dsc" model object
#' @param data A dataframe or tibble containing the panel data
#' @param response Column name of the numeric response variable
#' @param unit_id Column name of the observational unit IDs, a string or factor
#' @param treated_id A string identifying the treated unit in `unit_id`
#' @param bucket Column name for the string or factor grouping variable
#' @return A list containing the average squared 2-Wasserstein distance
eval_dsc <- function(model, data, response, unit_id, treated_id, bucket) {

  resp_var <- rlang::enquo(response)
  unit_var <- rlang::enquo(unit_id)
  bucket_var <- rlang::enquo(bucket)

  df <- dplyr::select(
    data,
    .response = !!resp_var,
    .unit = !!unit_var,
    .bucket = !!bucket_var
  )

  probs <- model$params$probs
  weights <- model$weights
  alpha <- model$alpha
  donor_names <- names(model$weights)
  
  test_buckets <- unique(df$.bucket)
  w2_sq_distances <- numeric(length(test_buckets))
  
  # Iterate over each bucket to calculate the daily distance
  for (i in seq_along(test_buckets)) {
    b <- test_buckets[i]
    df_b <- dplyr::filter(df, .bucket == b)
    
    target_quantiles <- df_b |>
      dplyr::filter(.unit == treated_id) |>
      dplyr::pull(.response) |>
      stats::quantile(probs = probs, names = FALSE, type = 7)
    
    # 4. Calculating donor quantiles
    donor_matrix <- df_b |>
      dplyr::filter(.unit %in% donor_names) |>
      dplyr::mutate(.unit = factor(.unit, levels = donor_names)) |>
      dplyr::reframe(
        q = stats::quantile(.response, probs, names = FALSE, type = 7),
        q_idx = seq_along(probs),
        .by = .unit
      ) |>
      tidyr::pivot_wider(names_from = .unit, values_from = q) |>
      # Select explicitly drops q_idx and guarantees column order
      dplyr::select(dplyr::all_of(donor_names)) |>
      as.matrix()
    
    barycenter <- as.vector(donor_matrix %*% weights) + alpha
    w2_sq_distances[i] <- mean((target_quantiles - barycenter)^2)
  }

  list(
    # Average W2 distance across all holdout buckets
    w2_distance = mean(w2_sq_distances), 
    w2_by_bucket = w2_sq_distances
  )
}

# Evaluating previous model
eval_model <- eval_dsc(
  model = dsc_model,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

# Create uniform weights vector
j_count <- length(dsc_model$params$donor_units)
uniform_w <- rep(1 / j_count, j_count)
names(uniform_w) <- dsc_model$params$donor_units

# Construct the dummy model
dsc_uniform <- list(
  weights = uniform_w,
  alpha = 0,
  params = list(
    probs = dsc_model$params$probs,
    donor_units = dsc_model$params$donor_units
  )
)

# Joint mean-shape/meaned fit
dsc_uncentered <- z_dsc(
  data = panel_train,
  response = pace,
  unit_id = unit_id,
  treated_unit = "ttc_west",
  bucket = bucket,
  center = FALSE,
  n_quantiles = 100L,
  lambda = 0.1,
  max_iter = 1e4L,
  tol = 1e-8
)

# Single bucket model
single_train <- panel_train |>
  mutate(bucket = 1)

dsc_single <- z_dsc(
  data = single_train,
  response = pace,
  unit_id = unit_id,
  treated_unit = "ttc_west",
  bucket = bucket,
  center = TRUE,
  n_quantiles = 100L,
  lambda = 0.1,
  max_iter = 1e4L,
  tol = 1e-8
)

# Lambda demonstration fit
dsc_lambda <- z_dsc(
  data = panel_train,
  response = pace,
  unit_id = unit_id,
  treated_unit = "ttc_west",
  bucket = bucket,
  center = FALSE,
  n_quantiles = 100L,
  lambda = 100,
  max_iter = 1e4L,
  tol = 1e-8
)

# Evaluate uniform weights
eval_uniform <- eval_dsc(
  model = dsc_uniform,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

# Evaluate uncentered model with no alpha
eval_uncentered <- eval_dsc(
  model = dsc_uncentered,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

# Evaluate single bucket
eval_single <- eval_dsc(
  model = dsc_single,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

# Evaluate on model with higher lambda
eval_lambda <- eval_dsc(
  model = dsc_lambda,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

# Lambda 100 with single bucket centered
dsc_l2_single_centered <- z_dsc(
  data = single_train,
  response = pace,
  unit_id = unit_id,
  treated_unit = "ttc_west",
  bucket = bucket,
  center = TRUE,
  n_quantiles = 200L,
  lambda = 100,
  max_iter = 1e4L,
  tol = 1e-8
)

# Evaluate lambda 100 with single bucket centered
eval_l2_single_centered <- eval_dsc(
  model = dsc_l2_single_centered,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

# Lambda 100 with single bucket uncentered
dsc_l2_single_uncenter <- z_dsc(
  data = single_train,
  response = pace,
  unit_id = unit_id,
  treated_unit = "ttc_west",
  bucket = bucket,
  center = FALSE,
  n_quantiles = 200L,
  lambda = 100,
  max_iter = 1e4L,
  tol = 1e-8
)

# Evaluate lambda 100 with single bucket uncentered
eval_l2_single_uncenter <- eval_dsc(
  model = dsc_l2_single_uncenter,
  data = panel_test,
  response = pace,
  unit_id = unit_id,
  treated_id = "ttc_west",
  bucket = bucket
)

library(tibble)
library(knitr)

# Collecting the results into a tibble
baseline_results <- tibble(
  `Model Configuration` = c(
    "Multi-Bucket (Baseline, lambda = 0.1)",
    "Multi-Bucket (High L2, lambda = 100)",
    "Single Bucket (Baseline, lambda = 0.1)",
    "Uncentered (Joint Mean-Shape)",
    "Uniform Weights (Naive Null)",
    "Single Bucket High L2 Centered",
    "Single Bucket High L2 Uncentered"
  ),
  `Sq. 2-Wasserstein Distance` = c(
    eval_model$w2_distance,
    eval_lambda$w2_distance,
    eval_single$w2_distance,
    eval_uncentered$w2_distance,
    eval_uniform$w2_distance,
    eval_l2_single_centered$w2_distance,
    eval_l2_single_uncenter$w2_distance
  )
) |>
  arrange(desc(`Sq. 2-Wasserstein Distance`))

# Render table
kable(
  baseline_results,
  digits = 1,
  caption = "Holdout Evaluation of DSC Configurations vs. Baselines",
  align = c("l", "c"),
  booktabs = TRUE
)

# sqrt(eval_single$w2_distance)
# sqrt(dsc_single$diagnostics$loss_penalized)
# sqrt(eval_lambda_single$w2_distance)

n_weights_model <- sum(dsc_model$weights != 0)
n_weights_single <- sum(dsc_single$weights != 0)
n_weights_lambda <- sum(dsc_lambda$weights != 0)
n_weights_l2_single <- sum(dsc_l2_single_uncenter$weights != 0)

rm(baseline_results, j_count, uniform_w, eval_dsc, panel_train, panel_test,
dsc_model, dsc_lambda, dsc_single, dsc_uncentered, dsc_uniform,
eval_model, eval_lambda, eval_single, eval_uncentered, eval_uniform,
n_weights_model, n_weights_single, n_weights_lambda, n_weights_l2_single)

dsc_predict <- function(model, donor_data) {

  # Extracting fitted model parameters
  probs <- model$params$probs
  weights <- tibble::enframe(model$weights, name = "unit_id", value = "w")
  alpha <- model$alpha

  # dplyr pipeline constructing the DSC
  donor_data |>
    dplyr::filter(unit_id %in% weights$unit_id) |>
    dplyr::reframe(
      prob = probs,
      q    = stats::quantile(pace, probs = probs, type = 7, names = FALSE),
      .by = c(post, unit_id)
    ) |>
    dplyr::left_join(weights, by = "unit_id") |>
    dplyr::summarise(
      pace  = sum(q * w) + alpha,
      w_sum = sum(w),
      .by   = c(post, prob)
    ) |>
    dplyr::mutate(series = "synthetic")
}

# Setup
active_weights <- dsc_l2_single_uncenter$weights[dsc_l2_single_uncenter$weights > 1e-3]
active_weights <- active_weights / sum(active_weights)
window_start <- ymd("2017-09-01")
window_end   <- ymd("2018-10-31")

# Preparing TTC data
ttc_panel <- ttc_prep |>
  filter(
    Direction == "WEST", tod == "3-PM",
    ObservedDate >= window_start, ObservedDate <= window_end
  ) |>
  mutate(unit_id = "ttc_west") |>
  select(unit_id, pace, post)

# Preparing donor pool data
bt_panel <- bt_prep |>
  filter(
    result_id %in% names(active_weights), tod == "3-PM",
    datetime_bin >= window_start, datetime_bin < window_end + days(1)
  ) |>
  select(unit_id = result_id, pace, post)


# Combined panel
panel_raw <- bind_rows(ttc_panel, bt_panel)

# Extracting target/treated quantiles
treated_q <- panel_raw |>
  filter(unit_id == "ttc_west") |>
  reframe(
    prob = dsc_l2_single_uncenter$params$probs,
    pace = stats::quantile(
      x = pace, 
      probs = dsc_l2_single_uncenter$params$probs, 
      type = 7, 
      names = FALSE
    ),
    .by = c(post)
  ) |>
  mutate(series = "treated")

# Extracting synthetic quantiles
dsc_active <- dsc_l2_single_uncenter
dsc_active$weights <- active_weights
synthetic_q <- dsc_predict(dsc_active, panel_raw)

# Constructing design matrix
panel_bamlss <- bind_rows(treated_q, synthetic_q) |>
  mutate(
    series = factor(series, levels = c("synthetic", "treated")),
    post = factor(post, levels = c("Pre-Treatment", "Post-Treatment"))
  )

# pak::pkg_install("bamlss")
# pak::pkg_install("gamlss.dist")
library(bamlss)
library(gamlss.dist)

f <- list(
  pace  ~ series * post,  # mu:    speed effect
  sigma ~ series * post,  # sigma: spread, variation
  nu    ~ series * post,  # nu:    tail asymmetry/skewness
  tau   ~ series          # tau:   tail heaviness/kurtosis
)

set.seed(2001)
model <- bamlss::bamlss(
  formula = f, 
  family = gamlss.dist::BCT, 
  data = panel_bamlss, 
  verbose = FALSE
)

summary(model)

# 1. Extract Point Estimates (Means) as a structured list
model_coefs <- coef(model, list = TRUE)

# Access the parametric ("p") terms of the location ("mu") formula
# Extracting the point estimates (Means)
mu_intercept <- model_coefs$mu["mu.p.(Intercept)", "Mean"]
mu_treated   <- model_coefs$mu["mu.p.seriestreated", "Mean"]
mu_did_mean  <- model_coefs$mu["mu.p.seriestreated:postPost-Treatment", "Mean"]

# Extracting the credible intervals
did_lower <- model_coefs$mu["mu.p.seriestreated:postPost-Treatment", "2.5%"]
did_upper <- model_coefs$mu["mu.p.seriestreated:postPost-Treatment", "97.5%"]
tau_intercept <- model_coefs$tau["tau.p.(Intercept)", "Mean"]
tau_treated <- model_coefs$tau["tau.p.seriestreated", "Mean"]

# 2. Extract Credible Intervals
# confint() natively parses the MCMC samples to generate the bounds
# model_ci <- confint(model)

emp_intercept <- panel_bamlss |>
  filter(series == "synthetic", post == "Pre-Treatment") |>
  pull(pace) |> mean()

emp_treated <- panel_bamlss |>
  filter(series == "treated") |> pull(pace) |> mean()

# summary(model)

# 
# p <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
# qBCT(p, mu = 23.80, sigma = 2.369, nu = 2.308, tau = 2.471)
# qBCT(p, mu = 66.71, sigma = 1.836, nu = -4.807, tau = 2.471)
# qBCT(p, mu = 29.51, sigma = 1.193, nu = 2.110, tau = 2.471)
# qBCT(p, mu = 48.90, sigma = 1.176, nu = -5.467, tau = 2.471)
# 
# qBCT(p, mu = 39.524, sigma = exp(-0.4072), nu = 0.8364, tau = exp(157.7))
# qBCT(p, mu = 83.136, sigma = exp(1.1578), nu = -3.6914, tau = 7.39)
# qBCT(p, mu = 8.032, sigma = exp(2.7148), nu = 1.6535, tau = exp(157.7))
# qBCT(p, mu = 36.737, sigma = exp(-1.9658), nu = -0.4877, tau = 7.39)
# 
# 
# 
# ggplot(panel_bamlss, aes(pace, colour = series)) + geom_density() + facet_wrap(~post)
# 
# 
# 
# dsc_c <- dsc_l2_single_centered
# dsc_c$weights <- dsc_c$weights[dsc_c$weights > 1e-3] |> (\(w) w / sum(w))()
# synth_c <- dsc_predict(dsc_c, panel_raw)
# ggplot(synth_c, aes(pace)) + geom_density() + facet_wrap(~post)
# 
# 
# 
# 
# # then qBCT(p, mu, sigma, nu, tau) per cell using the table above, and compare
# 
# sum(dsc_l2_single_uncenter$weights != 0)          # if ~68, weights are essentially uniform
# max(dsc_l2_single_uncenter$weights)               # compare against 1/68 = 0.0147
# best_weights <- dsc_l2_single_uncenter$weights
# best_weights[best_weights != 0]
# 
# 
# 
# print(bucket_summary)
# 
# bind_rows(ttc_panel, bt_panel) |>
#   summarise(n_units = n_distinct(unit_id), .by = bucket) |>
#   arrange(bucket) |>
#   print(n = 50)
# 
# range(ttc_panel$bucket)   # does TTC coverage even reach 2018?
# range(bt_panel$bucket)

ggplot(panel_bamlss, aes(x = pace, colour = series)) +
  geom_density(linewidth = 0.8) +
  facet_wrap(~ post) +
  scale_colour_manual(values = c("synthetic" = ttc_blue, "treated" = ttc_red)) +
  labs(x = "Pace (Seconds / 100m)", y = "Density", colour = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))



empirical_summary <- panel_bamlss |>
  summarise(
    mean     = mean(pace),
    sd       = sd(pace),
    skewness = e1071::skewness(pace),
    kurtosis = e1071::kurtosis(pace),
    q01      = quantile(pace, probs = 0.01, type = 7, names = FALSE),
    q05      = quantile(pace, probs = 0.05, type = 7, names = FALSE),
    q10      = quantile(pace, probs = 0.10, type = 7, names = FALSE),
    q25      = quantile(pace, probs = 0.25, type = 7, names = FALSE),
    median   = median(pace),
    q75      = quantile(pace, probs = 0.75, type = 7, names = FALSE),
    q90      = quantile(pace, probs = 0.90, type = 7, names = FALSE),
    q95      = quantile(pace, probs = 0.95, type = 7, names = FALSE),
    q99      = quantile(pace, probs = 0.99, type = 7, names = FALSE),
    .by      = c(series, post)
  ) |>
  arrange(post, series)

empirical_table <- empirical_summary |>
  pivot_longer(
    cols = -c(post, series),
    names_to = "Statistic",
    values_to = "Value"
  ) |>
  # Pivoting wider using both variables to create 4 distinct DiD columns
  pivot_wider(
    names_from = c(post, series),
    values_from = Value,
    names_sep = "___" # Using a distinct separator to avoid naming collisions
  ) |>
  mutate(
    `Pre-Treatment Gap`  = `Pre-Treatment___treated` - `Pre-Treatment___synthetic`,
    `Post-Treatment Gap` = `Post-Treatment___treated` - `Post-Treatment___synthetic`,
    `DiD Effect`         = `Post-Treatment Gap` - `Pre-Treatment Gap`
  ) |>
  # Cleaning up the statistic names for the final printed table
  mutate(
    Statistic = case_match(
      Statistic,
      "mean"     ~ "Mean",
      "sd"       ~ "Std. Deviation",
      "skewness" ~ "Skewness",
      "kurtosis" ~ "Excess Kurtosis",
      "q01"      ~ "1st Percentile",
      "q05"      ~ "5th Percentile",
      "q10"      ~ "10th Percentile",
      "q25"      ~ "25th Percentile",
      "median"   ~ "Median",
      "q75"      ~ "75th Percentile",
      "q90"      ~ "90th Percentile",
      "q95"      ~ "95th Percentile",
      "q99"      ~ "99th Percentile",
    )
  )

write_csv(empirical_table, "data/processed/empirical_table.csv")

mean_pre_gap <- empirical_table$`Pre-Treatment Gap`[1]

kable(
  empirical_table,
  format = "latex",
  booktabs = TRUE,
  # Update column names to include the new gap columns
  col.names = c(
    "Statistic", "Synthetic", "Target", "Synthetic", "Target", 
    "Pre-Gap", "Post-Gap", "DiD Effect"
  ),
  digits = 2,
  # caption = "Empirical Distributional Statistics and Treatment Effects"
) |>
  add_header_above(c(
    " " = 1, 
    "Pre-Treatment" = 2, 
    "Post-Treatment" = 2, 
    "Causal Estimates" = 3
  )) |>
  kable_styling(
    latex_options = c("striped", "scale_down"),
    stripe_color = "gray!6"
  )

# f_treated <- list(pace ~ post, sigma ~ post, nu ~ post, tau ~ post)
# m_treated <- bamlss(f_treated, family = BCT,
#                     data = filter(panel_bamlss, series == "treated"),
#                     verbose = FALSE)
# 
# summary(m_treated)

# 1. Get the exact input file explicitly from knitr
current_file <- knitr::current_input(dir = TRUE)

if (!is.null(current_file)) {
  
  # 2. Safely construct the output path
  milestone_dir <- dirname(current_file)
  base_name <- paste0(tools::file_path_sans_ext(basename(current_file)), "_R_code.R")
  output_script <- file.path(milestone_dir, base_name)
  
  # 3. Create a temporary R script to bypass all OS shell quoting issues
  tmp_r <- tempfile(fileext = ".R")
  
  # 4. Write the exact purl command into the temp file
  script_lines <- c(
    "suppressPackageStartupMessages(library(knitr))",
    sprintf("knitr::purl('%s', output = '%s', documentation = 0, quiet = TRUE)", 
            current_file, output_script)
  )
  writeLines(script_lines, tmp_r)
  
  # 5. Execute the temp script using a clean background R process
  exit_status <- system2("Rscript", args = tmp_r)
  
  # 6. Report the result to the Positron Render pane
  if (exit_status == 0) {
    message("Successfully extracted R code to: ", output_script)
  } else {
    message("Failed to extract R code. Exit status: ", exit_status)
  }
}
