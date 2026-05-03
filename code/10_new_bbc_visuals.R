# 10_new_bbc_visuals.R
# Goal:
#   Create Wimbledon-only BBC-style visuals:
#   - One plot for Wimbledon men
#   - One plot for Wimbledon women
#   - Each plot shows the top 10 recurring servers on the SAME graph
#   - Lines show projected First Serve SQS percentile over time
#   - Dot size shows number of observed first serves
#   - Eligible players must appear in at least 5 of the 8 Wimbledon years
#
# Notes:
#   - Historical scores use the fixed-effect component of the modern SQS model.
#   - Percentiles are computed within each tournament-year field.
#   - This script uses Wimbledon only.

rm(list = ls())

library(tidyverse)
library(data.table)
library(lme4)
library(ggrepel)
library(scales)

# ------------------------------------------------------------------------------
# Configuration

years_keep <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)

tournament <- "wimb"
genders <- c("men", "women")

serve_num <- 1
serve_label <- ifelse(serve_num == 1, "First Serve", "Second Serve")

# Minimum total serves across all years for a player to be included in model fitting.
min_serves_model <- 20

# Minimum serves for a player-year to be scored and shown.
min_serves_plot <- 5

# Require players to appear in at least this many of the 8 Wimbledon years.
min_years_present <- 5

# Number of recurring top servers to show on each gender-specific plot.
n_players_show <- 10

input_dir <- "data/processed/subset"
out_dir <- file.path("figures", "bbc_visuals", "top10_same_plot")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Helper functions

tournament_label_for <- function(tournament) {
  case_when(
    tournament == "wimb" ~ "Wimbledon",
    tournament == "us" ~ "U.S. Open",
    TRUE ~ tournament
  )
}

gender_label_for <- function(gender) {
  case_when(
    gender == "men" ~ "Men",
    gender == "women" ~ "Women",
    TRUE ~ gender
  )
}

clean_player_name <- function(x) {
  x_clean <- str_to_title(tolower(x))
  
  case_when(
    x_clean %in% c("Cori Gauff", "Coco Gauff") ~ "Coco Gauff",
    TRUE ~ x_clean
  )
}

compute_entropy <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  
  p <- prop.table(table(x))
  -sum(p * log2(p))
}

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_sd <- function(x) {
  out <- sd(x, na.rm = TRUE)
  ifelse(is.finite(out), out, NA_real_)
}

load_tournament_gender_data <- function(tournament, gender) {
  files <- file.path(
    input_dir,
    paste0(years_keep, "_", tournament, "_", gender, ".csv")
  )
  
  existing_files <- files[file.exists(files)]
  
  if (length(existing_files) == 0) {
    warning("No files found for ", tournament, " ", gender)
    return(NULL)
  }
  
  map_dfr(existing_files, function(f) {
    message("Reading: ", f)
    
    fread(
      f,
      colClasses = list(
        character = c(
          "match_id", "ElapsedTime",
          "player1", "player2",
          "ServeWidth", "ServeDepth",
          "P1Score", "P2Score",
          "state",
          "player1_name", "player2_name"
        )
      )
    ) %>%
      as_tibble() %>%
      mutate(
        source_file = basename(f),
        match_id = as.character(match_id),
        player1 = as.character(player1),
        player2 = as.character(player2),
        ServeWidth = as.character(ServeWidth),
        ServeDepth = as.character(ServeDepth)
      )
  })
}

clean_points <- function(df) {
  df %>%
    mutate(
      year = as.integer(year),
      ServeIndicator = as.integer(ServeIndicator),
      ServeNumber = as.integer(ServeNumber),
      PointWinner = as.integer(PointWinner),
      Speed_MPH = as.numeric(Speed_MPH),
      RallyCount = as.numeric(RallyCount)
    ) %>%
    filter(year %in% years_keep) %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    filter(!is.na(Speed_MPH), Speed_MPH > 0) %>%
    mutate(
      location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
      
      ServerName_raw = ifelse(ServeIndicator == 1, player1, player2),
      ReturnerName_raw = ifelse(ServeIndicator == 1, player2, player1),
      
      ServerName = clean_player_name(ServerName_raw),
      ReturnerName = clean_player_name(ReturnerName_raw),
      
      is_ace = ifelse(ServeIndicator == 1, P1Ace, P2Ace),
      
      server_won = as.integer(
        (ServeIndicator == 1 & PointWinner == 1) |
          (ServeIndicator == 2 & PointWinner == 2)
      ),
      
      is_efficient = as.integer(server_won == 1 & RallyCount <= 3)
    )
}

build_server_profiles <- function(df_clean, serve_num, group_year = FALSE) {
  group_vars <- if (group_year) {
    c("year", "ServerName")
  } else {
    c("ServerName")
  }
  
  df_clean %>%
    filter(ServeNumber == serve_num) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      avg_speed = mean(Speed_MPH, na.rm = TRUE),
      sd_speed = safe_sd(Speed_MPH),
      location_entropy = compute_entropy(location_bin),
      modal_location = get_mode(location_bin),
      n_serves = n(),
      .groups = "drop"
    ) %>%
    filter(
      is.finite(avg_speed),
      is.finite(sd_speed),
      is.finite(location_entropy),
      !is.na(modal_location)
    )
}

zscore_with_params <- function(profiles, scale_params) {
  profiles %>%
    mutate(
      avg_speed_z =
        (avg_speed - scale_params$avg_speed_mu) / scale_params$avg_speed_sd,
      sd_speed_z =
        (sd_speed - scale_params$sd_speed_mu) / scale_params$sd_speed_sd,
      location_entropy_z =
        (location_entropy - scale_params$location_entropy_mu) / scale_params$location_entropy_sd
    )
}

score_profiles_fixed_effects_only <- function(model, profiles_year_z) {
  b <- fixef(model)
  
  model_levels <- levels(model@frame$modal_location)
  
  profiles_year_z <- profiles_year_z %>%
    mutate(
      modal_location = factor(modal_location, levels = model_levels)
    ) %>%
    filter(!is.na(modal_location))
  
  mm <- model.matrix(
    ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location,
    data = profiles_year_z
  )
  
  cols <- intersect(colnames(mm), names(b))
  
  profiles_year_z %>%
    mutate(
      SQS_FE_logodds = as.numeric(mm[, cols, drop = FALSE] %*% b[cols])
    ) %>%
    filter(is.finite(SQS_FE_logodds))
}

# ------------------------------------------------------------------------------
# Select recurring top servers

select_recurring_top_servers <- function(sqs_year,
                                         min_years_present = 5,
                                         n_players_show = 10) {
  sqs_year %>%
    group_by(ServerName) %>%
    summarise(
      years_present = n_distinct(year),
      mean_percentile = mean(SQS_percentile, na.rm = TRUE),
      median_percentile = median(SQS_percentile, na.rm = TRUE),
      mean_centered_sqs = mean(SQS_FE_centered, na.rm = TRUE),
      total_serves = sum(n_serves, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(years_present >= min_years_present) %>%
    arrange(desc(mean_percentile), desc(total_serves)) %>%
    slice_head(n = n_players_show)
}

# ------------------------------------------------------------------------------
# Main combined top-10 plot

make_top10_same_plot <- function(sqs_year,
                                 tournament,
                                 gender,
                                 min_years_present = 5,
                                 n_players_show = 10) {
  tournament_label <- tournament_label_for(tournament)
  gender_label <- gender_label_for(gender)
  
  selected_players <- select_recurring_top_servers(
    sqs_year = sqs_year,
    min_years_present = min_years_present,
    n_players_show = n_players_show
  )
  
  if (nrow(selected_players) == 0) {
    warning(
      "No players meet min_years_present = ",
      min_years_present,
      " for ",
      tournament,
      " ",
      gender
    )
    return(NULL)
  }
  
  plot_df <- sqs_year %>%
    semi_join(selected_players, by = "ServerName") %>%
    left_join(
      selected_players %>%
        select(ServerName, mean_percentile, years_present),
      by = "ServerName"
    ) %>%
    mutate(
      ServerName = fct_reorder(ServerName, mean_percentile, .desc = TRUE)
    )
  
  # Label each player at their final observed year.
  label_df <- plot_df %>%
    group_by(ServerName) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    slice_max(order_by = n_serves, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  p <- ggplot(
    plot_df,
    aes(
      x = year,
      y = SQS_percentile,
      color = ServerName,
      group = ServerName
    )
  ) +
    geom_line(linewidth = 1.1, alpha = 0.85) +
    geom_point(aes(size = n_serves), alpha = 0.9) +
    ggrepel::geom_text_repel(
      data = label_df,
      aes(label = ServerName),
      size = 3.6,
      fontface = "bold",
      show.legend = FALSE,
      direction = "y",
      nudge_x = 0.35,
      segment.alpha = 0.35,
      min.segment.length = 0
    ) +
    scale_x_continuous(
      breaks = years_keep,
      limits = c(min(years_keep), max(years_keep) + 1)
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "th")
    ) +
    scale_size_continuous(
      name = "Observed\nfirst serves",
      range = c(2, 7)
    ) +
    labs(
      title = paste0("Top Wimbledon Servers Over Time: ", gender_label),
      subtitle = paste0(
        serve_label,
        " projected SQS percentile, top ",
        n_players_show,
        " recurring servers with appearances in ",
        min_years_present,
        "+ of 8 years"
      ),
      x = NULL,
      y = "Projected SQS percentile within Wimbledon year",
      color = "Server",
      caption = paste0(
        "Players are selected by average projected SQS percentile among those appearing in ",
        min_years_present,
        "+ Wimbledon years from 2016–2019 and 2021–2024. ",
        "Dot size reflects number of observed first serves. ",
        "Historical scores use the fixed-effect component of the modern SQS model."
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 23),
      plot.subtitle = element_text(size = 14),
      axis.title.y = element_text(size = 13),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(size = 9, hjust = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      size = guide_legend(order = 2)
    )
  
  list(
    plot = p,
    selected_players = selected_players,
    plot_df = plot_df
  )
}

# ------------------------------------------------------------------------------
# Main function for one Wimbledon/gender

process_wimbledon_gender <- function(gender) {
  tag <- paste0(tournament, "_", gender)
  
  message("\n==============================")
  message("Processing ", tag)
  message("==============================")
  
  df_raw <- load_tournament_gender_data(tournament, gender)
  
  if (is.null(df_raw) || nrow(df_raw) == 0) {
    warning("Skipping ", tag, ": no data.")
    return(invisible(NULL))
  }
  
  df_clean <- clean_points(df_raw)
  
  if (nrow(df_clean) == 0) {
    warning("Skipping ", tag, ": no rows after cleaning.")
    return(invisible(NULL))
  }
  
  # --------------------------------------------------------------------------
  # Build pooled player profiles for model fitting
  
  serve_profiles_all <- build_server_profiles(
    df_clean = df_clean,
    serve_num = serve_num,
    group_year = FALSE
  ) %>%
    filter(n_serves >= min_serves_model)
  
  if (nrow(serve_profiles_all) < 10) {
    warning("Skipping ", tag, ": too few servers for model.")
    return(invisible(NULL))
  }
  
  scale_params <- serve_profiles_all %>%
    summarise(
      avg_speed_mu = mean(avg_speed, na.rm = TRUE),
      avg_speed_sd = sd(avg_speed, na.rm = TRUE),
      sd_speed_mu = mean(sd_speed, na.rm = TRUE),
      sd_speed_sd = sd(sd_speed, na.rm = TRUE),
      location_entropy_mu = mean(location_entropy, na.rm = TRUE),
      location_entropy_sd = sd(location_entropy, na.rm = TRUE)
    ) %>%
    mutate(across(everything(), as.numeric))
  
  if (
    any(!is.finite(unlist(scale_params, use.names = FALSE))) ||
    scale_params$avg_speed_sd == 0 ||
    scale_params$sd_speed_sd == 0 ||
    scale_params$location_entropy_sd == 0
  ) {
    warning("Skipping ", tag, ": invalid scale parameters.")
    return(invisible(NULL))
  }
  
  serve_profiles_all_z <- serve_profiles_all %>%
    zscore_with_params(scale_params) %>%
    mutate(modal_location = factor(modal_location))
  
  model_df <- df_clean %>%
    filter(ServeNumber == serve_num) %>%
    inner_join(
      serve_profiles_all_z %>%
        select(
          ServerName,
          avg_speed_z,
          sd_speed_z,
          location_entropy_z,
          modal_location
        ),
      by = "ServerName"
    ) %>%
    select(
      is_efficient,
      ServerName,
      ReturnerName,
      avg_speed_z,
      sd_speed_z,
      location_entropy_z,
      modal_location
    ) %>%
    filter(
      is.finite(avg_speed_z),
      is.finite(sd_speed_z),
      is.finite(location_entropy_z),
      !is.na(modal_location),
      !is.na(is_efficient)
    )
  
  message("Rows in model_df: ", nrow(model_df))
  message("Servers in model_df: ", n_distinct(model_df$ServerName))
  message("Returners in model_df: ", n_distinct(model_df$ReturnerName))
  
  m_sqs <- glmer(
    is_efficient ~ avg_speed_z + sd_speed_z + location_entropy_z + modal_location +
      (1 | ServerName) + (1 | ReturnerName),
    data = model_df,
    family = binomial(),
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )
  
  # --------------------------------------------------------------------------
  # Build player-year profiles and score with fixed-effect projection
  
  serve_profiles_year <- build_server_profiles(
    df_clean = df_clean,
    serve_num = serve_num,
    group_year = TRUE
  ) %>%
    filter(n_serves >= min_serves_plot) %>%
    zscore_with_params(scale_params)
  
  sqs_year <- score_profiles_fixed_effects_only(
    model = m_sqs,
    profiles_year_z = serve_profiles_year
  ) %>%
    group_by(year) %>%
    mutate(
      SQS_FE_centered = SQS_FE_logodds - mean(SQS_FE_logodds, na.rm = TRUE),
      SQS_percentile = percent_rank(SQS_FE_logodds) * 100,
      SQS_rank = min_rank(desc(SQS_FE_logodds))
    ) %>%
    ungroup()
  
  # Save all scored player-years.
  write_csv(
    sqs_year,
    file.path(out_dir, paste0("historical_projected_sqs_", tag, ".csv"))
  )
  
  # --------------------------------------------------------------------------
  # Make combined top-10 plot.
  
  plot_out <- make_top10_same_plot(
    sqs_year = sqs_year,
    tournament = tournament,
    gender = gender,
    min_years_present = min_years_present,
    n_players_show = n_players_show
  )
  
  if (is.null(plot_out)) {
    warning("No combined top-10 plot produced for ", tag)
    return(invisible(sqs_year))
  }
  
  p <- plot_out$plot
  selected_players <- plot_out$selected_players
  plot_df <- plot_out$plot_df
  
  # Save selected player table.
  write_csv(
    selected_players,
    file.path(
      out_dir,
      paste0(
        "selected_top",
        n_players_show,
        "_recurring_servers_",
        tag,
        "_min",
        min_years_present,
        "years.csv"
      )
    )
  )
  
  # Save data used in plot.
  write_csv(
    plot_df,
    file.path(
      out_dir,
      paste0(
        "plot_data_top",
        n_players_show,
        "_same_plot_",
        tag,
        "_min",
        min_years_present,
        "years.csv"
      )
    )
  )
  
  # Save figure.
  ggsave(
    filename = file.path(
      out_dir,
      paste0(
        "top",
        n_players_show,
        "_same_plot_",
        tag,
        "_min",
        min_years_present,
        "years.png"
      )
    ),
    plot = p,
    width = 14,
    height = 8,
    dpi = 320
  )
  
  message("Saved combined top-10 plot for ", tag)
  
  invisible(sqs_year)
}

# ------------------------------------------------------------------------------
# Run Wimbledon men and Wimbledon women

all_results <- list()

for (g in genders) {
  tag <- paste0(tournament, "_", g)
  all_results[[tag]] <- process_wimbledon_gender(g)
}

# Combined output if useful.
combined_sqs_year <- bind_rows(
  lapply(names(all_results), function(tag) {
    x <- all_results[[tag]]
    if (is.null(x)) return(NULL)
    
    parts <- str_split(tag, "_", simplify = TRUE)
    
    x %>%
      mutate(
        tournament = parts[1],
        gender = parts[2]
      )
  })
)

if (nrow(combined_sqs_year) > 0) {
  write_csv(
    combined_sqs_year,
    file.path(out_dir, "historical_projected_sqs_wimbledon_men_women.csv")
  )
}

