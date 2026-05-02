rm(list = ls())

library(tidyverse)
library(data.table)
library(lme4)
library(ggrepel)
library(scales)

# ------------------------------------------------------------------------------
# Configuration

years_keep <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)

tournaments <- c("wimb", "us")
genders <- c("men", "women")

serve_num <- 1
serve_label <- ifelse(serve_num == 1, "First Serve", "Second Serve")

min_serves_model <- 20
min_serves_plot <- 5

input_dir <- "data/processed/subset"
out_dir <- file.path("figures", "bbc_visuals", "historical_sqs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Suggested players for trajectory visual
players_men <- c(
  "Roger Federer",
  "Rafael Nadal",
  "Novak Djokovic",
  "Andy Roddick",
  "John Isner",
  "Nick Kyrgios",
  "Carlos Alcaraz"
)

players_women <- c(
  "Serena Williams",
  "Venus Williams",
  "Maria Sharapova",
  "Petra Kvitova",
  "Aryna Sabalenka",
  "Elena Rybakina"
)

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
    
    fread(f, colClasses = list(
      character = c(
        "match_id", "ElapsedTime",
        "player1", "player2",
        "ServeWidth", "ServeDepth",
        "P1Score", "P2Score",
        "state",
        "player1_name", "player2_name"
      )
    )) %>%
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

make_visual_1_trajectories <- function(sqs_year, tournament, gender) {
  tournament_label <- tournament_label_for(tournament)
  gender_label <- gender_label_for(gender)
  
  players_to_show <- if (gender == "men") players_men else players_women
  
  plot_df <- sqs_year %>%
    filter(ServerName %in% players_to_show)
  
  if (nrow(plot_df) == 0) {
    warning("No trajectory data for ", tournament, " ", gender)
    return(NULL)
  }
  
  p <- ggplot(plot_df, aes(x = year, y = SQS_percentile)) +
    geom_line(linewidth = 1.1, alpha = 0.85) +
    geom_point(aes(size = n_serves), alpha = 0.9) +
    facet_wrap(~ ServerName, ncol = ifelse(gender == "men", 4, 3)) +
    scale_x_continuous(
      breaks = years_keep,
      limits = range(years_keep)
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = c(0, 50, 100),
      labels = function(x) paste0(x, "th")
    ) +
    scale_size_continuous(
      name = "Observed\nfirst serves",
      range = c(2, 6)
    ) +
    labs(
      title = paste0("Projected Serve Quality Over Time: ", tournament_label, " ", gender_label),
      subtitle = paste0(
        serve_label,
        " projected SQS percentile within tournament-year, ",
        min(years_keep), "–", max(years_keep)
      ),
      x = NULL,
      y = "Projected SQS percentile",
      caption = paste0(
        "Historical scores use the fixed-effect component of the modern SQS model. ",
        "Higher percentiles indicate stronger serve-driven short-point advantage relative to the same tournament-year field."
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(size = 14),
      strip.text = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  p
}

make_visual_2_distribution <- function(sqs_year, tournament, gender) {
  tournament_label <- tournament_label_for(tournament)
  gender_label <- gender_label_for(gender)
  
  p <- ggplot(sqs_year, aes(x = factor(year), y = SQS_FE_centered)) +
    geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
    labs(
      title = paste0("Serve-Quality Landscape: ", tournament_label, " ", gender_label),
      subtitle = paste0(
        "Distribution of projected ", serve_label,
        " SQS across qualifying player-years"
      ),
      x = NULL,
      y = "Projected SQS, centered within year",
      caption = paste0(
        "Boxes show the player-year distribution. Values are centered within each tournament-year; ",
        "positive values indicate above-average projected serve quality for that year."
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(size = 14),
      panel.grid.minor = element_blank()
    )
  
  p
}

make_visual_3_top10_average <- function(sqs_year, tournament, gender) {
  tournament_label <- tournament_label_for(tournament)
  gender_label <- gender_label_for(gender)
  
  top10_by_year <- sqs_year %>%
    group_by(year) %>%
    arrange(desc(SQS_FE_centered), .by_group = TRUE) %>%
    slice_head(n = 10) %>%
    summarise(
      top10_avg_sqs = mean(SQS_FE_centered, na.rm = TRUE),
      top10_sd_sqs = sd(SQS_FE_centered, na.rm = TRUE),
      n_top = n(),
      .groups = "drop"
    )
  
  p <- ggplot(top10_by_year, aes(x = year, y = top10_avg_sqs)) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(
        ymin = top10_avg_sqs - top10_sd_sqs,
        ymax = top10_avg_sqs + top10_sd_sqs
      ),
      width = 0.15,
      alpha = 0.45
    ) +
    scale_x_continuous(breaks = years_keep) +
    labs(
      title = paste0("How the Top End of Serving Has Shifted: ", tournament_label, " ", gender_label),
      subtitle = paste0(
        "Average projected ", serve_label,
        " SQS among the top 10 servers each year"
      ),
      x = NULL,
      y = "Top-10 average projected SQS",
      caption = paste0(
        "Top 10 is defined within each tournament-year. Error bars show ±1 SD among the top 10 servers."
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(size = 14),
      panel.grid.minor = element_blank()
    )
  
  p
}

# ------------------------------------------------------------------------------
# Main function for one tournament/gender

process_tournament_gender <- function(tournament, gender) {
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
  
  scale_param_values <- unlist(scale_params, use.names = FALSE)
  
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
        select(ServerName, avg_speed_z, sd_speed_z, location_entropy_z, modal_location),
      by = "ServerName"
    ) %>%
    select(
      is_efficient, ServerName, ReturnerName,
      avg_speed_z, sd_speed_z, location_entropy_z, modal_location
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
  
  # Save data behind visuals
  write_csv(
    sqs_year,
    file.path(out_dir, paste0("historical_projected_sqs_", tag, ".csv"))
  )
  
  # --------------------------------------------------------------------------
  # Make and save visuals
  
  p1 <- make_visual_1_trajectories(sqs_year, tournament, gender)
  p2 <- make_visual_2_distribution(sqs_year, tournament, gender)
  p3 <- make_visual_3_top10_average(sqs_year, tournament, gender)
  
  if (!is.null(p1)) {
    ggsave(
      filename = file.path(out_dir, paste0("visual1_player_trajectories_", tag, ".png")),
      plot = p1,
      width = 14,
      height = ifelse(gender == "men", 8, 8),
      dpi = 320
    )
  }
  
  if (!is.null(p2)) {
    ggsave(
      filename = file.path(out_dir, paste0("visual2_field_distribution_", tag, ".png")),
      plot = p2,
      width = 12,
      height = 7,
      dpi = 320
    )
  }
  
  if (!is.null(p3)) {
    ggsave(
      filename = file.path(out_dir, paste0("visual3_top10_average_", tag, ".png")),
      plot = p3,
      width = 12,
      height = 7,
      dpi = 320
    )
  }
  
  message("Saved visuals for ", tag)
  
  invisible(sqs_year)
}

# ------------------------------------------------------------------------------
# Run all tournament/gender combinations

all_results <- list()

for (t in tournaments) {
  for (g in genders) {
    tag <- paste0(t, "_", g)
    all_results[[tag]] <- process_tournament_gender(t, g)
  }
}

# Combined output if useful
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
    file.path(out_dir, "historical_projected_sqs_all.csv")
  )
}