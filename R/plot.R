#!#
# Copyright (c) 2025 Hangzhou Guanwaii Technology Co., Ltd.
#
# This source code is licensed under the MIT License,
# which is located in the LICENSE file in the source tree's root directory.
#
# File: plot.R
# Author: mingcheng <mingcheng@apache.org>
# File Created: 2025-10-25 14:59:34
#
# Modified By: mingcheng <mingcheng@apache.org>
# Last Modified: 2025-10-25 17:08:48
##

#' Process and Analyze Repository Data
#'
#' Calculate weighted language distribution scores based on repository
#' push recency, stars, forks, and time-weighted repository count.
#'
#' @param repos_data Data frame containing repository information from GitHub API
#' @param reference_date Date used to measure inactivity (defaults to today)
#' @return Data frame with aggregated language statistics and scores
#' @export
plot_repos <- function(repos_data, reference_date = Sys.Date()) {
  if (!inherits(reference_date, "Date") || length(reference_date) != 1L ||
      is.na(reference_date)) {
    stop("reference_date must be a single, non-missing Date.")
  }
  activity_date <- as.Date(rep(NA_character_, nrow(repos_data)))
  if ("pushed_at" %in% names(repos_data)) {
    activity_date <- as.Date(repos_data$pushed_at, format = "%Y-%m-%d")
  }
  missing_push <- is.na(activity_date)
  if (any(missing_push)) {
    warning("Missing or invalid pushed_at; falling back to updated_at where available.")
    if ("updated_at" %in% names(repos_data)) {
      fallback_date <- as.Date(repos_data$updated_at, format = "%Y-%m-%d")
      activity_date[missing_push] <- fallback_date[missing_push]
    }
  }
  if (anyNA(activity_date)) {
    warning("Repositories without a valid activity date receive zero weight.")
  }
  repos_data$activity_date <- activity_date

  # Print data overview for debugging
  message("Total repositories: ", nrow(repos_data))
  message("Language distribution:")
  print(table(repos_data$language, useNA = "ifany"))
  cat("\n")

  # Calculate weighted scores and aggregate by language
  repos_df <- repos_data |>
    dplyr::mutate(
      days_since_update = pmax(0, as.numeric(reference_date - activity_date)),
      # One year of inactivity halves every score contribution.
      date_weight = dplyr::if_else(
        is.na(days_since_update), 0, 2^(-days_since_update / 365)
      ),
      # Log-scale to reduce impact of large values
      star_score = log1p(stargazers_count),
      fork_score = log1p(forks_count),
      # Weighted score: priority = update time > repo count > stars > forks
      # Note: repo count is calculated after grouping
      weighted_score = (star_score * 0.2 + fork_score * 0.1 + 1.5) * date_weight
    ) |>
    # Group by language and aggregate
    dplyr::group_by(language) |>
    dplyr::summarise(
      total_score = sum(weighted_score, na.rm = TRUE),
      repo_count = dplyr::n(),
      total_stars = sum(stargazers_count, na.rm = TRUE),
      total_forks = sum(forks_count, na.rm = TRUE),
      avg_days_since_update = if (all(is.na(days_since_update))) {
        NA_real_
      } else {
        mean(days_since_update, na.rm = TRUE)
      },
      avg_date_weight = mean(date_weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Filter out NA languages
    dplyr::filter(!is.na(language)) |>
    # Add repository count weight (second most important factor)
    dplyr::mutate(
      repo_count_score = log1p(repo_count) * 3.5 * avg_date_weight,
      # The count bonus must decay too, or inactive languages retain a score floor.
      final_score = total_score + repo_count_score
    ) |>
    dplyr::arrange(dplyr::desc(final_score))

  if (nrow(repos_df) == 0 || !is.finite(sum(repos_df$final_score)) ||
      sum(repos_df$final_score) <= 0) {
    stop("No positive language scores; check repository languages and activity dates.")
  }

  # Print statistical results
  message("Language statistics:")
  print(repos_df)
  cat("\n")

  # Calculate percentages and create labels
  repos_df <- repos_df |>
    dplyr::mutate(
      percentage = round(final_score / sum(final_score) * 100, 1),
      label = paste0(language, " (", percentage, "%)")
    )

  return(repos_df)
}
