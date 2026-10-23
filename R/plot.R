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
#' Sum bounded repository contributions based on push recency, archived
#' status, stars and fork counts. Forked, disabled and empty repos are excluded.
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
  if (!is.data.frame(repos_data)) {
    stop("repos_data must be a data frame.")
  }
  required_fields <- c(
    "language", "stargazers_count", "forks_count",
    "fork", "archived", "disabled", "size"
  )
  missing_fields <- setdiff(required_fields, names(repos_data))
  if (length(missing_fields) > 0) {
    stop(
      "Missing repository fields: ", paste(missing_fields, collapse = ", "),
      ". Refresh the cached GitHub data."
    )
  }
  for (field in c("fork", "archived", "disabled")) {
    if (!is.logical(repos_data[[field]]) || anyNA(repos_data[[field]])) {
      stop(field, " must contain non-missing logical values.")
    }
  }
  for (field in c("stargazers_count", "forks_count", "size")) {
    values <- repos_data[[field]]
    if (!is.numeric(values) || any(!is.finite(values)) || any(values < 0)) {
      stop(field, " must contain finite, non-negative numbers.")
    }
  }

  total_repos <- nrow(repos_data)
  repos_data <- repos_data |>
    dplyr::filter(
      !fork, !disabled, size > 0,
      !is.na(language), nzchar(trimws(language))
    )
  message(
    "Scoring ", nrow(repos_data), " of ", total_repos,
    " repositories (excluding forks, disabled/empty repos and missing languages)."
  )

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
      archive_weight = dplyr::if_else(archived, 0.5, 1),
      activity_weight = date_weight * archive_weight,
      star_factor = pmin(log1p(stargazers_count) / log1p(200), 1),
      fork_factor = pmin(log1p(forks_count) / log1p(50), 1),
      base_contribution = 0.7 * activity_weight,
      star_contribution = 0.2 * star_factor * activity_weight,
      fork_contribution = 0.1 * fork_factor * activity_weight,
      weighted_score = base_contribution + star_contribution + fork_contribution
    ) |>
    # Group by language and aggregate
    dplyr::group_by(language) |>
    dplyr::summarise(
      total_score = sum(weighted_score),
      base_score = sum(base_contribution),
      star_score = sum(star_contribution),
      fork_score = sum(fork_contribution),
      repo_count = dplyr::n(),
      archived_repo_count = sum(archived),
      effective_repo_count = sum(activity_weight),
      total_stars = sum(stargazers_count),
      total_forks = sum(forks_count),
      avg_days_since_update = if (all(is.na(days_since_update))) {
        NA_real_
      } else {
        mean(days_since_update, na.rm = TRUE)
      },
      avg_date_weight = mean(date_weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Summing contributions already accounts for repository count.
    dplyr::mutate(final_score = total_score) |>
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
