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
#' update recency, stars, forks, and repository count.
#'
#' @param repos_data Data frame containing repository information from GitHub API
#' @return Data frame with aggregated language statistics and scores
#' @export
plot_repos <- function(repos_data) {
  # Print data overview for debugging
  message("Total repositories: ", nrow(repos_data))
  message("Language distribution:")
  print(table(repos_data$language, useNA = "ifany"))
  cat("\n")

  # Calculate weighted scores and aggregate by language
  repos_df <- repos_data |>
    dplyr::mutate(
      # Calculate update recency weight (most important factor)
      days_since_update = as.numeric(Sys.Date() - as.Date(updated_at)),
      # Exponential decay: 1 year decay constant
      date_weight = exp(-days_since_update / 365),
      # Log-scale to reduce impact of large values
      star_score = log1p(stargazers_count),
      fork_score = log1p(forks_count),
      # Weighted score: priority = update time > repo count > stars > forks
      # Note: repo count is calculated after grouping
      weighted_score = (star_score * 0.2 + fork_score * 0.1 + 1.5) * (date_weight^1.5)
    ) |>
    # Group by language and aggregate
    dplyr::group_by(language) |>
    dplyr::summarise(
      total_score = sum(weighted_score, na.rm = TRUE),
      repo_count = dplyr::n(),
      total_stars = sum(stargazers_count, na.rm = TRUE),
      total_forks = sum(forks_count, na.rm = TRUE),
      avg_days_since_update = mean(days_since_update, na.rm = TRUE),
      avg_date_weight = mean(date_weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Filter out NA languages
    dplyr::filter(!is.na(language)) |>
    # Add repository count weight (second most important factor)
    dplyr::mutate(
      repo_count_score = log1p(repo_count) * 3.5,
      # Final score = base score (with time weight) + repo count bonus
      final_score = total_score + repo_count_score
    ) |>
    dplyr::arrange(dplyr::desc(final_score))

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
