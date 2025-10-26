#!#
# Copyright (c) 2025 Hangzhou Guanwaii Technology Co., Ltd.
#
# This source code is licensed under the MIT License,
# which is located in the LICENSE file in the source tree's root directory.
#
# File: treemap.R
# Author: mingcheng <mingcheng@apache.org>
# File Created: 2025-10-25 23:41:07
#
# Modified By: mingcheng <mingcheng@apache.org>
# Last Modified: 2025-10-26 11:18:57
##

# Load required packages
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("treemapify", quietly = TRUE)) {
  install.packages("treemapify")
  library(treemapify)
}

if (!require("showtext", quietly = TRUE)) {
  install.packages("showtext")
  library(showtext)
}

if (!require("svglite", quietly = TRUE)) {
  install.packages("svglite")
  library(svglite)
}

#' Generate Treemap Visualization
#'
#' Creates a treemap visualization of repository language distribution
#' using ggplot2 and treemapify packages.
#'
#' @param username GitHub username
#' @param repos Data frame with language statistics
#' @param dir_name Output directory path
#' @param json_data Raw repository data
#' @export
generate_treemap <- function(username, repos, dir_name, json_data) {
  # Setup font for visualization
  tryCatch(
    font_add("FiraCode", regular = "./assets/FiraCode.ttf"),
    error = function(e) {
      warning("Failed to load custom font, using default: ", e$message)
    }
  )

  # Enable showtext
  showtext_auto()

  # Prepare data for treemap
  total_repos <- nrow(json_data)
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  today <- format(Sys.Date(), "%Y%m%d")

  # Filter data: only keep languages with percentage >= 1%
  repos_filtered <- repos[repos$percentage >= 1, ]

  # If too many languages remain, keep only top 12
  if (nrow(repos_filtered) > 12) {
    repos_filtered <- head(repos_filtered, 12)
  }

  message("Displaying ", nrow(repos_filtered), " languages in treemap (filtered >= 1%)")

  # Create the treemap plot
  p <- ggplot(repos_filtered, aes(
    area = final_score,
    fill = language,
    # label = paste0(language, "\n", percentage, "%\n(", repo_count, " repos)")
    label = paste0(language, "\n", percentage, "%")
  )) +
    geom_treemap(colour = "white", size = 2) +
    geom_treemap_text(
      aes(size = percentage),
      place = "centre",
      family = "FiraCode",
      min.size = 12
    ) +
    scale_size_continuous(range = c(12, 36)) +
    labs(
      title = "Repository Distribution by Language",
      subtitle = sprintf(
        "GitHub User: %s | Total Repositories: %d | Updated on %s",
        username, total_repos, date_str
      )
    ) +
    theme_minimal(base_family = "FiraCode") +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 24
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 10,
        colour = "#777777"
      ),
      legend.position = "none"
    )

  # Define treemap file paths
  treemap_files <- list(
    svg = c(
      file.path(dir_name, paste0("treemap_", today, ".svg")),
      file.path(dir_name, "treemap_latest.svg")
    ),
    png = c(
      file.path(dir_name, paste0("treemap_", today, ".png")),
      file.path(dir_name, "treemap_latest.png")
    )
  )

  # Save SVG versions
  for (svg_file in treemap_files$svg) {
    ggsave(
      svg_file,
      plot = p,
      width = 10,
      height = 6,
      bg = "white"
    )
    message("Treemap SVG saved to ", svg_file)
  }

  # Save PNG versions
  for (png_file in treemap_files$png) {
    ggsave(
      png_file,
      plot = p,
      width = 10,
      height = 6,
      dpi = 120,
      bg = "white"
    )
    message("Treemap PNG saved to ", png_file)
  }
}
