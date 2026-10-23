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

#' Generate Treemap Visualization
#'
#' Creates a treemap visualization of repository language distribution
#' using ggplot2 and treemapify packages.
#'
#' @param username GitHub username
#' @param repos Data frame with language statistics
#' @param dir_name Output directory path
#' @param json_data Raw repository data
#' @return The generated ggplot object, invisibly
#' @export
generate_treemap <- function(username, repos, dir_name, json_data) {
  min_percentage <- suppressWarnings(as.numeric(
    Sys.getenv("TREEMAP_MIN_PERCENTAGE", unset = "2")
  ))
  if (!is.finite(min_percentage) || min_percentage < 0 || min_percentage > 100) {
    stop("TREEMAP_MIN_PERCENTAGE must be a finite number between 0 and 100.")
  }
  min_font_size <- suppressWarnings(as.numeric(
    Sys.getenv("TREEMAP_MIN_FONT_SIZE", unset = "16")
  ))
  max_font_size <- suppressWarnings(as.numeric(
    Sys.getenv("TREEMAP_MAX_FONT_SIZE", unset = "32")
  ))
  if (!is.finite(min_font_size) || min_font_size <= 0 ||
      !is.finite(max_font_size) || max_font_size <= 0) {
    stop("TREEMAP_MIN_FONT_SIZE and TREEMAP_MAX_FONT_SIZE must be positive finite numbers (pt).")
  }
  if (min_font_size > max_font_size) {
    stop("TREEMAP_MIN_FONT_SIZE must not exceed TREEMAP_MAX_FONT_SIZE.")
  }
  repos_filtered <- prepare_chart_data(
    repos, max_languages = 12, min_percentage = min_percentage, group_other = FALSE
  )
  share_range <- range(repos_filtered$exact_percentage)
  if (share_range[1] == share_range[2]) {
    repos_filtered$font_size <- min_font_size + (max_font_size - min_font_size) / 2
  } else {
    relative_share <- (repos_filtered$exact_percentage - share_range[1]) / diff(share_range)
    repos_filtered$font_size <- pmin(max_font_size, pmax(
      min_font_size, min_font_size + relative_share * (max_font_size - min_font_size)
    ))
  }
  layout <- treemapify::treemapify(repos_filtered, area = "final_score")

  font_family <- setup_visualization_font()
  style <- visualization_style(repos$language)

  # Prepare data for treemap
  total_repos <- nrow(json_data)
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  today <- format(Sys.Date(), "%Y%m%d")

  message(
    "Displaying ", nrow(repos_filtered), " treemap blocks (threshold: ",
    min_percentage, "%, at most 12 languages)"
  )

  # Create the treemap plot
  p <- ggplot2::ggplot(layout, ggplot2::aes(
    fill = language,
    label = paste0(language, "\n", percentage, "%")
  )) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      colour = style$background,
      linewidth = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = (xmin + xmax) / 2,
        y = (ymin + ymax) / 2,
        size = font_size / ggplot2::.pt
      ),
      family = font_family,
      colour = style$foreground
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_manual(values = style$colors) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(
      title = "Activity-weighted Language Mix",
      subtitle = sprintf(
        "GitHub: %s | Scored Repos: %d/%d | Updated on %s",
        username, sum(repos$repo_count), total_repos, date_str
      ),
      caption = sprintf("Showing languages >= %g%% (up to 12); shares use all scored languages", min_percentage)
    ) +
    ggplot2::theme_void(base_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 20,
        face = "bold",
        colour = style$foreground
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 10,
        colour = style$muted
      ),
      plot.caption = ggplot2::element_text(
        size = 9,
        colour = style$muted,
        margin = ggplot2::margin(t = 10)
      ),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = style$background, colour = NA),
      plot.margin = ggplot2::margin(40, 20, 20, 20),
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
    ggplot2::ggsave(
      svg_file,
      plot = p,
      width = 10,
      height = 6,
      bg = style$background
    )
    message("Treemap SVG saved to ", svg_file)
  }

  # Save PNG versions
  for (png_file in treemap_files$png) {
    ggplot2::ggsave(
      png_file,
      plot = p,
      width = 10,
      height = 6,
      dpi = 120,
      bg = style$background
    )
    message("Treemap PNG saved to ", png_file)
  }

  invisible(p)
}
