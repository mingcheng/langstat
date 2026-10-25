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

layout_treemap_labels <- function(data, measurement_plot, font_family, min_font_size,
                                  style, outside = character(), width = 10, height = 6) {
  # Measure the real plot panel and font metrics without creating a preview file.
  grDevices::pdf(NULL, width = width, height = height)
  device <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(device), add = TRUE)
  grid::grid.newpage()
  table <- ggplot2::ggplotGrob(measurement_plot)
  panel <- table$layout[table$layout$name == "panel", ]
  panel_width <- grid::convertWidth(
    grid::unit(width, "in") - sum(table$widths[-panel$l]), "pt",
    valueOnly = TRUE
  )
  panel_height <- grid::convertHeight(
    grid::unit(height, "in") - sum(table$heights[-panel$t]), "pt",
    valueOnly = TRUE
  )
  if (!is.finite(panel_width) || !is.finite(panel_height) ||
    panel_width <= 0 || panel_height <= 0) {
    stop("The treemap title, legend and margins cannot fit with a usable plot panel.")
  }
  aspect <- panel_width / panel_height
  layout <- treemapify::treemapify(
    data,
    area = "final_score", start = "topleft", xlim = c(0, aspect)
  )
  layout$xmin <- layout$xmin / aspect
  layout$xmax <- layout$xmax / aspect
  layout$percentage_font_size <- min_font_size + (layout$font_size - min_font_size) / 2
  layout$name_label <- layout$language
  layout$percentage_label <- paste0(layout$percentage, "%")
  layout$label_layout <- ifelse(layout$language %in% outside, "legend", "stacked")
  for (field in c(
    "name_x", "name_y", "name_hjust", "name_width", "name_height",
    "percentage_x", "percentage_y", "percentage_hjust", "percentage_width", "percentage_height"
  )) {
    layout[[field]] <- numeric(nrow(layout))
  }

  measure <- function(label, size, face) {
    grob <- grid::textGrob(label, gp = grid::gpar(
      fontfamily = font_family, fontface = face, fontsize = size, lineheight = 1.05
    ))
    c(
      width = grid::convertWidth(grid::grobWidth(grob), "pt", valueOnly = TRUE),
      height = grid::convertHeight(
        grid::grobAscent(grob) + grid::grobDescent(grob), "pt",
        valueOnly = TRUE
      )
    )
  }
  wrap_name <- function(label, size, available_width) {
    if (measure(label, size, "bold")["width"] <= available_width) {
      return(label)
    }
    lines <- character()
    line <- ""
    words <- gsub("([[:lower:][:digit:]])([[:upper:]])", "\\1 \\2", label)
    for (word in strsplit(words, "[[:space:]]+")[[1]]) {
      candidate <- if (nzchar(line)) paste(line, word) else word
      if (measure(candidate, size, "bold")["width"] <= available_width) {
        line <- candidate
        next
      }
      if (nzchar(line)) {
        lines <- c(lines, line)
        line <- ""
      }
      for (character in strsplit(word, "", fixed = TRUE)[[1]]) {
        candidate <- paste0(line, character)
        if (nzchar(line) && measure(candidate, size, "bold")["width"] > available_width) {
          lines <- c(lines, line)
          line <- character
        } else {
          line <- candidate
        }
      }
    }
    paste(c(lines, line), collapse = "\n")
  }

  padding <- 6
  gap <- 6
  for (i in seq_len(nrow(layout))) {
    cell <- layout[i, ]
    if (cell$label_layout == "legend") {
      next
    }
    available_width <- (cell$xmax - cell$xmin) * panel_width - 2 * padding
    cell_height <- (cell$ymax - cell$ymin) * panel_height
    available_height <- cell_height - 2 * min(padding, cell_height * 0.15)
    name <- measure(cell$name_label, cell$font_size, "bold")
    percentage <- measure(cell$percentage_label, cell$percentage_font_size, "plain")
    inline_fits <- name["width"] + gap + percentage["width"] <= available_width &&
      max(name["height"], percentage["height"]) <= available_height
    inline <- inline_fits && (
      available_width / available_height >= 3 ||
        name["height"] + gap + percentage["height"] > available_height
    )
    if (!inline && name["width"] > available_width) {
      layout$name_label[i] <- wrap_name(cell$name_label, cell$font_size, available_width)
      name <- measure(layout$name_label[i], cell$font_size, "bold")
    }
    if (!inline && (max(name["width"], percentage["width"]) > available_width ||
      name["height"] + gap + percentage["height"] > available_height)) {
      layout$label_layout[i] <- "legend"
      next
    }
    center_x <- (cell$xmin + cell$xmax) / 2
    center_y <- (cell$ymin + cell$ymax) / 2
    layout$name_width[i] <- name["width"] / panel_width
    layout$name_height[i] <- name["height"] / panel_height
    layout$percentage_width[i] <- percentage["width"] / panel_width
    layout$percentage_height[i] <- percentage["height"] / panel_height
    if (inline) {
      layout$label_layout[i] <- "inline"
      layout$name_x[i] <- cell$xmin + padding / panel_width
      layout$percentage_x[i] <- cell$xmax - padding / panel_width
      layout$name_y[i] <- layout$percentage_y[i] <- center_y
      layout$name_hjust[i] <- 0
      layout$percentage_hjust[i] <- 1
    } else {
      layout$name_x[i] <- layout$percentage_x[i] <- center_x
      layout$name_y[i] <- center_y + (percentage["height"] + gap) / (2 * panel_height)
      layout$percentage_y[i] <- center_y - (name["height"] + gap) / (2 * panel_height)
      layout$name_hjust[i] <- layout$percentage_hjust[i] <- 0.5
    }
  }
  legend <- NULL
  relocated <- layout[layout$label_layout == "legend", , drop = FALSE]
  if (nrow(relocated) > 0) {
    names <- lapply(seq_len(nrow(relocated)), function(i) {
      measure(relocated$language[i], relocated$font_size[i], "bold")
    })
    percentages <- lapply(seq_len(nrow(relocated)), function(i) {
      measure(relocated$percentage_label[i], relocated$percentage_font_size[i], "plain")
    })
    name_widths <- vapply(names, function(size) size["width"], numeric(1))
    percentage_widths <- vapply(percentages, function(size) size["width"], numeric(1))
    marker <- 10
    column_gap <- 16
    required_width <- marker + 3 * gap + name_widths + percentage_widths
    columns <- if (nrow(relocated) > 1 && 2 * max(required_width) + column_gap <= panel_width) 2L else 1L
    column_width <- (panel_width - (columns - 1) * column_gap) / columns
    labels <- relocated$language
    for (i in seq_len(nrow(relocated))) {
      available_width <- column_width - marker - 3 * gap - percentage_widths[i]
      labels[i] <- wrap_name(labels[i], relocated$font_size[i], available_width)
      names[[i]] <- measure(labels[i], relocated$font_size[i], "bold")
      if (names[[i]]["width"] > available_width) {
        stop(
          "Treemap legend text for '", relocated$language[i], "' cannot fit. ",
          "Lower TREEMAP_MIN_FONT_SIZE/TREEMAP_MAX_FONT_SIZE."
        )
      }
    }
    name_widths <- vapply(names, function(size) size["width"], numeric(1))
    heights <- pmax(
      vapply(names, function(size) size["height"], numeric(1)),
      vapply(percentages, function(size) size["height"], numeric(1))
    )
    rows <- ceiling(seq_len(nrow(relocated)) / columns)
    row_heights <- vapply(split(heights, rows), max, numeric(1)) + 8
    total_height <- sum(row_heights)
    x <- ((seq_len(nrow(relocated)) - 1) %% columns) * (column_width + column_gap)
    y <- (total_height - cumsum(row_heights) + row_heights / 2)[rows]
    legend <- list(
      width = grid::unit(panel_width, "pt"),
      height = grid::unit(total_height, "pt"),
      grob = grid::grobTree(
        grid::rectGrob(
          x = grid::unit(x + marker / 2, "pt"), y = grid::unit(y, "pt"),
          width = grid::unit(marker, "pt"), height = grid::unit(marker, "pt"),
          gp = grid::gpar(fill = unname(style$colors[relocated$language]), col = NA)
        ),
        grid::textGrob(
          labels,
          x = grid::unit(x + marker + gap, "pt"), y = grid::unit(y, "pt"),
          hjust = 0, vjust = 0.5, name = "treemap-legend-names",
          gp = grid::gpar(
            fontfamily = font_family, fontface = "bold", fontsize = relocated$font_size,
            col = style$foreground, lineheight = 1.05
          )
        ),
        grid::textGrob(
          relocated$percentage_label,
          x = grid::unit(x + marker + 2 * gap + name_widths, "pt"), y = grid::unit(y, "pt"),
          hjust = 0, vjust = 0.5, name = "treemap-legend-percentages",
          gp = grid::gpar(
            fontfamily = font_family, fontsize = relocated$percentage_font_size,
            col = style$muted, lineheight = 1.05
          )
        ),
        vp = grid::viewport(
          width = grid::unit(panel_width, "pt"), height = grid::unit(total_height, "pt")
        )
      )
    )
  }
  list(data = layout, legend = legend)
}

treemap_overflow_guide <- function(legend) {
  if (is.null(legend)) {
    return(NULL)
  }
  ggplot2::guides(overflow = ggplot2::guide_custom(
    legend$grob,
    width = legend$width, height = legend$height, position = "bottom"
  ))
}

#' Generate Treemap Visualization
#'
#' Creates a treemap visualization of repository language distribution
#' using ggplot2 and treemapify packages.
#'
#' @param username GitHub username
#' @param repos Data frame with language statistics
#' @param dir_name Output directory path
#' @param summary Activity window and distinct repository counts
#' @return The generated ggplot object, invisibly
#' @export
generate_treemap <- function(username, repos, dir_name, summary) {
  if (utils::packageVersion("ggplot2") < "3.5.0") {
    stop("Treemap typography requires ggplot2 >= 3.5.0; update ggplot2 first.")
  }
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
    repos,
    max_languages = 12, min_percentage = min_percentage, group_other = FALSE
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
  font_family <- setup_visualization_font()
  style <- visualization_style(repos$language)

  # Prepare data for treemap
  subtitle <- profile_chart_subtitle(username, summary)
  today <- format(as.Date(summary$window_end), "%Y%m%d")

  message(
    "Displaying ", nrow(repos_filtered), " treemap blocks (threshold: ",
    min_percentage, "%, at most 12 languages)"
  )

  presentation <- list(
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE),
    ggplot2::labs(
      title = "Development languages",
      subtitle = subtitle,
      caption = paste(
        "Default-branch commits + merged PRs. Repository language mix, not proficiency.",
        sprintf("Showing languages >= %g%% (up to 12); shares use all scored languages", min_percentage),
        sep = "\n"
      )
    ),
    ggplot2::theme_void(base_family = font_family),
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0,
        size = 20,
        face = "bold",
        colour = style$foreground,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = 11,
        colour = style$muted,
        lineheight = 1.2,
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 9,
        colour = style$muted,
        lineheight = 1.2,
        margin = ggplot2::margin(t = 12)
      ),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = style$background, colour = NA),
      plot.margin = ggplot2::margin(20, 20, 18, 20),
      legend.position = "bottom",
      legend.justification = "left",
      legend.margin = ggplot2::margin(8, 0, 0, 0)
    )
  )
  outside <- character()
  legend <- NULL
  repeat {
    result <- layout_treemap_labels(
      repos_filtered, ggplot2::ggplot() +
        presentation +
        treemap_overflow_guide(legend),
      font_family, min_font_size, style, outside
    )
    layout <- result$data
    legend <- result$legend
    relocated <- layout$language[layout$label_layout == "legend"]
    if (setequal(outside, relocated)) {
      break
    }
    # Keep relocated labels outside while the legend changes the remaining panel height.
    outside <- relocated
  }
  if (length(outside) > 0) {
    message("Moving ", length(outside), " small-block labels to the full-size legend.")
  }
  inside <- layout[layout$label_layout != "legend", , drop = FALSE]
  p <- ggplot2::ggplot(layout, ggplot2::aes(fill = language)) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      colour = style$background,
      linewidth = 1.1
    ) +
    ggplot2::geom_text(
      data = inside,
      ggplot2::aes(
        x = name_x, y = name_y, label = name_label,
        size = font_size / ggplot2::.pt, hjust = name_hjust
      ),
      family = font_family, fontface = "bold",
      colour = style$foreground, lineheight = 1.05
    ) +
    ggplot2::geom_text(
      data = inside,
      ggplot2::aes(
        x = percentage_x, y = percentage_y, label = percentage_label,
        size = percentage_font_size / ggplot2::.pt, hjust = percentage_hjust
      ),
      family = font_family,
      colour = style$muted, lineheight = 1.05
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_manual(values = style$colors, guide = "none") +
    presentation +
    treemap_overflow_guide(legend)

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
