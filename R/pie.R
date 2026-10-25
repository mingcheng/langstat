#!#
# Copyright (c) 2025 Hangzhou Guanwaii Technology Co., Ltd.
#
# This source code is licensed under the MIT License,
# which is located in the LICENSE file in the source tree's root directory.
#
# File: pie.R
# Author: mingcheng <mingcheng@apache.org>
# File Created: 2025-10-25 23:36:10
#
# Modified By: mingcheng <mingcheng@apache.org>
# Last Modified: 2025-10-26 11:19:07
##

generate_pie_chart <- function(username, repos, dir_name, summary) {
  font_family <- setup_visualization_font()
  style <- visualization_style(c(repos$language, "Other"))
  chart_data <- prepare_chart_data(repos)
  subtitle <- profile_chart_subtitle(username, summary)

  # Define function to generate chart
  generate_chart <- function(repos, colors) {
    par(
      bg = style$background,
      col = style$foreground,
      fg = style$foreground,
      mai = c(0.5, 0.5, 0.8, 0.5),
      lwd = 2,
      family = font_family
    )

    pie(
      repos$final_score,
      labels = repos$label,
      col = colors,
      border = style$background,
      main = paste0(
        "Public Development Language Profile\n", subtitle
      ),
      cex = 1,
      cex.main = 0.8,
      col.main = style$muted,
      font.main = 2,
      radius = 1
    )
    mtext(
      "Default-branch commits + merged PRs; repository languages are a proxy, not proficiency.",
      side = 1, line = 1, cex = 0.65, col = style$muted
    )
  }

  # Generate pie charts in multiple formats
  colors <- unname(style$colors[chart_data$language])
  today <- format(as.Date(summary$window_end), "%Y%m%d")

  # Define chart files
  chart_files <- list(
    svg = c(
      file.path(dir_name, paste0("piechart_", today, ".svg")),
      file.path(dir_name, "latest.svg")
    ),
    png = c(
      file.path(dir_name, paste0("piechart_", today, ".png")),
      file.path(dir_name, "latest.png")
    )
  )

  # Generate SVG charts
  for (svg_file in chart_files$svg) {
    svglite::svglite(svg_file, width = 10, height = 6, bg = style$background)
    generate_chart(chart_data, colors)
    dev.off()
    message("SVG chart saved to ", svg_file)
  }

  # Generate PNG charts
  for (png_file in chart_files$png) {
    png(png_file, width = 1200, height = 720, res = 120, bg = style$background)
    generate_chart(chart_data, colors)
    dev.off()
    message("PNG chart saved to ", png_file)
  }
}
