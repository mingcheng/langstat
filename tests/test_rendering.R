fixture <- data.frame(
  language = c("R", "Go"), final_score = c(2, 1), repo_count = c(2L, 1L)
)
fixture_summary <- list(
  window_start = as.character(as.Date(Sys.time(), tz = "UTC") - 364),
  window_end = as.character(as.Date(Sys.time(), tz = "UTC")),
  public_repositories = 3L, scored_repositories = 3L,
  scored_owned_repositories = 2L, scored_contributed_repositories = 1L
)

with_treemap_settings <- function(value, code, min_font = NULL, max_font = NULL) {
  settings <- list(
    TREEMAP_MIN_PERCENTAGE = value,
    TREEMAP_MIN_FONT_SIZE = min_font,
    TREEMAP_MAX_FONT_SIZE = max_font
  )
  previous <- Sys.getenv(names(settings), unset = NA_character_)
  on.exit(
    {
      Sys.unsetenv(names(settings))
      if (any(!is.na(previous))) {
        do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
      }
    },
    add = TRUE
  )
  Sys.unsetenv(names(settings))
  configured <- settings[!vapply(settings, is.null, logical(1))]
  if (length(configured) > 0) {
    do.call(Sys.setenv, configured)
  }
  force(code)
}

assert_treemap_text_bounds <- function(data) {
  data <- data[data$label_layout != "legend", , drop = FALSE]
  for (prefix in c("name", "percentage")) {
    x <- data[[paste0(prefix, "_x")]]
    y <- data[[paste0(prefix, "_y")]]
    width <- data[[paste0(prefix, "_width")]]
    height <- data[[paste0(prefix, "_height")]]
    hjust <- data[[paste0(prefix, "_hjust")]]
    stopifnot(
      all(x - width * hjust >= data$xmin - 1e-10),
      all(x + width * (1 - hjust) <= data$xmax + 1e-10),
      all(y - height / 2 >= data$ymin - 1e-10),
      all(y + height / 2 <= data$ymax + 1e-10)
    )
  }
  inline <- data$label_layout == "inline"
  stopifnot(
    all(data$name_x[inline] + data$name_width[inline] <
      data$percentage_x[inline] - data$percentage_width[inline]),
    all(data$name_y[!inline] - data$name_height[!inline] / 2 >
      data$percentage_y[!inline] + data$percentage_height[!inline] / 2)
  )
}

run_test("missing local environment file is optional", {
  stopifnot(identical(load_local_env(tempfile()), FALSE))
})

run_test("local environment loads quoted values without overriding exports", local({
  path <- tempfile("langstat-env-")
  keys <- c("LANGSTAT_TEST_EXPORTED", "LANGSTAT_TEST_IMPORTED")
  previous <- Sys.getenv(keys, unset = NA_character_)
  on.exit(
    {
      unlink(path)
      Sys.unsetenv(keys)
      if (any(!is.na(previous))) {
        do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
      }
    },
    add = TRUE
  )
  Sys.unsetenv(keys)
  Sys.setenv(LANGSTAT_TEST_EXPORTED = "from-shell")
  writeLines(c(
    "# Local test values, not credentials",
    "LANGSTAT_TEST_EXPORTED=from-file",
    "LANGSTAT_TEST_IMPORTED=\"value with spaces\""
  ), path)
  stopifnot(
    load_local_env(path),
    Sys.getenv("LANGSTAT_TEST_EXPORTED") == "from-shell",
    Sys.getenv("LANGSTAT_TEST_IMPORTED") == "value with spaces"
  )
}))

run_test("malformed local environment rolls back and never echoes secret lines", local({
  path <- tempfile("langstat-env-invalid-")
  key <- "LANGSTAT_TEST_IMPORTED"
  previous <- Sys.getenv(key, unset = NA_character_)
  on.exit(
    {
      unlink(path)
      Sys.unsetenv(key)
      if (!is.na(previous)) {
        Sys.setenv(LANGSTAT_TEST_IMPORTED = previous)
      }
    },
    add = TRUE
  )
  Sys.unsetenv(key)
  writeLines(c("LANGSTAT_TEST_IMPORTED=partial-value", "invalid-DUMMY-SECRET-line"), path)
  error <- tryCatch(load_local_env(path), error = identity)
  stopifnot(
    inherits(error, "error"),
    grepl("Failed to load", conditionMessage(error)),
    !grepl("DUMMY-SECRET", conditionMessage(error)),
    is.na(Sys.getenv(key, unset = NA_character_))
  )
}))

run_test("bundled custom font is loaded", {
  font_family <- suppressWarnings(setup_visualization_font())
  stopifnot(
    identical(font_family, "DankMono"),
    "DankMono" %in% sysfonts::font_families(),
    showtext::showtext_opts()$dpi == 120
  )
})

run_test("missing custom font falls back to sans", {
  font_family <- suppressWarnings(setup_visualization_font(tempfile()))
  stopifnot(identical(font_family, "sans"))
})

run_test("shared palette is independent of ranking and covers larger language sets", {
  style <- visualization_style(c("R", "Go", "Python"))
  stopifnot(
    identical(style, visualization_style(c("Python", "R", "Go"))),
    length(unique(style$colors)) == 3
  )
  large <- visualization_style(paste0("Language", seq_len(20)))
  stopifnot(length(unique(large$colors)) == 20, !anyNA(large$colors))
})

run_test("Other uses a neutral color without changing individual language colors", {
  languages <- c("R", "Go", "Python")
  colors <- visualization_style(languages)$colors
  with_other <- visualization_style(c(languages, "Other"))$colors
  stopifnot(
    identical(colors, with_other[names(colors)]),
    unname(with_other["Other"]) == "#CBD5E1"
  )
})

run_test("minor chart languages use an exact cutoff and preserve all contributions", {
  data <- data.frame(
    language = c("Primary", "ExactlyOne", "RoundedUp", "Tiny"),
    final_score = c(98, 1, 0.96, 0.04)
  )
  chart <- prepare_chart_data(data)
  stopifnot(
    identical(chart$language, c("Primary", "ExactlyOne", "Other")),
    isTRUE(all.equal(chart$percentage, c(98, 1, 1))),
    sum(chart$final_score) == sum(data$final_score),
    nrow(data) == 4
  )
})

run_test("grouped chart language limit preserves remaining scores in Other", {
  data <- data.frame(language = paste0("Language", seq_len(13)), final_score = seq_len(13))
  chart <- prepare_chart_data(data, max_languages = 12)
  stopifnot(
    nrow(chart) == 13, chart$language[13] == "Other",
    chart$final_score[13] == 1, sum(chart$final_score) == sum(data$final_score)
  )
})

run_test("treemap cutoff uses unrounded shares and does not create Other", {
  data <- data.frame(
    language = c("Primary", "ExactlyTwo", "RoundedUp", "Tiny"),
    final_score = c(96, 2, 1.96, 0.04)
  )
  chart <- prepare_chart_data(data, min_percentage = 2, group_other = FALSE)
  stopifnot(
    identical(chart$language, c("Primary", "ExactlyTwo")),
    isTRUE(all.equal(chart$exact_percentage, c(96, 2))),
    isTRUE(all.equal(chart$percentage, c(96, 2))),
    !"Other" %in% chart$language
  )
  stopifnot(nrow(prepare_chart_data(data, min_percentage = 0, group_other = FALSE)) == 4)
  data$final_score <- c(1, 0, 0, 0)
  chart <- prepare_chart_data(data, min_percentage = 100, group_other = FALSE)
  stopifnot(nrow(chart) == 1, chart$percentage == 100)
})

run_test("filtered treemap still limits individual languages without aggregation", {
  data <- data.frame(language = paste0("Language", seq_len(17)), final_score = 1)
  chart <- prepare_chart_data(data, max_languages = 12, min_percentage = 2, group_other = FALSE)
  stopifnot(
    nrow(chart) == 12, !"Other" %in% chart$language,
    all(abs(chart$exact_percentage - 100 / 17) < 1e-12)
  )
})

run_test("treemap rejects invalid thresholds and reports an entirely hidden chart", {
  output_dir <- tempfile("langstat-invalid-threshold-")
  for (value in c("", "invalid", "-1", "101", "Inf", "NaN", "2%")) {
    with_treemap_settings(value, expect_error(
      generate_treemap("test-user", fixture, output_dir, fixture_summary),
      "TREEMAP_MIN_PERCENTAGE"
    ))
    stopifnot(!dir.exists(output_dir))
  }
  with_treemap_settings("100", expect_error(
    generate_treemap("test-user", fixture, output_dir, fixture_summary), "No languages meet"
  ))
})

run_test("treemap rejects invalid font bounds before creating exports", {
  output_dir <- tempfile("langstat-invalid-font-")
  for (value in c("", "invalid", "-1", "0", "Inf", "-Inf", "NaN", "NA", "16pt")) {
    for (bound in c("min", "max")) {
      with_treemap_settings(NULL, expect_error(
        generate_treemap("test-user", fixture, output_dir, fixture_summary), "positive finite numbers"
      ), min_font = if (bound == "min") value else NULL, max_font = if (bound == "max") value else NULL)
      stopifnot(!dir.exists(output_dir))
    }
  }
  with_treemap_settings(NULL, expect_error(
    generate_treemap("test-user", fixture, output_dir, fixture_summary), "must not exceed"
  ), min_font = "30", max_font = "16")
})

run_test("treemap renders bounded linear fonts with default and overridden settings", local({
  repos <- data.frame(
    language = c("Large", "Medium", "ExactlyTwo", "RoundedUp", "Small"),
    final_score = c(75, 20, 2, 1.99, 1.01)
  )
  output_dir <- tempfile("langstat-treemap-sizing-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  configurations <- list(
    list(threshold = NULL, min = NULL, max = NULL, sizes = c(32, 16 + 16 * 18 / 73, 16)),
    list(threshold = "2.5", min = NULL, max = NULL, sizes = c(32, 16)),
    list(threshold = NULL, min = "12.5", max = "28.5", sizes = c(28.5, 12.5 + 16 * 18 / 73, 12.5)),
    list(threshold = NULL, min = "18", max = NULL, sizes = c(32, 18 + 14 * 18 / 73, 18)),
    list(threshold = NULL, min = NULL, max = "28", sizes = c(28, 16 + 12 * 18 / 73, 16)),
    list(threshold = NULL, min = "20", max = "20", sizes = c(20, 20, 20))
  )
  for (config in configurations) {
    plot <- with_treemap_settings(config$threshold, suppressMessages(
      generate_treemap("test-user", repos, output_dir, fixture_summary)
    ), min_font = config$min, max_font = config$max)
    expected_languages <- if (is.null(config$threshold)) {
      c("Large", "Medium", "ExactlyTwo")
    } else {
      c("Large", "Medium")
    }
    stopifnot(setequal(plot$data$language, expected_languages))
    expected_sizes <- unname(stats::setNames(config$sizes, expected_languages)[plot$data$language])
    lower_bound <- if (is.null(config$min)) 16 else as.numeric(config$min)
    expected_percent_sizes <- lower_bound + (expected_sizes - lower_bound) / 2
    stopifnot(
      isTRUE(all.equal(plot$data$font_size, expected_sizes)),
      isTRUE(all.equal(plot$data$percentage_font_size, expected_percent_sizes))
    )
    assert_treemap_text_bounds(plot$data)
    grid::grid.newpage()
    table <- ggplot2::ggplotGrob(plot)
    panel <- table$grobs[[which(table$layout$name == "panel")]]
    text <- Filter(function(grob) inherits(grob, "text"), panel$children)
    stopifnot(
      length(text) == 2,
      isTRUE(all.equal(unname(text[[1]]$gp$fontsize), expected_sizes)),
      isTRUE(all.equal(unname(text[[2]]$gp$fontsize), expected_percent_sizes)),
      all(text[[1]]$gp$font == 2),
      all(text[[2]]$gp$font == 1),
      isTRUE(all.equal(
        with(plot$data, (xmax - xmin) * (ymax - ymin)),
        plot$data$final_score / sum(plot$data$final_score)
      ))
    )
  }
}))

run_test("treemap font range handles equal shares single languages and extreme ratios", local({
  output_dir <- tempfile("langstat-font-edge-cases-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  cases <- list(
    list(scores = c(1, 1, 1), sizes = c(24, 24, 24)),
    list(scores = 100, sizes = 24),
    list(scores = c(98, 2), sizes = c(32, 16)),
    list(scores = c(33.334, 33.333, 33.333), sizes = c(32, 16, 16))
  )
  for (case in cases) {
    repos <- data.frame(language = paste0("Language", seq_along(case$scores)), final_score = case$scores)
    plot <- with_treemap_settings(NULL, suppressMessages(
      generate_treemap("test-user", repos, output_dir, fixture_summary)
    ))
    expected <- stats::setNames(case$sizes, repos$language)[plot$data$language]
    stopifnot(
      all(is.finite(plot$data$font_size)),
      all(plot$data$font_size >= 16 & plot$data$font_size <= 32),
      isTRUE(all.equal(plot$data$font_size, unname(expected)))
    )
    assert_treemap_text_bounds(plot$data)
  }
  plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", repos[1, ], output_dir, fixture_summary)
  ), min_font = "18", max_font = "36")
  stopifnot(plot$data$font_size == 27)
}))

run_test("treemap wraps long names without truncation or changing bounded font sizes", local({
  output_dir <- tempfile("langstat-wrapped-names-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  data <- data.frame(
    language = c(
      "A deliberately long language name", "LongestSingleWordLanguageIdentifier",
      "Objective-C", "TypeScript"
    ),
    final_score = 1
  )
  plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", data, output_dir, fixture_summary)
  ))
  stopifnot(
    any(grepl("\n", plot$data$name_label, fixed = TRUE)),
    identical(
      gsub("[[:space:]]", "", plot$data$name_label),
      gsub("[[:space:]]", "", plot$data$language)
    ),
    all(plot$data$font_size == 24),
    all(plot$data$percentage_font_size == 20),
    plot$theme$plot.title$hjust == 0,
    plot$theme$plot.caption$hjust == 0
  )
  assert_treemap_text_bounds(plot$data)
}))

run_test("extreme-ratio treemaps move labels to a full-size colored legend", local({
  output_dir <- tempfile("langstat-overflow-legend-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  data <- data.frame(language = c("Large", "Tiny"), final_score = c(98, 2))
  plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", data, output_dir, fixture_summary)
  ))
  stopifnot(
    plot$data$label_layout[plot$data$language == "Tiny"] == "legend",
    plot$data$font_size[plot$data$language == "Tiny"] == 16,
    isTRUE(all.equal(
      with(plot$data, (xmax - xmin) * (ymax - ymin)),
      plot$data$final_score / sum(plot$data$final_score)
    ))
  )
  legend <- plot$guides$guides$overflow$params$grob
  stopifnot(
    legend$children[["treemap-legend-names"]]$label == "Tiny",
    legend$children[["treemap-legend-names"]]$gp$fontsize == 16,
    legend$children[["treemap-legend-percentages"]]$label == "2%",
    legend$children[["treemap-legend-percentages"]]$gp$fontsize == 16
  )
  assert_treemap_text_bounds(plot$data)
}))

run_test("multiple small-block legend entries preserve names colors and mapped font sizes", local({
  output_dir <- tempfile("langstat-multiple-legend-labels-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  data <- data.frame(
    language = c("Large", "SmallOne", "SmallTwo"),
    final_score = c(96, 2, 2)
  )
  plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", data, output_dir, fixture_summary)
  ))
  outside <- plot$data[plot$data$label_layout == "legend", ]
  legend <- plot$guides$guides$overflow$params$grob
  style <- visualization_style(data$language)
  stopifnot(
    setequal(outside$language, c("SmallOne", "SmallTwo")),
    identical(legend$children[["treemap-legend-names"]]$label, outside$language),
    identical(legend$children[["treemap-legend-names"]]$gp$fontsize, outside$font_size),
    identical(
      legend$children[["treemap-legend-percentages"]]$gp$fontsize,
      outside$percentage_font_size
    ),
    identical(legend$children[[1]]$gp$fill, unname(style$colors[outside$language]))
  )
  assert_treemap_text_bounds(plot$data)
}))

run_test("fonts too large for the chart and legend fail before export and restore the device", {
  output_dir <- tempfile("langstat-unfittable-text-")
  previous_device <- grDevices::dev.cur()
  with_treemap_settings(NULL, expect_error(
    suppressMessages(generate_treemap("test-user", fixture, output_dir, fixture_summary)),
    "cannot fit"
  ), min_font = "400", max_font = "400")
  stopifnot(
    !dir.exists(output_dir),
    identical(grDevices::dev.cur(), previous_device)
  )
})

run_test("all-minor and zero-score languages produce valid chart data", {
  data <- data.frame(language = paste0("Language", seq_len(101)), final_score = 1)
  chart <- prepare_chart_data(data)
  stopifnot(nrow(chart) == 1, chart$language == "Other", chart$percentage == 100)
  data$final_score <- c(1, rep(0, 100))
  chart <- prepare_chart_data(data)
  stopifnot(nrow(chart) == 1, chart$language == "Language1", chart$percentage == 100)
  data$final_score <- 0
  expect_error(prepare_chart_data(data), "positive finite total")
})

run_test("chart captions use distinct profile counts rather than per-language counts", {
  profile <- build_activity_profile(profile_fixture())
  subtitle <- profile_chart_subtitle("tester", profile$summary)
  stopifnot(
    grepl("Scored public repos: 2/3", subtitle, fixed = TRUE),
    grepl("owned 1, contributed 1", subtitle, fixed = TRUE),
    grepl("2025-10-26 to 2026-10-25 UTC", subtitle, fixed = TRUE),
    sum(profile$languages$repo_count) == 3
  )
  expect_error(profile_chart_subtitle("tester", data.frame()), "summary")
})

run_test("pie chart and treemap exports preserve the palette fonts and file names", local({
  repos <- rbind(fixture, transform(fixture[1, ], language = "Legacy", final_score = 0.001))
  output_dir <- tempfile("langstat-test-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  suppressMessages(generate_pie_chart("test-user", repos, output_dir, fixture_summary))
  plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", repos, output_dir, fixture_summary)
  ))
  today <- format(as.Date(fixture_summary$window_end), "%Y%m%d")
  expected_files <- c(
    paste0("piechart_", today, ".svg"), paste0("piechart_", today, ".png"),
    "latest.svg", "latest.png",
    paste0("treemap_", today, ".svg"), paste0("treemap_", today, ".png"),
    "treemap_latest.svg", "treemap_latest.png"
  )
  output_files <- file.path(output_dir, expected_files)
  stopifnot(all(file.exists(output_files)), all(file.size(output_files) > 0))
  style <- visualization_style(c(repos$language, "Other"))
  chart <- prepare_chart_data(repos)
  stopifnot("Other" %in% chart$language, !"Legacy" %in% chart$language)
  stopifnot(!any(plot$data$language %in% c("Other", "Legacy")))
  for (svg_name in c("latest.svg", "treemap_latest.svg")) {
    svg <- paste(readLines(file.path(output_dir, svg_name), warn = FALSE), collapse = "\n")
    languages <- if (svg_name == "latest.svg") chart$language else plot$data$language
    stopifnot(
      grepl("<path", svg, fixed = TRUE), !grepl("<text", svg, fixed = TRUE),
      grepl(style$background, svg, fixed = TRUE),
      all(vapply(unname(style$colors[languages]), grepl, logical(1), x = svg, fixed = TRUE))
    )
  }
}))
