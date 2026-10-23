#!/usr/bin/env Rscript

source("R/helpers.R")
source("R/plot.R")
source("R/pie.R")
source("R/treemap.R")

run_test <- function(name, code) {
  tryCatch(
    {
      force(code)
      message("PASS: ", name)
    },
    error = function(error) {
      stop("FAIL: ", name, "\n", error$message, call. = FALSE)
    }
  )
}

run_test("all R files parse", {
  files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
  invisible(lapply(files, parse))
})

fixture <- data.frame(
  language = c("R", "R", "Go", NA),
  pushed_at = as.character(Sys.Date() - c(0, 365, 0, 0)),
  updated_at = as.character(Sys.Date() - c(0, 365, 0, 0)),
  stargazers_count = c(10, 0, 1, 100),
  forks_count = c(2, 0, 0, 100),
  fork = FALSE,
  archived = FALSE,
  disabled = FALSE,
  size = 1024
)

run_test("repository statistics are aggregated and ranked", {
  invisible(capture.output(repos <- suppressMessages(plot_repos(fixture))))

  stopifnot(
    identical(as.character(repos$language), c("R", "Go")),
    identical(repos$repo_count, c(2L, 1L)),
    identical(repos$total_stars, c(10, 1)),
    identical(repos$total_forks, c(2, 0)),
    isTRUE(all.equal(sum(repos$percentage), 100)),
    identical(repos$label, paste0(repos$language, " (", repos$percentage, "%)"))
  )
})

statistics <- function(data, reference_date = Sys.Date()) {
  invisible(capture.output(result <- suppressMessages(plot_repos(data, reference_date))))
  result
}

with_treemap_settings <- function(value, code, min_font = NULL, max_font = NULL) {
  settings <- list(
    TREEMAP_MIN_PERCENTAGE = value,
    TREEMAP_MIN_FONT_SIZE = min_font,
    TREEMAP_MAX_FONT_SIZE = max_font
  )
  previous <- Sys.getenv(names(settings), unset = NA_character_)
  on.exit({
    Sys.unsetenv(names(settings))
    if (any(!is.na(previous))) {
      do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
    }
  }, add = TRUE)
  Sys.unsetenv(names(settings))
  configured <- settings[!vapply(settings, is.null, logical(1))]
  if (length(configured) > 0) {
    do.call(Sys.setenv, configured)
  }
  force(code)
}

run_test("every score contribution has a one-year half-life", {
  reference_date <- as.Date("2026-10-23")
  data <- data.frame(
    language = c("Current", "OneYear", "TwoYears"),
    pushed_at = as.character(reference_date - c(0, 365, 730)),
    updated_at = as.character(reference_date),
    stargazers_count = 10,
    forks_count = 2,
    fork = FALSE,
    archived = FALSE,
    disabled = FALSE,
    size = 1024
  )
  result <- statistics(data, reference_date)
  stopifnot(
    isTRUE(all.equal(result$avg_date_weight, c(1, 0.5, 0.25))),
    isTRUE(all.equal(result$final_score / result$final_score[1], c(1, 0.5, 0.25))),
    isTRUE(all.equal(result$base_score / result$base_score[1], c(1, 0.5, 0.25))),
    isTRUE(all.equal(result$star_score / result$star_score[1], c(1, 0.5, 0.25))),
    isTRUE(all.equal(result$fork_score / result$fork_score[1], c(1, 0.5, 0.25)))
  )
  older <- statistics(data, reference_date + 365)
  stopifnot(
    isTRUE(all.equal(older$final_score, result$final_score / 2)),
    identical(older$percentage, result$percentage)
  )
})

run_test("old repository counts cannot maintain a permanent score floor", {
  data <- fixture[rep(1, 101), ]
  data$language <- c("Current", rep("Inactive", 100))
  data$pushed_at <- as.character(Sys.Date() - c(0, rep(3650, 100)))
  result <- statistics(data)
  stopifnot(
    identical(result$language, c("Current", "Inactive")),
    result$percentage[2] < 10,
    isTRUE(all.equal(result$effective_repo_count[2], 100 / 1024)),
    isTRUE(all.equal(result$base_score[2], 0.7 * 100 / 1024))
  )
})

run_test("repository count is weighted by activity without a duplicate bonus", {
  result <- statistics(fixture)
  row <- result[result$language == "R", ]
  stopifnot(
    isTRUE(all.equal(row$avg_date_weight, 0.75)),
    isTRUE(all.equal(row$effective_repo_count, 1.5)),
    isTRUE(all.equal(row$base_score, 0.7 * 1.5)),
    isTRUE(all.equal(row$final_score, row$base_score + row$star_score + row$fork_score)),
    identical(row$final_score, row$total_score),
    !"repo_count_score" %in% names(row)
  )
})

run_test("popularity contributions are normalized and capped at 20 and 10 percent", {
  data <- fixture[rep(1, 5), ]
  data$language <- c("Base", "Stars", "Forks", "Capped", "Extreme")
  data$stargazers_count <- c(0, 200, 0, 200, 1e12)
  data$forks_count <- c(0, 0, 50, 50, 1e12)
  result <- statistics(data)
  scores <- stats::setNames(result$final_score, result$language)
  stopifnot(
    isTRUE(all.equal(unname(scores[data$language]), c(0.7, 0.9, 0.8, 1, 1))),
    all(result$star_score <= 0.2),
    all(result$fork_score <= 0.1),
    all(result$final_score >= 0 & result$final_score <= 1)
  )
})

run_test("popularity has diminishing returns and cannot dominate repeated use", {
  data <- fixture[rep(1, 6), ]
  data$language <- paste0("Language", seq_len(6))
  data$stargazers_count <- c(0, 1, 2, 10, 200, 1000)
  data$forks_count <- 0
  result <- statistics(data)
  scores <- stats::setNames(result$final_score, result$language)[data$language]
  stopifnot(
    all(diff(scores) >= 0),
    scores[2] - scores[1] > scores[3] - scores[2],
    scores[5] == scores[6]
  )
  data <- fixture[rep(1, 3), ]
  data$language <- c("RegularUse", "RegularUse", "Popular")
  data$stargazers_count <- c(0, 0, 1e12)
  data$forks_count <- c(0, 0, 1e12)
  result <- statistics(data)
  stopifnot(
    identical(result$language, c("RegularUse", "Popular")),
    isTRUE(all.equal(result$final_score, c(1.4, 1)))
  )
})

run_test("archiving halves all score components without changing raw counts", {
  data <- fixture[rep(1, 2), ]
  data$language <- c("Active", "Archived")
  data$archived <- c(FALSE, TRUE)
  result <- statistics(data)
  stopifnot(
    identical(result$repo_count, c(1L, 1L)),
    identical(result$archived_repo_count, c(0L, 1L)),
    isTRUE(all.equal(result$effective_repo_count, c(1, 0.5)))
  )
  for (field in c("base_score", "star_score", "fork_score", "final_score")) {
    stopifnot(isTRUE(all.equal(result[[field]][2], result[[field]][1] / 2)))
  }
})

run_test("forked disabled and empty repositories are completely excluded", {
  extra <- fixture[rep(1, 3), ]
  extra$language <- c("Forked", "Disabled", "Empty")
  extra$fork <- c(TRUE, FALSE, FALSE)
  extra$disabled <- c(FALSE, TRUE, FALSE)
  extra$size <- c(1024, 1024, 0)
  extra$stargazers_count <- 1e12
  extra$forks_count <- 1e12
  stopifnot(isTRUE(all.equal(
    statistics(rbind(fixture, extra)),
    statistics(fixture)
  )))
})

run_test("eligibility metadata and numeric inputs are validated explicitly", {
  for (field in c("fork", "archived", "disabled", "size")) {
    data <- fixture
    data[[field]] <- NULL
    error <- tryCatch(statistics(data), error = identity)
    stopifnot(inherits(error, "error"), grepl("Missing repository fields", conditionMessage(error)))
  }
  for (field in c("stargazers_count", "forks_count", "size")) {
    for (invalid in c(-1, NA_real_, Inf)) {
      data <- fixture
      data[[field]][1] <- invalid
      error <- tryCatch(statistics(data), error = identity)
      stopifnot(inherits(error, "error"), grepl(field, conditionMessage(error), fixed = TRUE))
    }
  }
  data <- fixture
  data$fork[1] <- NA
  error <- tryCatch(statistics(data), error = identity)
  stopifnot(inherits(error, "error"), grepl("non-missing logical", conditionMessage(error)))
})

run_test("an entirely excluded repository set fails clearly", {
  data <- fixture
  data$fork <- TRUE
  error <- tryCatch(statistics(data), error = identity)
  stopifnot(
    inherits(error, "error"),
    grepl("No positive language scores", conditionMessage(error))
  )
})

run_test("future pushes cannot increase scores above current activity", {
  data <- fixture[1:2, ]
  data$pushed_at <- as.character(Sys.Date() + c(0, 365))
  result <- statistics(data)
  stopifnot(result$avg_date_weight == 1, result$avg_days_since_update == 0)
})

run_test("legacy dates fall back explicitly and unknown dates have zero weight", {
  data <- fixture[1:3, ]
  data$pushed_at <- c(NA, "not-a-date", as.character(Sys.Date()))
  data$updated_at <- c(as.character(Sys.Date() - 365), NA, as.character(Sys.Date()))
  warnings <- character()
  result <- withCallingHandlers(
    statistics(data),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(
    length(warnings) == 2,
    any(grepl("falling back", warnings)),
    any(grepl("zero weight", warnings)),
    result$avg_date_weight[result$language == "R"] == 0.25
  )
  data$pushed_at <- NULL
  data$updated_at <- as.character(Sys.Date())
  warnings <- character()
  result <- withCallingHandlers(
    statistics(data),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(length(warnings) == 1, all(result$avg_date_weight == 1))
})

run_test("unusable activity data fails instead of generating invalid percentages", {
  data <- fixture
  data$pushed_at <- NA_character_
  data$updated_at <- NA_character_
  error <- tryCatch(
    suppressWarnings(statistics(data)),
    error = identity
  )
  stopifnot(
    inherits(error, "error"),
    grepl("No positive language scores", conditionMessage(error))
  )
})

run_test("missing local environment file is optional", {
  stopifnot(identical(load_local_env(tempfile()), FALSE))
})

run_test("local environment loads quoted values without overriding exports", local({
  path <- tempfile("langstat-env-")
  keys <- c("LANGSTAT_TEST_EXPORTED", "LANGSTAT_TEST_IMPORTED")
  previous <- Sys.getenv(keys, unset = NA_character_)
  on.exit({
    unlink(path)
    Sys.unsetenv(keys)
    if (any(!is.na(previous))) {
      do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
    }
  }, add = TRUE)
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
  on.exit({
    unlink(path)
    Sys.unsetenv(key)
    if (!is.na(previous)) {
      Sys.setenv(LANGSTAT_TEST_IMPORTED = previous)
    }
  }, add = TRUE)
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
    final_score = c(98, 1, 0.96, 0.04),
    total_score = c(98, 1, 0.96, 0.04),
    repo_count = c(10L, 2L, 3L, 4L)
  )
  chart <- prepare_chart_data(data)
  stopifnot(
    identical(chart$language, c("Primary", "ExactlyOne", "Other")),
    isTRUE(all.equal(chart$percentage, c(98, 1, 1))),
    chart$repo_count[chart$language == "Other"] == 7,
    sum(chart$final_score) == sum(data$final_score),
    sum(chart$repo_count) == sum(data$repo_count),
    nrow(data) == 4
  )
})

run_test("grouped chart language limit preserves remaining scores in Other", {
  data <- data.frame(
    language = paste0("Language", seq_len(13)),
    final_score = seq_len(13),
    repo_count = 1L
  )
  chart <- prepare_chart_data(data, max_languages = 12)
  stopifnot(
    nrow(chart) == 13,
    chart$language[13] == "Other",
    chart$final_score[13] == 1,
    sum(chart$final_score) == sum(data$final_score),
    sum(chart$repo_count) == sum(data$repo_count)
  )
})

run_test("treemap cutoff uses unrounded shares and does not create Other", {
  data <- data.frame(
    language = c("Primary", "ExactlyTwo", "RoundedUp", "Tiny"),
    final_score = c(96, 2, 1.96, 0.04),
    total_score = c(96, 2, 1.96, 0.04),
    repo_count = 1L
  )
  chart <- prepare_chart_data(data, min_percentage = 2, group_other = FALSE)
  stopifnot(
    identical(chart$language, c("Primary", "ExactlyTwo")),
    isTRUE(all.equal(chart$exact_percentage, c(96, 2))),
    isTRUE(all.equal(chart$percentage, c(96, 2))),
    !"Other" %in% chart$language
  )
  all_positive <- prepare_chart_data(data, min_percentage = 0, group_other = FALSE)
  stopifnot(nrow(all_positive) == 4)

  data$final_score <- c(1, 0, 0, 0)
  chart <- prepare_chart_data(data, min_percentage = 100, group_other = FALSE)
  stopifnot(nrow(chart) == 1, chart$percentage == 100)
})

run_test("filtered treemap still limits individual languages without aggregation", {
  data <- data.frame(
    language = paste0("Language", seq_len(17)),
    final_score = 1,
    repo_count = 1L
  )
  chart <- prepare_chart_data(
    data, max_languages = 12, min_percentage = 2, group_other = FALSE
  )
  stopifnot(
    nrow(chart) == 12,
    !"Other" %in% chart$language,
    all(abs(chart$exact_percentage - 100 / 17) < 1e-12)
  )
})

run_test("treemap rejects invalid thresholds and reports an entirely hidden chart", {
  repos <- statistics(fixture)
  output_dir <- tempfile("langstat-invalid-threshold-")
  for (value in c("", "invalid", "-1", "101", "Inf", "NaN", "2%")) {
    error <- with_treemap_settings(value, tryCatch(
      generate_treemap("test-user", repos, output_dir, fixture),
      error = identity
    ))
    stopifnot(
      inherits(error, "error"),
      grepl("TREEMAP_MIN_PERCENTAGE", conditionMessage(error)),
      !dir.exists(output_dir)
    )
  }
  error <- with_treemap_settings("100", tryCatch(
    generate_treemap("test-user", repos, output_dir, fixture),
    error = identity
  ))
  stopifnot(inherits(error, "error"), grepl("No languages meet", conditionMessage(error)))
})

run_test("treemap rejects invalid font bounds before creating exports", {
  repos <- statistics(fixture)
  output_dir <- tempfile("langstat-invalid-font-")
  for (value in c("", "invalid", "-1", "0", "Inf", "-Inf", "NaN", "NA", "16pt")) {
    for (bound in c("min", "max")) {
      error <- with_treemap_settings(NULL, tryCatch(
        generate_treemap("test-user", repos, output_dir, fixture),
        error = identity
      ), min_font = if (bound == "min") value else NULL,
      max_font = if (bound == "max") value else NULL)
      stopifnot(
        inherits(error, "error"),
        grepl("positive finite numbers", conditionMessage(error)),
        !dir.exists(output_dir)
      )
    }
  }
  error <- with_treemap_settings(NULL, tryCatch(
    generate_treemap("test-user", repos, output_dir, fixture),
    error = identity
  ), min_font = "30", max_font = "16")
  stopifnot(inherits(error, "error"), grepl("must not exceed", conditionMessage(error)))
})

run_test("treemap renders bounded linear fonts with default and overridden settings", local({
  repos <- data.frame(
    language = c("Large", "Medium", "ExactlyTwo", "RoundedUp", "Small"),
    final_score = c(75, 20, 2, 1.99, 1.01),
    total_score = c(75, 20, 2, 1.99, 1.01),
    repo_count = 1L
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
      generate_treemap("test-user", repos, output_dir, repos)
    ), min_font = config$min, max_font = config$max)
    expected_languages <- if (is.null(config$threshold)) {
      c("Large", "Medium", "ExactlyTwo")
    } else {
      c("Large", "Medium")
    }
    stopifnot(setequal(plot$data$language, expected_languages))
    expected_sizes <- unname(stats::setNames(config$sizes, expected_languages)[plot$data$language])
    stopifnot(isTRUE(all.equal(plot$data$font_size, expected_sizes)))

    grid::grid.newpage()
    table <- ggplot2::ggplotGrob(plot)
    panel <- table$grobs[[which(table$layout$name == "panel")]]
    text <- Filter(function(grob) inherits(grob, "text"), panel$children)
    stopifnot(
      length(text) == 1,
      isTRUE(all.equal(unname(text[[1]]$gp$fontsize), expected_sizes)),
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
    repos <- data.frame(
      language = paste0("Language", seq_along(case$scores)),
      final_score = case$scores,
      repo_count = 1L
    )
    plot <- with_treemap_settings(NULL, suppressMessages(
      generate_treemap("test-user", repos, output_dir, repos)
    ))
    expected <- stats::setNames(case$sizes, repos$language)[plot$data$language]
    stopifnot(
      all(is.finite(plot$data$font_size)),
      all(plot$data$font_size >= 16 & plot$data$font_size <= 32),
      isTRUE(all.equal(plot$data$font_size, unname(expected)))
    )
  }
  plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", repos[1, ], output_dir, repos[1, ])
  ), min_font = "18", max_font = "36")
  stopifnot(plot$data$font_size == 27)
}))

run_test("all-minor and zero-score languages produce valid chart data", {
  data <- data.frame(
    language = paste0("Language", seq_len(101)),
    final_score = 1,
    repo_count = 1L
  )
  chart <- prepare_chart_data(data)
  stopifnot(nrow(chart) == 1, chart$language == "Other", chart$percentage == 100)
  data$final_score <- c(1, rep(0, 100))
  chart <- prepare_chart_data(data)
  stopifnot(nrow(chart) == 1, chart$language == "Language1", chart$percentage == 100)
  data$final_score <- 0
  error <- tryCatch(prepare_chart_data(data), error = identity)
  stopifnot(inherits(error, "error"), grepl("positive finite total", conditionMessage(error)))
})

run_test("pie chart and treemap files use the shared palette and outlined font", local({
  chart_fixture <- rbind(
    fixture,
    transform(fixture[1, ], language = "Legacy", pushed_at = as.character(Sys.Date() - 7 * 365))
  )
  repos <- statistics(chart_fixture)
  output_dir <- tempfile("langstat-test-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  suppressMessages(generate_pie_chart("test-user", repos, output_dir, chart_fixture))
  treemap_plot <- with_treemap_settings(NULL, suppressMessages(
    generate_treemap("test-user", repos, output_dir, chart_fixture)
  ))

  today <- format(Sys.Date(), "%Y%m%d")
  expected_files <- c(
    paste0("piechart_", today, ".svg"),
    paste0("piechart_", today, ".png"),
    "latest.svg",
    "latest.png",
    paste0("treemap_", today, ".svg"),
    paste0("treemap_", today, ".png"),
    "treemap_latest.svg",
    "treemap_latest.png"
  )
  output_files <- file.path(output_dir, expected_files)

  stopifnot(all(file.exists(output_files)), all(file.size(output_files) > 0))
  style <- visualization_style(c(repos$language, "Other"))
  chart <- prepare_chart_data(repos)
  stopifnot("Other" %in% chart$language, !"Legacy" %in% chart$language)
  stopifnot(!any(treemap_plot$data$language %in% c("Other", "Legacy")))
  for (svg_name in c("latest.svg", "treemap_latest.svg")) {
    svg <- paste(readLines(file.path(output_dir, svg_name), warn = FALSE), collapse = "\n")
    languages <- if (svg_name == "latest.svg") chart$language else treemap_plot$data$language
    stopifnot(
      grepl("<path", svg, fixed = TRUE),
      !grepl("<text", svg, fixed = TRUE),
      grepl(style$background, svg, fixed = TRUE),
      all(vapply(unname(style$colors[languages]), grepl, logical(1), x = svg, fixed = TRUE))
    )
  }
}))

message("All tests passed.")
