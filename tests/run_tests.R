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
  forks_count = c(2, 0, 0, 100)
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

run_test("every score contribution has a one-year half-life", {
  reference_date <- as.Date("2026-10-23")
  data <- data.frame(
    language = c("Current", "OneYear", "TwoYears"),
    pushed_at = as.character(reference_date - c(0, 365, 730)),
    updated_at = as.character(reference_date),
    stargazers_count = 10,
    forks_count = 2
  )
  result <- statistics(data, reference_date)
  stopifnot(
    isTRUE(all.equal(result$avg_date_weight, c(1, 0.5, 0.25))),
    isTRUE(all.equal(result$final_score / result$final_score[1], c(1, 0.5, 0.25))),
    isTRUE(all.equal(result$repo_count_score / result$repo_count_score[1], c(1, 0.5, 0.25)))
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
    isTRUE(all.equal(result$repo_count_score[2], 3.5 * log1p(100) / 1024))
  )
})

run_test("mixed activity uses mean date weight for repository count bonus", {
  result <- statistics(fixture)
  row <- result[result$language == "R", ]
  stopifnot(
    isTRUE(all.equal(row$avg_date_weight, 0.75)),
    isTRUE(all.equal(row$repo_count_score, 3.5 * log1p(2) * 0.75))
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

run_test("pie chart and treemap files use the shared palette and outlined font", local({
  invisible(capture.output(repos <- suppressMessages(plot_repos(fixture))))
  output_dir <- tempfile("langstat-test-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  suppressMessages(generate_pie_chart("test-user", repos, output_dir, fixture))
  suppressMessages(generate_treemap("test-user", repos, output_dir, fixture))

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
  style <- visualization_style(repos$language)
  for (svg_name in c("latest.svg", "treemap_latest.svg")) {
    svg <- paste(readLines(file.path(output_dir, svg_name), warn = FALSE), collapse = "\n")
    stopifnot(
      grepl("<path", svg, fixed = TRUE),
      !grepl("<text", svg, fixed = TRUE),
      grepl(style$background, svg, fixed = TRUE),
      all(vapply(unname(style$colors), grepl, logical(1), x = svg, fixed = TRUE))
    )
  }
}))

message("All tests passed.")
