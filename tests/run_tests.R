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

run_test("bundled custom font is loaded", {
  font_family <- suppressWarnings(setup_visualization_font())
  stopifnot(identical(font_family, "FiraCode"))
})

run_test("missing custom font falls back to sans", {
  font_family <- suppressWarnings(setup_visualization_font(tempfile()))
  stopifnot(identical(font_family, "sans"))
})

run_test("pie chart and treemap files are generated", {
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
})

message("All tests passed.")
