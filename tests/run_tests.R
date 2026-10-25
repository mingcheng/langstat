#!/usr/bin/env Rscript

source("R/helpers.R")
source("R/profile.R")
source("R/fetch.R")
source("R/pie.R")
source("R/treemap.R")

test_count <- 0L
run_test <- function(name, code) {
  tryCatch(
    {
      force(code)
      test_count <<- test_count + 1L
      message("PASS: ", name)
    },
    error = function(error) {
      stop("FAIL: ", name, "\n", error$message, call. = FALSE)
    }
  )
}

expect_error <- function(code, pattern = NULL) {
  error <- tryCatch(force(code), error = identity)
  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) {
    stopifnot(grepl(pattern, conditionMessage(error), ignore.case = TRUE))
  }
  invisible(error)
}

run_test("all R files parse", {
  files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
  invisible(lapply(files, parse))
})

source("tests/test_profile.R")
source("tests/test_fetch.R")
source("tests/test_rendering.R")

message(test_count, " tests passed.")
