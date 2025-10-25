#!/usr/bin/env Rscript

# !#
# Copyright (c) 2025 Hangzhou Guanwaii Technology Co., Ltd.
#
# This source code is licensed under the MIT License,
# which is located in the LICENSE file in the source tree's root directory.
#
# File: main.R
# Author: mingcheng <mingcheng@apache.org>
# File Created: 2025-10-25 15:04:02
#
# Modified By: mingcheng <mingcheng@apache.org>
# Last Modified: 2025-10-25 16:18:16
##

# Load required packages
if (!require("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
  library(jsonlite)
}


# Source helper functions
tryCatch(
  suppressMessages(source("R/fetch.R")),
  error = function(e) stop("Failed to source R/fetch.R: ", e$message)
)

tryCatch(
  suppressMessages(source("R/plot.R")),
  error = function(e) stop("Failed to source R/plot.R: ", e$message)
)

# Get GitHub username from environment or use default
username <- Sys.getenv("GITHUB_USERNAME", unset = "mingcheng")
if (username == "") {
  stop("Please set the GITHUB_USERNAME environment variable.")
}

message("Analyzing GitHub user: ", username)

# Setup directories and file paths
today <- format(Sys.Date(), "%Y%m%d")
dir_name <- file.path("data", username)
dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)

raw_file_name <- file.path(dir_name, paste0("raw_", today, ".json"))
plotted_file_name <- file.path(dir_name, paste0("plotted_", today, ".csv"))

# Fetch or load cached repository data
if (file.exists(raw_file_name)) {
  json_data <- fromJSON(raw_file_name)
  message("Loaded cached data from ", raw_file_name)
} else {
  json_data <- fetch_json(username)
  if (is.null(json_data)) {
    stop("Failed to fetch data from GitHub API")
  }
  message("Fetched ", length(json_data), " repositories")

  write_json(json_data, raw_file_name, pretty = TRUE, auto_unbox = TRUE)
  message("Raw data saved to ", raw_file_name)
}

# Process and analyze repository data
repos <- plot_repos(json_data)
if (is.null(repos)) {
  stop("Failed to process repository data")
}
message("Successfully processed repository data")

# Export statistical results
write.csv(repos, plotted_file_name, row.names = FALSE)
message("Statistical data saved to ", plotted_file_name)

# Generate visualizations

# Generate pie chart
tryCatch(
  suppressMessages(source("R/pie.R")),
  error = function(e) stop("Failed to source R/pie.R: ", e$message)
)

generate_pie_chart(username, repos, dir_name, json_data)

# Generate treemap
tryCatch(
  suppressMessages(source("R/treemap.R")),
  error = function(e) stop("Failed to source R/treemap.R: ", e$message)
)

generate_treemap(username, repos, dir_name, json_data)




message("Analysis complete!")
