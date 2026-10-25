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

required_packages <- c(
  "dplyr", "ggplot2", "httr", "jsonlite", "showtext", "svglite", "sysfonts",
  "treemapify"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    ". Install them with install.packages()."
  )
}

if (utils::packageVersion("ggplot2") < "3.5.0") {
  stop("Treemap typography requires ggplot2 >= 3.5.0; update ggplot2 first.")
}

source_module <- function(path) {
  tryCatch(
    suppressMessages(source(path)),
    error = function(error) stop("Failed to source ", path, ": ", error$message)
  )
}

invisible(lapply(
  c("R/helpers.R", "R/profile.R", "R/fetch.R", "R/pie.R", "R/treemap.R"),
  source_module
))

load_local_env()

# Get GitHub username from environment or use default
username <- validate_github_username(Sys.getenv("GITHUB_USERNAME", unset = "mingcheng"))

message("Analyzing public GitHub development activity for: ", username)

# Setup directories and file paths
reference_date <- as.Date(Sys.time(), tz = "UTC")
today <- format(reference_date, "%Y%m%d")
dir_name <- file.path("data", username)
if (!dir.exists(dir_name) && !dir.create(dir_name, recursive = TRUE)) {
  stop("Could not create the output directory: ", dir_name)
}

raw_file_name <- file.path(dir_name, paste0("activity_v2_", today, ".json"))
plotted_file_name <- file.path(dir_name, paste0("plotted_", today, ".csv"))
repository_file_name <- file.path(dir_name, paste0("repositories_", today, ".csv"))
summary_file_name <- file.path(dir_name, paste0("summary_", today, ".json"))

snapshot <- read_activity_cache(raw_file_name, username, reference_date)
if (is.null(snapshot)) {
  # Check for HTTP proxy settings
  proxy <- Sys.getenv("HTTP_PROXY", unset = "")
  if (!nzchar(proxy)) {
    proxy <- NULL
    message("No proxy configured.")
  } else {
    message("Using configured HTTP proxy.")
  }

  snapshot <- fetch_activity_snapshot(username, reference_date, proxy)
  write_activity_cache(snapshot, raw_file_name)
  message("Public activity evidence saved to ", raw_file_name)
} else {
  message("Loaded validated activity snapshot from ", raw_file_name)
}

profile <- build_activity_profile(snapshot)
if (profile$summary$status != "ok") {
  stop(
    "No scorable public development activity between ",
    snapshot$metadata$window_start, " and ", snapshot$metadata$window_end,
    ". Private work, other branches and unlinked commit authors are not covered.",
    " Existing charts have not been replaced."
  )
}
message(
  "Scoring ", profile$summary$scored_repositories, " distinct public repositories (",
  profile$summary$scored_owned_repositories, " owned, ",
  profile$summary$scored_contributed_repositories, " contributed) across ",
  nrow(profile$languages), " languages."
)

# Export statistical results
write.csv(profile$languages, plotted_file_name, row.names = FALSE, na = "")
write.csv(profile$repositories, repository_file_name, row.names = FALSE, na = "")
jsonlite::write_json(profile$summary, summary_file_name, pretty = TRUE, auto_unbox = TRUE)
message("Language scores and repository evidence exported.")

generate_pie_chart(username, profile$languages, dir_name, profile$summary)

generate_treemap(username, profile$languages, dir_name, profile$summary)

message("Analysis complete!")
