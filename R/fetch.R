#!#
# Copyright (c) 2025 Hangzhou Guanwaii Technology Co., Ltd.
#
# This source code is licensed under the MIT License,
# which is located in the LICENSE file in the source tree's root directory.
#
# File: fetch.R
# Author: mingcheng <mingcheng@apache.org>
# File Created: 2025-10-25 14:58:15
#
# Modified By: mingcheng <mingcheng@apache.org>
# Last Modified: 2025-10-25 17:25:34
##

# Load required packages
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}
library(httr)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(jsonlite)

#' Fetch GitHub Repository Data
#'
#' Fetches repository information for a GitHub user via the GitHub API.
#' Supports authentication via GITHUB_TOKEN environment variable for higher
#' rate limits (5000 req/hr vs 60 req/hr).
#'
#' @param username Character string of GitHub username
#' @param proxy Optional proxy configuration for httr::use_proxy()
#' @return Data frame of repository information, or NULL on error
#' @export
fetch_json <- function(username, proxy = NULL) {
  tryCatch(
    {
      # Prepare GitHub API friendly headers
      headers <- c(
        "Accept" = "application/vnd.github+json",
        "User-Agent" = "langstat.r/0.1 (+https://github.com/mingcheng/langstat)",
        "X-GitHub-Api-Version" = "2022-11-28"
      )

      # Determine per_page parameter from environment variable (must be 10-100)
      per_page <- as.numeric(Sys.getenv("GITHUB_PER_PAGE", unset = "100"))
      if (is.na(per_page) || per_page < 10) {
        per_page <- 10
      } else if (per_page > 100) {
        per_page <- 100
      }

      # Build API URL with pagination parameters
      url <- paste0(
        "https://api.github.com/users/",
        username,
        "/repos?per_page=", per_page, "&page=1"
      )

      # Add authentication token if available
      token <- Sys.getenv("GITHUB_TOKEN", unset = "")
      if (nzchar(token)) {
        headers["Authorization"] <- paste("Bearer", token)
      }

      # Make HTTP request
      response <- GET(
        url,
        add_headers(.headers = headers),
        if (!is.null(proxy)) use_proxy(proxy)
      )

      # Check response status
      if (status_code(response) != 200) {
        stop("HTTP request failed with status code: ", status_code(response))
      }

      # Parse JSON response
      content_text <- content(response, "text", encoding = "UTF-8")
      return(fromJSON(content_text))
    },
    error = function(e) {
      message("Error fetching JSON data: ", e$message)
      return(NULL)
    }
  )
}
