github_client <- function(token = Sys.getenv("GITHUB_TOKEN", unset = Sys.getenv("GH_TOKEN")),
                          proxy = NULL, transport = httr::GET, sleep = Sys.sleep,
                          now = Sys.time) {
  headers <- c(
    Accept = "application/vnd.github+json",
    `User-Agent` = "langstat.r/0.2 (+https://github.com/mingcheng/langstat)",
    `X-GitHub-Api-Version` = "2022-11-28"
  )
  if (nzchar(token)) {
    headers["Authorization"] <- paste("Bearer", token)
  } else {
    warning("No GitHub token configured; public requests are limited to 60 per hour.")
  }
  function(path, query = list()) {
    url <- if (startsWith(path, "/")) paste0("https://api.github.com", path) else path
    for (attempt in 0:3) {
      if (!grepl("^https://api[.]github[.]com/", url)) {
        stop("Refusing to send GitHub credentials to a non-GitHub API URL.")
      }
      response <- tryCatch(
        transport(
          url,
          query = query, httr::add_headers(.headers = headers),
          httr::timeout(30), httr::config(followlocation = FALSE),
          if (!is.null(proxy)) httr::use_proxy(proxy)
        ),
        error = function(error) error
      )
      if (inherits(response, "error")) {
        if (attempt == 3L) {
          stop("GitHub transport failed after four attempts for ", path, ".")
        }
        warning("GitHub transport failed; retrying ", path, ".")
        sleep(2^attempt)
        next
      }
      status <- httr::status_code(response)
      response_headers <- httr::headers(response)
      if (status == 200L) {
        text <- httr::content(response, "text", encoding = "UTF-8")
        if (!jsonlite::validate(text)) {
          stop("GitHub returned invalid JSON for ", path, ".")
        }
        return(jsonlite::fromJSON(text, simplifyVector = FALSE))
      }
      if (status %in% c(301L, 302L, 307L, 308L)) {
        location <- response_headers[["location"]]
        if (is.null(location) || attempt == 3L) {
          stop("GitHub returned an invalid or repeated redirect for ", path, ".")
        }
        url <- location
        query <- list()
        next
      }
      limited <- status == 429L || (status == 403L &&
        (identical(response_headers[["x-ratelimit-remaining"]], "0") ||
          !is.null(response_headers[["retry-after"]])))
      if (status == 403L && !limited) {
        text <- httr::content(response, "text", encoding = "UTF-8")
        if (jsonlite::validate(text)) {
          message <- jsonlite::fromJSON(text, simplifyVector = FALSE)$message
          limited <- is.character(message) && length(message) == 1L &&
            grepl("secondary rate limit|abuse", message, ignore.case = TRUE)
        }
      }
      if (!(limited || status %in% c(500L, 502L, 503L, 504L)) || attempt == 3L) {
        stop(
          "GitHub HTTP ", status, " for ", path,
          ". Check token permissions and API rate limits; no partial snapshot was saved."
        )
      }
      delay <- 2^attempt
      if (limited) {
        retry_after <- suppressWarnings(as.numeric(response_headers[["retry-after"]]))
        reset <- suppressWarnings(as.numeric(response_headers[["x-ratelimit-reset"]]))
        if (length(retry_after) == 1L && is.finite(retry_after)) {
          delay <- max(delay, retry_after)
        } else if (length(reset) == 1L && is.finite(reset)) {
          delay <- max(delay, reset - as.numeric(now()) + 1)
        } else {
          delay <- 60
        }
        if (delay > 120) {
          stop(
            "GitHub rate limit exhausted for ", path, ". Retry after the quota resets",
            " or use a token with more remaining quota; no partial snapshot was saved."
          )
        }
      }
      warning("GitHub HTTP ", status, "; retrying ", path, " in ", ceiling(delay), " seconds.")
      sleep(delay)
    }
    stop("GitHub request did not complete for ", path, ".")
  }
}

github_page_size <- function(value = Sys.getenv("GITHUB_PER_PAGE", unset = "100")) {
  size <- suppressWarnings(as.numeric(value))
  if (length(size) != 1L || !is.finite(size) || size < 1 || size > 100 ||
    size != floor(size)) {
    stop("GITHUB_PER_PAGE must be an integer between 1 and 100.")
  }
  as.integer(size)
}

github_list <- function(request, path, query = list(), per_page = 100L) {
  per_page <- github_page_size(per_page)
  items <- list()
  page <- 1L
  previous <- NULL
  repeat {
    query$per_page <- per_page
    query$page <- page
    result <- request(path, query)
    if (!is.list(result) || !is.null(names(result))) {
      stop("Expected a GitHub result array for ", path, ".")
    }
    if (length(result) > 0 && identical(result, previous)) {
      stop("GitHub repeated a result page for ", path, "; refusing incomplete pagination.")
    }
    items <- c(items, result)
    if (length(result) < per_page) {
      return(items)
    }
    previous <- result
    page <- page + 1L
  }
}

github_search <- function(request, path, terms, date_field, start, end, per_page = 100L) {
  per_page <- github_page_size(per_page)
  query <- list(
    q = paste(
      terms,
      paste0(date_field, ":", start, "T00:00:00Z..", end, "T23:59:59Z")
    ),
    sort = if (path == "/search/commits") "author-date" else "created",
    order = "asc", per_page = per_page, page = 1L
  )
  first <- request(path, query)
  valid_result <- function(result) {
    is.list(result) && is.numeric(result$total_count) &&
      length(result$total_count) == 1L && is.finite(result$total_count) &&
      result$total_count >= 0 && result$total_count == floor(result$total_count) &&
      is.logical(result$incomplete_results) && length(result$incomplete_results) == 1L &&
      !is.na(result$incomplete_results) && is.list(result$items) &&
      is.null(names(result$items))
  }
  if (!valid_result(first)) {
    stop("GitHub returned an invalid search response for ", path, ".")
  }
  if (first$total_count > 1000 || first$incomplete_results) {
    if (start >= end) {
      stop(
        "GitHub search is incomplete or exceeds 1,000 results on ", start,
        "; refusing to generate a truncated activity profile."
      )
    }
    middle <- start + floor(as.numeric(end - start) / 2)
    return(c(
      github_search(request, path, terms, date_field, start, middle, per_page),
      github_search(request, path, terms, date_field, middle + 1, end, per_page)
    ))
  }
  items <- first$items
  pages <- ceiling(first$total_count / per_page)
  if (pages > 1L) {
    for (page in 2:pages) {
      query$page <- page
      result <- request(path, query)
      if (!valid_result(result) || result$incomplete_results ||
        result$total_count != first$total_count) {
        stop("GitHub search changed or became incomplete while paging; retry collection.")
      }
      items <- c(items, result$items)
    }
  }
  if (length(items) != first$total_count) {
    stop("GitHub search result count does not match its pagination; retry collection.")
  }
  items
}

github_string <- function(value, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("GitHub response is missing a valid ", field, ".")
  }
  value
}

github_id <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
    value <= 0 || value != floor(value)) {
    stop("GitHub response is missing a valid numeric repository or PR ID.")
  }
  format(value, scientific = FALSE, trim = TRUE)
}

normalize_repository <- function(repo, username, source) {
  for (field in c("private", "fork", "archived", "disabled")) {
    if (!is.logical(repo[[field]]) || length(repo[[field]]) != 1L || is.na(repo[[field]])) {
      stop("GitHub repository response is missing ", field, ".")
    }
  }
  if (repo$private) {
    stop("Refusing to collect or publish a private repository.")
  }
  if (!is.numeric(repo$size) || length(repo$size) != 1L ||
    !is.finite(repo$size) || repo$size < 0) {
    stop("GitHub repository size must be a finite, non-negative number.")
  }
  data.frame(
    id = github_id(repo$id),
    full_name = github_string(repo$full_name, "repository full_name"),
    owned = tolower(github_string(repo$owner$login, "repository owner")) == username,
    private = FALSE, fork = repo$fork, archived = repo$archived,
    disabled = repo$disabled, size = repo$size,
    default_branch = if (repo$size == 0 && is.null(repo$default_branch)) {
      ""
    } else {
      github_string(repo$default_branch, "default_branch")
    },
    sources = source
  )
}

fetch_activity_snapshot <- function(username, reference_date = as.Date(Sys.time(), tz = "UTC"),
                                    proxy = NULL, request = NULL,
                                    per_page = github_page_size()) {
  snapshot <- new_activity_snapshot(username, reference_date)
  username <- snapshot$metadata$username
  if (is.null(request)) {
    request <- github_client(proxy = proxy)
  }
  start <- as.Date(snapshot$metadata$window_start)
  end <- as.Date(snapshot$metadata$window_end)
  is_target_author <- function(actor) {
    if (is.null(actor) || tolower(github_string(actor, "activity author login")) != username) {
      snapshot$metadata$excluded_author_records <<- snapshot$metadata$excluded_author_records + 1L
      return(FALSE)
    }
    TRUE
  }
  add_repo <- function(repo, source) {
    row <- normalize_repository(repo, username, source)
    index <- match(row$id, snapshot$repositories$id)
    if (is.na(index)) {
      snapshot$repositories <<- rbind(snapshot$repositories, row)
    } else {
      sources <- strsplit(snapshot$repositories$sources[index], ",", fixed = TRUE)[[1]]
      snapshot$repositories$sources[index] <<- paste(sort(unique(c(sources, source))), collapse = ",")
    }
    row$id
  }
  add_activity <- function(repository_id, kind, id, actor, occurred_at) {
    actor <- tolower(github_string(actor, "activity author login"))
    if (actor != username) {
      stop("GitHub returned activity not authored by the requested user.")
    }
    time <- parse_activity_time(github_string(occurred_at, "activity timestamp"))
    date <- as.Date(time, tz = "UTC")
    if (date < start || date > end) {
      return(invisible(NULL))
    }
    snapshot$activities <<- rbind(snapshot$activities, data.frame(
      repository_id = repository_id, kind = kind,
      id = github_string(id, "activity identity"), actor = actor,
      occurred_at = format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ))
  }
  message("Discovering owned public repositories for ", username, ".")
  owned <- github_list(request, paste0("/users/", username, "/repos"), list(
    type = "owner", sort = "full_name", direction = "asc"
  ), per_page)
  for (repo in owned) {
    add_repo(repo, "owned")
  }

  message("Discovering public authored commits and merged pull requests.")
  commits <- github_search(
    request, "/search/commits", paste("is:public", paste0("author:", username)),
    "author-date", start, end, per_page
  )
  pulls <- github_search(
    request, "/search/issues", paste("is:public is:pr is:merged", paste0("author:", username)),
    "merged", start, end, per_page
  )
  repository_for <- function(full_name, source) {
    index <- match(tolower(full_name), tolower(snapshot$repositories$full_name))
    if (is.na(index)) {
      return(add_repo(request(paste0("/repos/", full_name)), source))
    }
    sources <- strsplit(snapshot$repositories$sources[index], ",", fixed = TRUE)[[1]]
    snapshot$repositories$sources[index] <<- paste(sort(unique(c(sources, source))), collapse = ",")
    snapshot$repositories$id[index]
  }
  for (commit in commits) {
    if (!is_target_author(commit$author$login)) {
      next
    }
    full_name <- github_string(commit$repository$full_name, "commit repository")
    id <- repository_for(full_name, "commit_search")
    add_activity(id, "commit", commit$sha, commit$author$login, commit$commit$author$date)
  }
  for (pull in pulls) {
    if (!is_target_author(pull$user$login)) {
      next
    }
    url <- github_string(pull$repository_url, "PR repository URL")
    if (!grepl("^https://api[.]github[.]com/repos/[^/]+/[^/]+$", url)) {
      stop("GitHub returned an invalid PR repository URL.")
    }
    full_name <- sub("^https://api[.]github[.]com/repos/", "", url)
    id <- repository_for(full_name, "merged_pr")
    merged_at <- pull$pull_request$merged_at
    if (is.null(merged_at)) {
      detail <- request(paste0("/repos/", full_name, "/pulls/", github_id(pull$number)))
      if (!isTRUE(detail$merged) || is.null(detail$merged_at)) {
        stop("A PR search result could not be verified as merged.")
      }
      merged_at <- detail$merged_at
    }
    add_activity(id, "pull_request", github_id(pull$id), pull$user$login, merged_at)
  }

  # Owned forks and unindexed owned repositories need a direct default-branch lookup.
  for (i in seq_len(nrow(snapshot$repositories))) {
    repo <- snapshot$repositories[i, ]
    if (repo$disabled || repo$size == 0) {
      next
    }
    if (repo$owned) {
      commits <- github_list(request, paste0("/repos/", repo$full_name, "/commits"), list(
        author = username, sha = repo$default_branch,
        since = paste0(start, "T00:00:00Z"), until = paste0(end, "T23:59:59Z")
      ), per_page)
      for (commit in commits) {
        if (!is_target_author(commit$author$login)) {
          next
        }
        add_activity(repo$id, "commit", commit$sha, commit$author$login, commit$commit$author$date)
      }
    }
    if (repo$id %in% snapshot$activities$repository_id) {
      result <- request(paste0("/repos/", repo$full_name, "/languages"))
      if (!is.list(result) || (length(result) > 0 && is.null(names(result)))) {
        stop("GitHub returned an invalid repository language map.")
      }
      for (language in names(result)) {
        bytes <- result[[language]]
        if (!is.numeric(bytes) || length(bytes) != 1L || !is.finite(bytes) || bytes < 0) {
          stop("GitHub returned an invalid language byte count.")
        }
        snapshot$languages <- rbind(snapshot$languages, data.frame(
          repository_id = repo$id, language = language, bytes = bytes
        ))
      }
    }
  }
  snapshot$activities <- unique(snapshot$activities)
  if (snapshot$metadata$excluded_author_records > 0) {
    warning(
      "Excluded ", snapshot$metadata$excluded_author_records,
      " records without a matching linked primary author; see excluded_author_records in the summary."
    )
  }
  snapshot$metadata$complete <- TRUE
  validate_activity_snapshot(snapshot)
  snapshot
}

read_activity_cache <- function(path, username, reference_date) {
  if (!file.exists(path)) {
    return(NULL)
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (!jsonlite::validate(text)) {
    stop("Activity cache is invalid JSON; remove it and retry: ", path)
  }
  snapshot <- jsonlite::fromJSON(text)
  template <- new_activity_snapshot(username, reference_date)
  for (name in c("repositories", "languages", "activities")) {
    if (is.list(snapshot[[name]]) && length(snapshot[[name]]) == 0L) {
      snapshot[[name]] <- template[[name]]
    }
  }
  validate_activity_snapshot(snapshot)
  if (!identical(snapshot$metadata$username, template$metadata$username) ||
    !identical(snapshot$metadata$window_end, template$metadata$window_end)) {
    stop("Activity cache belongs to a different user or UTC window; refresh it: ", path)
  }
  snapshot
}

write_activity_cache <- function(snapshot, path) {
  validate_activity_snapshot(snapshot)
  temporary <- tempfile("activity-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(snapshot, temporary, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically save the activity snapshot: ", path)
  }
  invisible(path)
}
