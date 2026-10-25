validate_github_username <- function(username) {
  if (!is.character(username) || length(username) != 1L || is.na(username) ||
    !grepl("^[A-Za-z0-9]([A-Za-z0-9-]{0,37}[A-Za-z0-9])?$", username) ||
    grepl("--", username, fixed = TRUE)) {
    stop("GITHUB_USERNAME must be a valid GitHub user login.")
  }
  tolower(username)
}

validate_reference_date <- function(date) {
  if (!inherits(date, "Date") || length(date) != 1L || is.na(date)) {
    stop("reference_date must be a single, non-missing Date.")
  }
  invisible(date)
}

parse_activity_time <- function(value) {
  pattern <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T([01][0-9]|2[0-3]):",
    "[0-5][0-9]:[0-5][0-9]([.][0-9]+)?(Z|[+-]([01][0-9]|2[0-3]):[0-5][0-9])$"
  )
  if (!is.character(value) || anyNA(value) || any(!grepl(pattern, value))) {
    stop("Activity timestamps must be valid ISO 8601 timestamps with a timezone.")
  }
  normalized <- sub("Z$", "+0000", value)
  normalized <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", normalized)
  parsed <- as.POSIXct(normalized, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
  dates <- as.Date(substr(value, 1, 10), format = "%Y-%m-%d")
  if (anyNA(parsed) || anyNA(dates) ||
    any(format(dates, "%Y-%m-%d") != substr(value, 1, 10))) {
    stop("Activity timestamps contain invalid dates.")
  }
  parsed
}

new_activity_snapshot <- function(username, reference_date = as.Date(Sys.time(), tz = "UTC")) {
  validate_reference_date(reference_date)
  list(
    metadata = list(
      schema_version = 2L,
      model_version = "activity-days-v1",
      username = validate_github_username(username),
      visibility = "public",
      window_days = 365L,
      half_life_days = 365L,
      window_start = as.character(reference_date - 364),
      window_end = as.character(reference_date),
      collected_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      sources = c("owned_default_branch_commits", "public_commit_search", "merged_pull_requests"),
      excluded_author_records = 0L,
      complete = FALSE
    ),
    repositories = data.frame(
      id = character(), full_name = character(), owned = logical(),
      private = logical(), fork = logical(), archived = logical(),
      disabled = logical(), size = numeric(), default_branch = character(),
      sources = character()
    ),
    languages = data.frame(
      repository_id = character(), language = character(), bytes = numeric()
    ),
    activities = data.frame(
      repository_id = character(), kind = character(), id = character(),
      actor = character(), occurred_at = character()
    )
  )
}

validate_activity_snapshot <- function(snapshot) {
  if (!is.list(snapshot)) {
    stop("Activity snapshot must be a list.")
  }
  metadata <- snapshot$metadata
  if (!is.list(metadata) ||
    !isTRUE(metadata$schema_version == 2L) ||
    !identical(metadata$model_version, "activity-days-v1") ||
    !identical(metadata$visibility, "public") ||
    !isTRUE(metadata$window_days == 365L) ||
    !isTRUE(metadata$half_life_days == 365L) ||
    !isTRUE(metadata$complete)) {
    stop("An incompatible or incomplete activity snapshot cannot be scored.")
  }
  username <- validate_github_username(metadata$username)
  start <- as.Date(metadata$window_start, format = "%Y-%m-%d")
  end <- as.Date(metadata$window_end, format = "%Y-%m-%d")
  validate_reference_date(start)
  validate_reference_date(end)
  if (end - start != 364 ||
    !identical(as.character(start), metadata$window_start) ||
    !identical(as.character(end), metadata$window_end)) {
    stop("Activity snapshot must cover exactly 365 inclusive UTC dates.")
  }
  if (length(metadata$collected_at) != 1L) {
    stop("Activity snapshot must identify its collection time.")
  }
  parse_activity_time(metadata$collected_at)
  template <- new_activity_snapshot(username, end)
  if (!identical(metadata$sources, template$metadata$sources)) {
    stop("Activity snapshot has unsupported collection sources.")
  }
  if (!is.numeric(metadata$excluded_author_records) ||
    length(metadata$excluded_author_records) != 1L ||
    !is.finite(metadata$excluded_author_records) ||
    metadata$excluded_author_records < 0 ||
    metadata$excluded_author_records != floor(metadata$excluded_author_records)) {
    stop("Activity snapshot must count excluded author records explicitly.")
  }
  for (name in c("repositories", "languages", "activities")) {
    table <- snapshot[[name]]
    expected <- template[[name]]
    if (!is.data.frame(table) || !setequal(names(table), names(expected))) {
      stop("Invalid or missing activity snapshot table: ", name)
    }
    for (column in names(expected)) {
      values <- table[[column]]
      prototype <- expected[[column]]
      valid_type <- if (is.numeric(prototype)) {
        is.numeric(values)
      } else {
        typeof(values) == typeof(prototype)
      }
      if (!valid_type || anyNA(values)) {
        stop(name, "$", column, " has invalid or missing values.")
      }
      if (is.character(values) && column != "default_branch" &&
        any(!nzchar(trimws(values)))) {
        stop(name, "$", column, " must not contain empty strings.")
      }
      if (is.numeric(values) && any(!is.finite(values) | values < 0)) {
        stop(name, "$", column, " must contain finite, non-negative numbers.")
      }
    }
  }
  repos <- snapshot$repositories
  languages <- snapshot$languages
  activities <- snapshot$activities
  if (anyDuplicated(repos$id) || anyDuplicated(tolower(repos$full_name)) ||
    any(repos$private)) {
    stop("Repository identities must be unique and all repositories must be public.")
  }
  if (any(repos$owned != (tolower(sub("/.*", "", repos$full_name)) == username))) {
    stop("Repository ownership must match the target user's login.")
  }
  if (any(repos$size > 0 & !nzchar(repos$default_branch))) {
    stop("Non-empty repositories must identify their default branch.")
  }
  if (any(!languages$repository_id %in% repos$id) ||
    any(!activities$repository_id %in% repos$id)) {
    stop("Language and activity evidence must refer to known public repositories.")
  }
  if (anyDuplicated(languages[c("repository_id", "language")])) {
    stop("Repository language entries must be unique.")
  }
  if (any(!activities$kind %in% c("commit", "pull_request")) ||
    any(tolower(activities$actor) != username)) {
    stop("Activity evidence must be an authored commit or merged PR by the target user.")
  }
  dates <- as.Date(parse_activity_time(activities$occurred_at), tz = "UTC")
  if (any(dates < start | dates > end)) {
    stop("Activity evidence falls outside the snapshot's UTC window.")
  }
  if (nrow(activities) > 0) {
    keys <- paste(activities$kind, activities$id, sep = ":")
    times <- parse_activity_time(activities$occurred_at)
    if (any(vapply(
      split(as.numeric(times), keys), function(x) length(unique(x)) != 1L,
      logical(1)
    ))) {
      stop("Repeated activity identities have conflicting timestamps.")
    }
  }
  invisible(snapshot)
}

build_activity_profile <- function(snapshot) {
  validate_activity_snapshot(snapshot)
  metadata <- snapshot$metadata
  reference_date <- as.Date(metadata$window_end)
  repos <- snapshot$repositories
  languages <- snapshot$languages |>
    dplyr::filter(bytes > 0) |>
    dplyr::group_by(repository_id) |>
    dplyr::mutate(language_share = bytes / sum(bytes)) |>
    dplyr::ungroup()
  eligible <- !repos$disabled & repos$size > 0 & repos$id %in% languages$repository_id
  activities <- snapshot$activities |>
    dplyr::filter(repository_id %in% repos$id[eligible]) |>
    dplyr::distinct()

  # Prefer originals to forks when the same commit is visible in multiple repositories.
  repo_index <- match(activities$repository_id, repos$id)
  activities <- activities[order(
    repos$fork[repo_index], !repos$owned[repo_index],
    repos$full_name[repo_index], activities$kind, activities$id
  ), , drop = FALSE]
  activities <- activities[
    !duplicated(activities[c("kind", "id")]), ,
    drop = FALSE
  ]
  activities$activity_date <- as.Date(parse_activity_time(activities$occurred_at), tz = "UTC")
  days <- activities |>
    dplyr::distinct(repository_id, activity_date) |>
    dplyr::mutate(
      date_weight = 2^(-as.numeric(reference_date - activity_date) / metadata$half_life_days)
    )
  day_scores <- days |>
    dplyr::group_by(repository_id) |>
    dplyr::summarise(
      active_days = dplyr::n(),
      weighted_active_days = sum(date_weight),
      last_activity_on = if (dplyr::n() > 0) as.character(max(activity_date)) else NA_character_,
      .groups = "drop"
    )
  evidence_counts <- activities |>
    dplyr::group_by(repository_id) |>
    dplyr::summarise(
      commit_count = sum(kind == "commit"),
      merged_pr_count = sum(kind == "pull_request"),
      .groups = "drop"
    )
  max_activity <- sum(2^(-(0:364) / metadata$half_life_days))
  repo_scores <- repos |>
    dplyr::left_join(day_scores, by = c("id" = "repository_id")) |>
    dplyr::left_join(evidence_counts, by = c("id" = "repository_id")) |>
    dplyr::mutate(
      dplyr::across(
        c(active_days, weighted_active_days, commit_count, merged_pr_count),
        ~ dplyr::coalesce(.x, 0)
      ),
      repository_score = weighted_active_days / max_activity,
      exclusion_reason = dplyr::case_when(
        disabled ~ "disabled",
        size == 0 ~ "empty",
        !id %in% snapshot$activities$repository_id ~ "no_personal_activity",
        !id %in% languages$repository_id ~ "no_detected_languages",
        active_days > 0 ~ "",
        TRUE ~ "duplicate_activity"
      )
    ) |>
    dplyr::arrange(dplyr::desc(repository_score), full_name)

  allocated <- languages |>
    dplyr::inner_join(
      dplyr::filter(repo_scores, repository_score > 0),
      by = c("repository_id" = "id")
    )
  language_scores <- allocated |>
    dplyr::group_by(language) |>
    dplyr::summarise(
      total_score = sum(repository_score * language_share),
      weighted_active_days = sum(weighted_active_days * language_share),
      repo_count = dplyr::n_distinct(repository_id),
      owned_repo_count = dplyr::n_distinct(repository_id[owned]),
      contributed_repo_count = dplyr::n_distinct(repository_id[!owned]),
      last_activity_on = if (dplyr::n() > 0) max(last_activity_on) else NA_character_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      final_score = total_score,
      percentage = round(final_score / sum(final_score) * 100, 1),
      label = paste0(language, " (", percentage, "%)")
    ) |>
    dplyr::arrange(dplyr::desc(final_score), language)
  if (any(!is.finite(language_scores$final_score)) ||
    any(repo_scores$repository_score > 1 + 1e-12) ||
    !isTRUE(all.equal(sum(language_scores$final_score), sum(repo_scores$repository_score)))) {
    stop("Activity score allocation failed its conservation or boundedness checks.")
  }
  summary <- c(metadata, list(
    status = if (nrow(language_scores) > 0) "ok" else "no_scorable_activity",
    public_repositories = nrow(repos),
    owned_repositories = sum(repos$owned),
    contributed_repositories = sum(!repos$owned),
    scored_repositories = sum(repo_scores$repository_score > 0),
    scored_owned_repositories = sum(repo_scores$repository_score > 0 & repo_scores$owned),
    scored_contributed_repositories = sum(repo_scores$repository_score > 0 & !repo_scores$owned),
    user_active_days = length(unique(days$activity_date)),
    evidence_count = nrow(activities)
  ))
  list(languages = language_scores, repositories = repo_scores, summary = summary)
}
