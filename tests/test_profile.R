profile_fixture <- function(reference_date = as.Date("2026-10-25")) {
  snapshot <- new_activity_snapshot("tester", reference_date)
  snapshot$metadata$complete <- TRUE
  snapshot$repositories <- data.frame(
    id = c("1", "2", "3"),
    full_name = c("tester/one", "team/two", "tester/dormant"),
    owned = c(TRUE, FALSE, TRUE), private = FALSE,
    fork = FALSE, archived = FALSE, disabled = FALSE,
    size = 100, default_branch = "main",
    sources = c("owned", "commit_search", "owned")
  )
  snapshot$languages <- data.frame(
    repository_id = c("1", "1", "2"),
    language = c("R", "Go", "Go"), bytes = c(80, 20, 100)
  )
  snapshot$activities <- data.frame(
    repository_id = c("1", "1", "2"),
    kind = c("commit", "pull_request", "commit"),
    id = c("commit-one", "pr-one", "commit-two"),
    actor = "tester",
    occurred_at = paste0(as.character(reference_date - c(0, 0, 100)), "T12:00:00Z")
  )
  snapshot
}

run_test("personal active days allocate normalized multi-language scores", {
  profile <- build_activity_profile(profile_fixture())
  denominator <- sum(2^(-(0:364) / 365))
  scores <- stats::setNames(profile$languages$final_score, profile$languages$language)
  stopifnot(
    isTRUE(all.equal(unname(scores["R"]), 0.8 / denominator)),
    isTRUE(all.equal(unname(scores["Go"]), (0.2 + 2^(-100 / 365)) / denominator)),
    isTRUE(all.equal(sum(scores), sum(profile$repositories$repository_score))),
    isTRUE(all.equal(sum(profile$languages$weighted_active_days), 1 + 2^(-100 / 365))),
    all(profile$languages$total_score == profile$languages$final_score),
    sum(profile$languages$percentage) == 100,
    profile$summary$scored_repositories == 2,
    sum(profile$languages$repo_count) == 3,
    profile$summary$scored_owned_repositories == 1,
    profile$summary$scored_contributed_repositories == 1,
    profile$summary$user_active_days == 2,
    profile$repositories$exclusion_reason[profile$repositories$id == "3"] == "no_personal_activity"
  )
})

run_test("repository byte size cannot amplify an activity score", {
  snapshot <- profile_fixture()
  before <- build_activity_profile(snapshot)
  snapshot$languages$bytes[snapshot$languages$repository_id == "1"] <-
    snapshot$languages$bytes[snapshot$languages$repository_id == "1"] * 1e6
  after <- build_activity_profile(snapshot)
  stopifnot(isTRUE(all.equal(before$languages, after$languages)))
})

run_test("commit bursts and merged PRs on the same UTC day do not inflate activity", {
  snapshot <- profile_fixture()
  before <- build_activity_profile(snapshot)
  burst <- snapshot$activities[rep(1, 100), ]
  burst$id <- paste0("burst-", seq_len(100))
  snapshot$activities <- rbind(snapshot$activities, burst, snapshot$activities)
  after <- build_activity_profile(snapshot)
  stopifnot(
    isTRUE(all.equal(before$languages, after$languages)),
    after$repositories$active_days[after$repositories$id == "1"] == 1
  )
  snapshot$activities$occurred_at[nrow(snapshot$activities)] <- "2026-07-18T12:00:00Z"
  expect_error(build_activity_profile(snapshot), "conflicting timestamps")
})

run_test("all activity days decay and the rolling window leaves no count bonus", {
  snapshot <- profile_fixture()
  before <- build_activity_profile(snapshot)
  snapshot$metadata$window_start <- as.character(as.Date(snapshot$metadata$window_start) + 1)
  snapshot$metadata$window_end <- as.character(as.Date(snapshot$metadata$window_end) + 1)
  after <- build_activity_profile(snapshot)
  stopifnot(isTRUE(all.equal(
    after$languages$final_score, before$languages$final_score * 2^(-1 / 365)
  )))
  snapshot <- profile_fixture()
  snapshot$activities <- snapshot$activities[rep(1, 365), ]
  snapshot$activities$id <- paste0("daily-", seq_len(365))
  snapshot$activities$occurred_at <- paste0(
    as.Date(snapshot$metadata$window_end) - 0:364, "T00:00:00Z"
  )
  profile <- build_activity_profile(snapshot)
  stopifnot(
    isTRUE(all.equal(profile$repositories$repository_score[profile$repositories$id == "1"], 1)),
    isTRUE(all.equal(sum(profile$languages$final_score), 1))
  )
  snapshot$activities$occurred_at[365] <- paste0(
    as.Date(snapshot$metadata$window_end) - 365, "T23:59:59Z"
  )
  expect_error(build_activity_profile(snapshot), "outside")
})

run_test("ownership and archived status are context rather than usage multipliers", {
  snapshot <- profile_fixture()
  before <- build_activity_profile(snapshot)
  snapshot$repositories$archived <- TRUE
  after <- build_activity_profile(snapshot)
  stopifnot(isTRUE(all.equal(before$languages, after$languages)))
  stopifnot(!any(c("star_score", "fork_score", "base_score") %in% names(after$languages)))
})

run_test("shared commits are credited once but unique fork development is retained", {
  snapshot <- profile_fixture()
  fork <- snapshot$repositories[1, ]
  fork$id <- "4"
  fork$full_name <- "tester/fork"
  fork$fork <- TRUE
  snapshot$repositories <- rbind(snapshot$repositories, fork)
  snapshot$languages <- rbind(snapshot$languages, data.frame(
    repository_id = "4", language = "Rust", bytes = 100
  ))
  shared <- snapshot$activities[snapshot$activities$id == "commit-two", ]
  shared$repository_id <- "4"
  unique_commit <- transform(
    shared,
    id = "fork-only", occurred_at = "2026-10-24T12:00:00Z"
  )
  snapshot$activities <- rbind(snapshot$activities, shared, unique_commit)
  profile <- build_activity_profile(snapshot)
  stopifnot(
    profile$summary$scored_repositories == 3,
    profile$repositories$active_days[profile$repositories$id == "4"] == 1,
    profile$repositories$commit_count[profile$repositories$id == "2"] == 1,
    "Rust" %in% profile$languages$language
  )
  snapshot$activities <- snapshot$activities[rev(seq_len(nrow(snapshot$activities))), ]
  reordered <- build_activity_profile(snapshot)
  stopifnot(isTRUE(all.equal(profile, reordered)))
  snapshot$activities <- snapshot$activities[snapshot$activities$id != "fork-only", ]
  profile <- build_activity_profile(snapshot)
  stopifnot(
    !"Rust" %in% profile$languages$language,
    profile$repositories$exclusion_reason[profile$repositories$id == "4"] == "duplicate_activity"
  )
})

run_test("disabled and empty repositories do not receive language credit", {
  snapshot <- profile_fixture()
  snapshot$repositories$disabled[1] <- TRUE
  snapshot$repositories$size[2] <- 0
  profile <- build_activity_profile(snapshot)
  stopifnot(
    profile$summary$status == "no_scorable_activity",
    profile$summary$scored_repositories == 0,
    nrow(profile$languages) == 0,
    setequal(profile$repositories$exclusion_reason, c("disabled", "empty", "no_personal_activity"))
  )
})

run_test("missing activity and undetected languages are explicit distinct states", {
  snapshot <- profile_fixture()
  snapshot$activities <- snapshot$activities[FALSE, ]
  profile <- build_activity_profile(snapshot)
  stopifnot(
    profile$summary$status == "no_scorable_activity",
    all(profile$repositories$exclusion_reason == "no_personal_activity"),
    profile$summary$user_active_days == 0
  )
  snapshot <- profile_fixture()
  snapshot$languages <- snapshot$languages[FALSE, ]
  profile <- build_activity_profile(snapshot)
  stopifnot(
    profile$summary$status == "no_scorable_activity",
    sum(profile$repositories$exclusion_reason == "no_detected_languages") == 2
  )
  snapshot <- new_activity_snapshot("tester", as.Date("2026-10-25"))
  snapshot$metadata$complete <- TRUE
  profile <- build_activity_profile(snapshot)
  stopifnot(profile$summary$public_repositories == 0, nrow(profile$languages) == 0)
})

run_test("activity timezone offsets and fractional seconds normalize to UTC dates", {
  parsed <- parse_activity_time(c(
    "2026-10-25T00:30:00.123+08:00", "2026-10-24T16:30:00.123Z",
    "2026-10-24T23:30:00-02:00"
  ))
  stopifnot(
    parsed[1] == parsed[2],
    identical(as.character(as.Date(parsed, tz = "UTC")), c("2026-10-24", "2026-10-24", "2026-10-25"))
  )
  for (value in c(
    "not-a-date", "2026-02-30T00:00:00Z", "2026-10-25",
    "2026-10-25T24:00:00Z", "2026-10-25T12:00:00+99:99"
  )) {
    expect_error(parse_activity_time(value), "timestamp")
  }
})

run_test("schema validation rejects missing private foreign and invalid evidence", {
  for (field in c("schema_version", "visibility", "complete", "sources", "window_start")) {
    snapshot <- profile_fixture()
    snapshot$metadata[[field]] <- NULL
    expect_error(build_activity_profile(snapshot))
  }
  for (field in c("private", "owned", "disabled", "size")) {
    snapshot <- profile_fixture()
    snapshot$repositories[[field]] <- NULL
    expect_error(build_activity_profile(snapshot), "table")
  }
  for (value in c(-1, Inf, NA_real_)) {
    snapshot <- profile_fixture()
    snapshot$languages$bytes[1] <- value
    expect_error(build_activity_profile(snapshot), "bytes")
  }
  snapshot <- profile_fixture()
  snapshot$repositories$private[1] <- TRUE
  expect_error(build_activity_profile(snapshot), "public")
  snapshot <- profile_fixture()
  snapshot$activities$actor[1] <- "another-user"
  expect_error(build_activity_profile(snapshot), "target user")
  snapshot <- profile_fixture()
  snapshot$activities$kind[1] <- "issue"
  expect_error(build_activity_profile(snapshot), "authored commit")
  snapshot <- profile_fixture()
  snapshot$activities$repository_id[1] <- "unknown"
  expect_error(build_activity_profile(snapshot), "known public")
  snapshot <- profile_fixture()
  snapshot$activities$occurred_at[1] <- "2026-10-26T00:00:00Z"
  expect_error(build_activity_profile(snapshot), "outside")
  snapshot <- profile_fixture()
  snapshot$languages <- rbind(snapshot$languages, snapshot$languages[1, ])
  expect_error(build_activity_profile(snapshot), "unique")
  expect_error(build_activity_profile(data.frame(language = "R")), "snapshot")
})

run_test("GitHub usernames and reference dates are validated before collection", {
  for (username in c("", "../secrets", "user/name", "user query", "-user", "user--name")) {
    expect_error(new_activity_snapshot(username), "GITHUB_USERNAME")
  }
  stopifnot(new_activity_snapshot("Test-User")$metadata$username == "test-user")
  expect_error(new_activity_snapshot("tester", "2026-10-25"), "reference_date")
})
