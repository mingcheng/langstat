api_repository <- function(id, full_name, fork = FALSE) {
  list(
    id = id, full_name = full_name, owner = list(login = sub("/.*", "", full_name)),
    private = FALSE, fork = fork, archived = FALSE, disabled = FALSE,
    size = 100, default_branch = "main"
  )
}

api_commit <- function(sha, full_name, date = "2026-10-25T12:00:00Z") {
  list(
    sha = sha, repository = list(full_name = full_name),
    author = list(login = "tester"), commit = list(author = list(date = date))
  )
}

api_search_result <- function(items, total = length(items), incomplete = FALSE) {
  list(total_count = total, incomplete_results = incomplete, items = items)
}

run_test("owned repositories paginate beyond the first API page", {
  pages <- integer()
  result <- github_list(function(path, query) {
    pages <<- c(pages, query$page)
    stopifnot(path == "/users/tester/repos", query$per_page == 100)
    if (query$page == 1) as.list(1:100) else as.list(101:137)
  }, "/users/tester/repos")
  stopifnot(length(result) == 137, identical(pages, 1:2))
  expect_error(github_list(function(path, query) list("same"), "/repos", per_page = 1), "repeated")
  for (value in c("", "NaN", "0", "101", "1.5", "invalid")) {
    expect_error(github_page_size(value), "GITHUB_PER_PAGE")
  }
})

run_test("search pagination covers all results with explicit UTC bounds", {
  queries <- list()
  result <- github_search(
    function(path, query) {
      queries[[length(queries) + 1L]] <<- query
      page <- list(as.list(1:2), as.list(3:4), list(5))[[query$page]]
      api_search_result(page, total = 5)
    }, "/search/commits", "is:public author:tester", "author-date",
    as.Date("2026-10-24"), as.Date("2026-10-25"),
    per_page = 2
  )
  stopifnot(
    identical(unlist(result), c(1, 2, 3, 4, 5)),
    length(queries) == 3,
    grepl("author-date:2026-10-24T00:00:00Z..2026-10-25T23:59:59Z", queries[[1]]$q, fixed = TRUE)
  )
})

run_test("large and incomplete searches split into non-overlapping date windows", {
  for (incomplete in c(FALSE, TRUE)) {
    queries <- character()
    result <- github_search(
      function(path, query) {
        queries <<- c(queries, query$q)
        if (length(queries) == 1) {
          return(api_search_result(list(), total = if (incomplete) 0 else 1001, incomplete = incomplete))
        }
        api_search_result(list(length(queries)))
      }, "/search/commits", "is:public author:tester", "author-date",
      as.Date("2026-10-24"), as.Date("2026-10-25")
    )
    stopifnot(
      length(result) == 2, length(queries) == 3,
      grepl("2026-10-24T00:00:00Z..2026-10-24T23:59:59Z", queries[2], fixed = TRUE),
      grepl("2026-10-25T00:00:00Z..2026-10-25T23:59:59Z", queries[3], fixed = TRUE)
    )
  }
})

run_test("search truncation changing totals and incomplete pages fail explicitly", {
  expect_error(github_search(
    function(path, query) api_search_result(list(), 1001),
    "/search/commits", "author:tester", "author-date",
    as.Date("2026-10-25"), as.Date("2026-10-25")
  ), "1,000")
  expect_error(github_search(
    function(path, query) api_search_result(list(), 1),
    "/search/commits", "author:tester", "author-date",
    as.Date("2026-10-25"), as.Date("2026-10-25")
  ), "count")
  expect_error(github_search(
    function(path, query) api_search_result(list(query$page), 2, query$page == 2),
    "/search/commits", "author:tester", "author-date",
    as.Date("2026-10-25"), as.Date("2026-10-25"),
    per_page = 1
  ), "incomplete")
})

run_test("collection combines owned authored and merged-PR sources without primary-language guesses", {
  calls <- list()
  request <- function(path, query = list()) {
    calls[[length(calls) + 1L]] <<- list(path = path, query = query)
    if (path == "/users/tester/repos") {
      stopifnot(query$type == "owner")
      return(if (query$page == 1L) {
        list(
          api_repository(1, "tester/one"), api_repository(2, "tester/fork", fork = TRUE)
        )
      } else {
        list(api_repository(3, "tester/dormant"))
      })
    }
    if (path == "/search/commits") {
      stopifnot(grepl("is:public author:tester", query$q, fixed = TRUE))
      return(api_search_result(list(
        api_commit("own", "tester/one"), api_commit("external", "team/project")
      )))
    }
    if (path == "/search/issues") {
      stopifnot(grepl("is:public is:pr is:merged author:tester", query$q, fixed = TRUE))
      return(api_search_result(list(list(
        id = 42, number = 7, user = list(login = "tester"),
        repository_url = "https://api.github.com/repos/team/project",
        pull_request = list(merged_at = NULL)
      ))))
    }
    if (path == "/repos/team/project") {
      return(api_repository(4, "team/project"))
    }
    if (path == "/repos/team/project/pulls/7") {
      return(list(merged = TRUE, merged_at = "2026-10-24T00:30:00+08:00"))
    }
    if (path == "/repos/tester/one/commits") {
      stopifnot(
        query$author == "tester", query$sha == "main",
        query$since == "2025-10-26T00:00:00Z",
        query$until == "2026-10-25T23:59:59Z"
      )
      return(if (query$page == 1) {
        list(
          api_commit("own", "tester/one"),
          api_commit("old", "tester/one", "2025-10-25T00:00:00Z")
        )
      } else {
        list()
      })
    }
    if (path == "/repos/tester/fork/commits") {
      return(list(api_commit("fork-only", "tester/fork")))
    }
    if (path == "/repos/tester/dormant/commits") {
      return(list())
    }
    if (path %in% c("/repos/tester/one/languages", "/repos/team/project/languages")) {
      return(list(R = 80, Go = 20))
    }
    if (path == "/repos/tester/fork/languages") {
      return(list(Rust = 100))
    }
    stop("Unexpected API call: ", path)
  }
  snapshot <- suppressMessages(fetch_activity_snapshot(
    "Tester", as.Date("2026-10-25"),
    request = request, per_page = 2
  ))
  profile <- build_activity_profile(snapshot)
  stopifnot(
    snapshot$metadata$complete,
    nrow(snapshot$repositories) == 4,
    nrow(snapshot$activities) == 4,
    nrow(snapshot$languages) == 5,
    sum(snapshot$activities$id == "own") == 1,
    snapshot$activities$occurred_at[snapshot$activities$id == "42"] == "2026-10-23T16:30:00Z",
    profile$summary$scored_repositories == 3,
    profile$summary$scored_contributed_repositories == 1,
    !any(vapply(calls, function(call) call$path == "/repos/tester/dormant/languages", logical(1)))
  )
})

run_test("private repositories fail and unmatched authors are excluded with an audit count", {
  repo <- api_repository(1, "tester/private")
  repo$private <- TRUE
  expect_error(normalize_repository(repo, "tester", "owned"), "private")
  repo$private <- NULL
  expect_error(normalize_repository(repo, "tester", "owned"), "private")
  request <- function(path, query = list()) {
    if (path == "/users/tester/repos") {
      return(list(api_repository(1, "tester/one")))
    }
    if (path == "/search/commits") {
      commit <- api_commit("wrong", "tester/one")
      commit$author$login <- "someone-else"
      return(api_search_result(list(commit)))
    }
    if (path == "/search/issues") {
      return(api_search_result(list()))
    }
    if (path == "/repos/tester/one/commits") {
      commit <- api_commit("unlinked", "tester/one")
      commit$author <- NULL
      return(list(commit))
    }
    stop("Unexpected request")
  }
  warnings <- character()
  snapshot <- withCallingHandlers(
    suppressMessages(fetch_activity_snapshot("tester", as.Date("2026-10-25"), request = request)),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(
    snapshot$metadata$excluded_author_records == 2,
    nrow(snapshot$activities) == 0,
    length(warnings) == 1, grepl("Excluded 2 records", warnings, fixed = TRUE)
  )
})

run_test("a PR without a verified merge timestamp cannot enter an activity snapshot", {
  request <- function(path, query = list()) {
    if (path == "/users/tester/repos") {
      return(list(api_repository(1, "tester/one")))
    }
    if (path == "/search/commits") {
      return(api_search_result(list()))
    }
    if (path == "/search/issues") {
      return(api_search_result(list(list(
        id = 42, number = 7, user = list(login = "tester"),
        repository_url = "https://api.github.com/repos/tester/one",
        pull_request = list(merged_at = NULL)
      ))))
    }
    if (path == "/repos/tester/one/pulls/7") {
      return(list(merged = FALSE, merged_at = NULL))
    }
    stop("Unexpected request")
  }
  expect_error(suppressMessages(fetch_activity_snapshot(
    "tester", as.Date("2026-10-25"),
    request = request
  )), "verified as merged")
})

mock_response <- function(status, body = list(), headers = list()) {
  structure(list(
    status_code = status,
    headers = c(list(`content-type` = "application/json; charset=utf-8"), headers),
    content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)),
    url = "https://api.github.com/test"
  ), class = "response")
}

run_test("GitHub transport retries transient and secondary limits with bounded waits", {
  for (status in c(503L, 429L, 403L)) {
    attempts <- 0L
    waits <- numeric()
    request <- github_client(token = "test-token", transport = function(...) {
      attempts <<- attempts + 1L
      if (attempts > 1L) {
        return(mock_response(200L, list(ok = TRUE)))
      }
      mock_response(
        status, list(message = "You have exceeded a secondary rate limit."),
        if (status == 429L) list(`retry-after` = "2") else list()
      )
    }, sleep = function(seconds) waits <<- c(waits, seconds))
    result <- suppressWarnings(request("/test"))
    stopifnot(
      result$ok, attempts == 2L,
      identical(waits, if (status == 503L) 1 else if (status == 429L) 2 else 60)
    )
  }
})

run_test("GitHub authentication quota failures and unsafe redirects never look successful", {
  calls <- 0L
  request <- github_client(token = "DUMMY-SECRET", transport = function(...) {
    calls <<- calls + 1L
    mock_response(401L)
  }, sleep = function(seconds) stop("Should not retry authentication"))
  expect_error(request("/test"), "HTTP 401")
  stopifnot(calls == 1L)
  request <- github_client(
    token = "test-token", transport = function(...) {
      mock_response(403L, headers = list(`x-ratelimit-remaining` = "0", `x-ratelimit-reset` = "1000"))
    }, now = function() as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
    sleep = function(seconds) stop("Should not wait for an exhausted hourly quota")
  )
  expect_error(request("/test"), "quota")
  request <- github_client(token = "DUMMY-SECRET", transport = function(...) {
    mock_response(301L, headers = list(location = "https://example.com/steal-token"))
  })
  expect_error(request("/test"), "non-GitHub")
  expect_error(request("https://example.com/test"), "non-GitHub")
})

run_test("transport failures retry finitely without exposing credentials", {
  attempts <- 0L
  request <- github_client(token = "DUMMY-SECRET", transport = function(...) {
    attempts <<- attempts + 1L
    stop("Transport failed with DUMMY-SECRET")
  }, sleep = function(seconds) invisible(NULL))
  error <- tryCatch(suppressWarnings(request("/test")), error = identity)
  stopifnot(
    inherits(error, "error"), attempts == 4L,
    !grepl("DUMMY-SECRET", conditionMessage(error), fixed = TRUE)
  )
})

run_test("versioned snapshots round-trip and reject stale foreign or corrupt caches", local({
  directory <- tempfile("langstat-cache-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "activity_v2_20261025.json")
  snapshot <- profile_fixture()
  stopifnot(is.null(read_activity_cache(path, "tester", as.Date("2026-10-25"))))
  write_activity_cache(snapshot, path)
  cached <- read_activity_cache(path, "tester", as.Date("2026-10-25"))
  stopifnot(isTRUE(all.equal(build_activity_profile(snapshot), build_activity_profile(cached))))
  expect_error(read_activity_cache(path, "other", as.Date("2026-10-25")), "different user")
  expect_error(read_activity_cache(path, "tester", as.Date("2026-10-26")), "UTC window")
  writeLines("{broken", path)
  expect_error(read_activity_cache(path, "tester", as.Date("2026-10-25")), "invalid JSON")
  snapshot$metadata$complete <- FALSE
  expect_error(write_activity_cache(snapshot, path), "incomplete")
  snapshot <- new_activity_snapshot("tester", as.Date("2026-10-25"))
  snapshot$metadata$complete <- TRUE
  write_activity_cache(snapshot, path)
  cached <- read_activity_cache(path, "tester", as.Date("2026-10-25"))
  stopifnot(is.data.frame(cached$activities), nrow(cached$activities) == 0)
}))
