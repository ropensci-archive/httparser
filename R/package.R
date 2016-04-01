parse_block <- function(x, ..., scheme = "http") {
  lines <- strsplit(x, "\n")[[1]]
  first_blank <- which(lines == "")
  if (length(first_blank) > 0) {
    first_blank <- first_blank[1]
    body <- paste(lines[seq(first_blank + 1, length(lines))], collapse = "\n")
  } else {
    first_blank <- length(lines) + 1
    body <- NULL
  }

  res <- c(parse_request_line(lines[1]),
    list(headers = parse_headers(lines[seq(2, first_blank - 1)])),
    body = list(body))

  res$parts$hostname <- res$parts$hostname %||% res$headers$Host
  res$headers$Host <- NULL
  res$parts$scheme <- scheme
  res$query <- res$parts$query
  res$parts$query <- NULL

  res <- replace_params(res, ...)
  attr(res, "substitutions") <- unlist(list(...))
  res
}

`%||%` <- function(x, y) if (is.null(x)) y else x

unquote <- function(e, where) {
  if (is.pairlist(e))
    as.pairlist(lapply(e, unquote))
  else if (length(e) <= 1L)
    e
  else if (e[[1L]] == as.name("."))
    eval(e[[2L]], where)
  else as.call(lapply(e, unquote))
}

replace_params <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x)) {
     lapply(x, replace_params, ...)
  } else {
    params <- list(...)
    for (i in seq_along(params)) {
      if (grepl(names(params)[i], x)) {
        string <- gsub(names(params)[i], "%s", x)
        x <- unquote(parse(text = paste0("sprintf(\"", string, "\", ", params[i], ")")))[[1]]
      }
    }
    x
  }
}

parse_request_line <- function(x) {
  split <- strsplit(x, "[[:space:]]+")[[1]]
  stopifnot(length(split) >= 2)
  parts <- httr::parse_url(split[[2]])
  parts$path <- gsub("^/", "", parts$path)
  list(verb = split[[1]], parts = parts)
}

parse_headers <- function(x) {
  r <- regexpr("^([^:]+):[[:space:]]*(.*)", x, perl = TRUE)
  starts <- attr(r, "capture.start")
  res <- substring(x, starts, starts + attr(r, "capture.length") - 1L)

  values <- res[seq(length(res) / 2 + 1, length(res))]
  names(values) <- res[seq(1, length(res) / 2)]
  as.list(values)
}
