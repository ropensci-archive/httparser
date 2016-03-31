test <- "POST /auth/github HTTP/1.1
User-Agent: MyClient/1.0.0
Accept: application/vnd.travis-ci.2+json
Host: api.travis-ci.org
Content-Type: application/json
Content-Length: 37

{\"github_token\":\"YOUR GITHUB TOKEN\"}"

test2 <- "GET /users HTTP/1.1
User-Agent: MyClient/1.0.0
Accept: application/vnd.travis-ci.2+json
Host: api.travis-ci.org
Authorization: token \"YOUR TRAVIS ACCESS TOKEN\""

parse_block <- function(x, ...) {
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
  replace_params(res, ...)
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
      x <- gsub(names(params)[i], params[i], x)
    }
    x
  }
}

parse_request_line <- function(x) {
  split <- strsplit(x, "[[:space:]]+")[[1]]
  stopifnot(length(split) >= 2)
  list(verb = split[[1]], path = httr::parse_url(split[[2]]))
}

parse_headers <- function(x) {
  r <- regexpr("^([^:]+):[[:space:]]*(.*)", x, perl = TRUE)
  starts <- attr(r, "capture.start")
  res <- substring(x, starts, starts + attr(r, "capture.length") - 1L)

  values <- res[seq(length(res) / 2 + 1, length(res))]
  names(values) <- res[seq(1, length(res) / 2)]
  as.list(values)
}
