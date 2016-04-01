# do.call(VERB, list(url = do.call(modify_url, c(url = "", t3$parts)), verb = t3$verb, add_headers(unlist(t3$headers))))

make_function <- function(x) {
  subs <- attr(x, "substitutions")
  args <- alist(Today="test")#as.pairlist(rep(alist(a=), length(subs)))
  #names(args) <- subs
  f <- eval(
      unquote("function(...) {
        headers <- .(headers)
}", list(headers = x$headers)))
        #str(headers)
        #query <- lapply(.(query), eval)
        #str(query)
        #parts <- .(parts)
        #url <- do.call(modify_url, c(url = "", parts))
        #str(url)
        #.(verb)(url = url, query = query, add_headers(.headers = headers))
      #},
        #list(
          #verb = as.name(x$verb),
          #parts = x$parts,
          #headers = x$headers,
          #query = x$query)))
  formals(f) <- args
  f
}
