
rCodeValidator <- function(code) {
  # this one should be called from JASP!
  #' @param code a character string with R code

  result <- tryCatch(
    validateStringRCode(code),
    invalidCodeError = function(e) { list(pass = FALSE, reason = e[["message"]])     },
    error            = function(e) { list(pass = FALSE, reason = "unexpected error") }
  )

  return(result) #Actually getting json back is not very practical. A list is better

}

fakeAssign <- function(expr, envir) {
  assign(x = as.character(expr), value = NULL, envir = envir)
}

validateRawRCode <- function(rcode) {
  expr <- substitute(rcode)
  return(validateRExpression(expr))
}

validateStringRCode <- function(rcode) {
  expr <- parse(text = rcode)
  return(validateRExpression(expr))
}

validateRExpression <- function(expr) {
  envir <- new.env(parent = baseenv())
  validateInner(expr, envir)
  return(list(pass = TRUE, reason = ""))
}

validateInner <- function(code, envir) {
  for (i in seq_along(code)) {
    line <- code[[i]]
    validateAndEval(line, envir, i == 1L && is.call(code))
  }
}

validateAndEval <- function(expr, envir, isfunction = FALSE) {

  if (is.symbol(expr)) {

    validateSymbol(expr, envir = envir, isfunction = isfunction)
    return()

  } else if (is.call(expr)) {

    try(print(expr))

    if (is.assignment(expr[[1L]])) {

      if (is.assignment(expr[[3L]]))
        invalidCodeError(gettext("It's not allowed to reassign the assignment operator!"))

      if (length(expr[[3L]]) > 1L)
        validateInner(expr[[3L]], envir = envir)
      else
        validateAndEval(expr[[3L]], envir)

      fakeAssign(expr[[2]], envir = envir)

    } else {

      validateInner(expr, envir = envir)

    }
  }
}

validateSymbol <- function(sym, envir, isfunction) {

  if (!is.symbol(sym))
    return()

  name <- as.character(sym)
  val  <- mget(name, envir = envir,
               ifnotfound = NA, inherits = TRUE,
               mode = if (isfunction) "function" else "any"
  )[[1L]]
  if (!is.function(val))
    return()

  if (!inWhiteList(val))
    invalidCodeError(gettextf("Illegal function: %s", name))
}

is.assignment <- function(x) {
  identical(x, as.symbol("<-")) || identical(x, as.symbol("="))
}

is.loop <- function(x) {
  identical(x, as.symbol("for")) || identical(x, as.symbol("while")) || identical(x, as.symbol("repeat"))
}

inWhiteList <- function(f) {
  # TODO: figure out if we can do this a little bit more efficiently. Perhaps in c++?
  #for (fun in whiteList)
  #  if (identical(f, fun))
  #    return(TRUE)
  #return(FALSE)

  #Maybe like:
  .isRFunctionInWhitelist(f)
}

# TODO: we need some way to fill this nicely... also we may want to prefixes the namespaces sometimes
whiteList <- list(
  list, `<-`, `{`, `[[`, print, `for`, `if`, `&&`, `||`, `c`, `+`, `-`, `/`, `*`, `:`, `>`, `body`, `function`, `c`, `local`
)

invalidCodeError <- function(message) {
  e <- structure(class = c('invalidCodeError', 'error', 'condition'), list(message=message, call=sys.call(-1)))
  stop(e)
}

# testing efficiency of lookups by pretending the whitelist consists of ~11000 functions
# aa <- as.list(asNamespace("base"))
# bb <- as.list(asNamespace("stats"))
#
# funs <- c(aa, bb)
# funs <- c(funs, funs, funs, funs, funs)
# foo <- function(f, lst) {
#   for (fun in lst)
#     if (identical(f, fun))
#       return(TRUE)
#   return(FALSE)
# }
#
#
# install.packages("bench")
# bch <- bench::mark(expr1 = foo(adist, funs))
# summary(bch, relative = TRUE)
# length(funs)
# bch
