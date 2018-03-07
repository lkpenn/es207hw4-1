#turns a vector `c("a", "b", "c")` into the string `a, b, and c`

vec2str <- function(x, sep = ", ", last = ", and ") {
  if (length(x) > 1) {
    str_c(str_c(x[-length(x)], collapse = sep),
          x[length(x)],
          sep = last)
  } else {
    x
  }
}