# Rewritten from an earlier version based on a function by John Fox,
# http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
numbers2words <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  ones <- c(
    "", "one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine"
  )
  teens <- c(
    "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen"
  )
  tens <- c(
    "", "", "twenty", "thirty", "forty", "fifty", "sixty",
    "seventy", "eighty", "ninety"
  )
  scales <- c("", "thousand", "million", "billion", "trillion")

  under_thousand <- function(n) {
    parts <- character()

    hundreds <- n %/% 100
    if (hundreds > 0) {
      parts <- c(parts, ones[hundreds + 1], "hundred")
      n <- n %% 100
    }

    if (n >= 20) {
      parts <- c(parts, tens[n %/% 10 + 1])
      n <- n %% 10
      if (n > 0) {
        parts <- c(parts, ones[n + 1])
      }
    } else if (n >= 10) {
      parts <- c(parts, teens[n - 9])
    } else if (n > 0) {
      parts <- c(parts, ones[n + 1])
    }

    paste(parts, collapse = " ")
  }

  convert_one <- function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    if (!is.finite(value)) {
      stop("x must contain only finite values or NA.")
    }

    value <- round(value)
    if (abs(value) >= 1e15) {
      stop("numbers2words supports absolute values below 1 quadrillion.")
    }
    if (value == 0) {
      return("zero")
    }
    if (value < 0) {
      return(paste("minus", convert_one(-value)))
    }

    pieces <- character()
    scale_i <- 1L

    while (value > 0) {
      chunk <- value %% 1000
      if (chunk > 0) {
        chunk_words <- under_thousand(chunk)
        scale_word <- scales[scale_i]
        if (nzchar(scale_word)) {
          chunk_words <- paste(chunk_words, scale_word)
        }
        pieces <- c(chunk_words, pieces)
      }
      value <- value %/% 1000
      scale_i <- scale_i + 1L
    }

    paste(pieces, collapse = " ")
  }

  vapply(x, convert_one, character(1), USE.NAMES = FALSE)
}
