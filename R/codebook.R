#' Get codebook information of the variable
#'
codebook <- function(x, ...) {
  UseMethod("codebook")
}

codebook.labelled <- function(x) {
  label <- attr(x, "label")
  type <- type_sum(x)
  cat(paste0("Label: ", label, " Type: ", type))
}

codebook.numeric <- function(x) {
  width <- tibble_width(NULL)

  padleft <- function(s) format(s, w = width / 3, justify = "right")
  # align <- function(s) format(c(s, "unique values: "), justify = "right")[1]

  type <- paste0(padleft("type: "), type_sum(x))
  range <- paste0(padleft("range: "), "[",
                  format(min(x, na.rm = TRUE), digits = 3), ", ",
                  format(max(x, na.rm = TRUE), digits = 3), "]")

  unique_values <- paste0(padleft("unique values: "), length(unique(x)))
  missing <- paste0("missing: ", paste0(sum(is.na(x)), "/", length(x)))

  mean <- paste0(padleft("mean: "), format(mean(x, na.rm = TRUE), digits = 3))
  stddev <- paste0(padleft("std. dev.: "), format(sd(x, na.rm = TRUE), digits = 3))

  percentiles1 <- paste0(padleft("percentiles: "),
                         "10%  ", "25%  ", "50%  ", "75%  ", "90%  ")
  percentiles2 <- c(padleft(" "),
                    paste0(format(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9)), digits = 3),
                    "  "))

  cat(codebook_header(x))
  cat(type, "\n", "\n", range, "\n", sep = "")
  cat(paste0(unique_values,
             format(missing, w = width / 3, justify = "right")), "\n", "\n")
  cat(mean, "\n", stddev, "\n", "\n", percentiles1, "\n", percentiles2, sep = "")
}

