args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: superb-parity.R <old-library> <new-library>")
}

old_lib <- normalizePath(args[[1]], mustWork = TRUE)
new_lib <- normalizePath(args[[2]], mustWork = TRUE)

make_cases <- function() {
  d1 <- expand.grid(
    id = factor(1:8),
    condition = factor(
      c("left response", "right/response"),
      levels = c("left response", "right/response")
    ),
    trial = 1:3,
    KEEP.OUT.ATTRS = FALSE
  )
  id1 <- as.numeric(d1$id)
  cond1 <- as.numeric(d1$condition)
  d1$y <- 0.7 * id1 + 1.3 * cond1 +
    (0.11 + 0.015 * id1) * d1$trial^2 +
    0.03 * id1 * cond1

  d2 <- expand.grid(
    id = factor(1:9),
    cue = factor(c("pre cue", "post/cue")),
    target = factor(c("red", "blue", "green")),
    trial = 1:2,
    KEEP.OUT.ATTRS = FALSE
  )
  id2 <- as.numeric(d2$id)
  cue2 <- as.numeric(d2$cue)
  target2 <- as.numeric(d2$target)
  d2$y <- id2 * 0.4 + cue2 * 1.1 + target2 * 0.6 +
    d2$trial * (0.2 + id2 * 0.01) +
    cue2 * target2 * 0.08

  d3 <- expand.grid(
    id = factor(1:12),
    moment = factor(c("early", "middle", "late")),
    trial = 1:3,
    KEEP.OUT.ATTRS = FALSE
  )
  id3 <- as.numeric(d3$id)
  d3$group <- factor(ifelse(id3 <= 6, "control group", "test/group"))
  moment3 <- as.numeric(d3$moment)
  d3$y <- id3 * 0.35 + moment3 * 0.9 +
    d3$trial * (0.25 + id3 * 0.02) +
    (d3$group == "test/group") * 1.4

  sparse_design <- data.frame(
    phase = c("P 1", "P 2", "P 2"),
    side = c("left", "left", "right/side"),
    stringsAsFactors = FALSE
  )
  d4 <- do.call(rbind, lapply(1:8, function(id) {
    tmp <- sparse_design[rep(seq_len(nrow(sparse_design)), each = 3), ]
    tmp$id <- factor(id, levels = 1:8)
    tmp$trial <- rep(1:3, times = nrow(sparse_design))
    tmp
  }))
  d4$id <- factor(as.character(d4$id), levels = as.character(1:8))
  d4$phase <- factor(d4$phase, levels = c("P 1", "P 2"))
  d4$side <- factor(d4$side, levels = c("left", "right/side"))
  id4 <- as.numeric(d4$id)
  d4$y <- id4 * 0.5 + as.numeric(d4$phase) * 0.8 +
    as.numeric(d4$side) * 0.4 +
    d4$trial * (0.15 + id4 * 0.012)

  d5 <- d1[
    !(d1$id == levels(d1$id)[1] &
      d1$condition == "right/response"),
  ]

  list(
    mean_cm_ci = list(
      data = d1, wid = "id", within = "condition", value_var = "y",
      aggr_fun = "mean", errorbar = "CI",
      adjustments = list(purpose = "single", decorrelation = "CM")
    ),
    median_cm_ci = list(
      data = d1, wid = "id", within = "condition", value_var = "y",
      aggr_fun = "median", errorbar = "CI",
      adjustments = list(purpose = "single", decorrelation = "CM")
    ),
    sd_cm_difference_se = list(
      data = d1, wid = "id", within = "condition", value_var = "y",
      aggr_fun = "sd", errorbar = "SE",
      adjustments = list(purpose = "difference", decorrelation = "CM")
    ),
    var_none_ci = list(
      data = d1, wid = "id", within = "condition", value_var = "y",
      aggr_fun = "var", errorbar = "CI",
      adjustments = list(purpose = "single", decorrelation = "none")
    ),
    two_within_mean = list(
      data = d2, wid = "id", within = c("cue", "target"), value_var = "y",
      aggr_fun = "mean", errorbar = "CI",
      adjustments = list(purpose = "difference", decorrelation = "CM")
    ),
    mixed_median = list(
      data = d3, wid = "id", within = "moment", between = "group",
      value_var = "y", aggr_fun = "median", errorbar = "SE",
      adjustments = list(purpose = "single", decorrelation = "CM")
    ),
    sparse_design_mean = list(
      data = d4, wid = "id", within = c("phase", "side"), value_var = "y",
      aggr_fun = "mean", errorbar = "CI",
      adjustments = list(purpose = "single", decorrelation = "CM")
    ),
    drop_incomplete = list(
      data = d5, wid = "id", within = "condition", value_var = "y",
      aggr_fun = "mean", errorbar = "CI", drop_NA_subj = TRUE,
      adjustments = list(purpose = "single", decorrelation = "CM")
    )
  )
}

cases_file <- tempfile(fileext = ".rds")
old_file <- tempfile(fileext = ".rds")
new_file <- tempfile(fileext = ".rds")
saveRDS(make_cases(), cases_file)

run_version <- function(lib, cases_file, out_file) {
  .libPaths(c(lib, .libPaths()))
  suppressPackageStartupMessages(library(apastats2))

  cases <- readRDS(cases_file)
  out <- lapply(cases, function(case) {
    aggr_name <- case$aggr_fun
    case$aggr_fun <- NULL

    res <- switch(
      aggr_name,
      mean = do.call(
        function(...) get_superb_ci(..., aggr_fun = mean),
        case
      ),
      median = do.call(
        function(...) get_superb_ci(..., aggr_fun = median),
        case
      ),
      sd = do.call(
        function(...) get_superb_ci(..., aggr_fun = sd),
        case
      ),
      var = do.call(
        function(...) get_superb_ci(..., aggr_fun = var),
        case
      ),
      stop("Unsupported parity statistic: ", aggr_name)
    )
    factor_cols <- intersect(
      c(case$within, case$between),
      names(res)
    )

    for (nm in factor_cols) {
      res[[nm]] <- as.character(res[[nm]])
    }

    keep <- c(
      factor_cols,
      "center", "lowerwidth", "upperwidth", "lower_ci", "upper_ci"
    )
    res <- res[, keep, drop = FALSE]
    if (length(factor_cols) > 0L) {
      res <- res[do.call(order, res[factor_cols]), , drop = FALSE]
    }
    rownames(res) <- NULL
    res
  })

  saveRDS(out, out_file)
  invisible(TRUE)
}

callr::r(
  run_version,
  args = list(old_lib, cases_file, old_file),
  show = TRUE
)
callr::r(
  run_version,
  args = list(new_lib, cases_file, new_file),
  show = TRUE
)

old <- readRDS(old_file)
new <- readRDS(new_file)

failures <- character()
for (nm in names(old)) {
  comparison <- all.equal(
    old[[nm]],
    new[[nm]],
    tolerance = 1e-10,
    check.attributes = FALSE
  )
  if (!isTRUE(comparison)) {
    failures <- c(
      failures,
      paste0(nm, ": ", paste(comparison, collapse = "; "))
    )
  } else {
    message("Parity OK: ", nm)
  }
}

if (length(failures) > 0L) {
  stop(
    "superb parity failures:\n",
    paste(failures, collapse = "\n")
  )
}

message("All superb parity cases passed.")
