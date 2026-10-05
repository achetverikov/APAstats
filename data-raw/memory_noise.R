# Rebuild memory_noise from the public Chetverikov & Hansmann-Roth data.
# Source project: https://osf.io/kqb8t/
# Analysis-ready file used by the CHR2026 canonical adapter:
# https://osf.io/download/qwm5h/
#
# Requires circhelp and data.table for the density-asymmetry calculation.

raw <- data.table::fread("https://osf.io/download/qwm5h/")

keep <- raw$expName %in% c("color_1", "color_hv_1") &
  raw$noise %in% c("low - high", "high - low") &
  raw$is_outlier == 0 &
  !is.na(raw$bias_to_distr_corr) &
  !is.na(raw$abs_td_dist)

dat <- raw[keep]

bias_curve <- circhelp::density_asymmetry(
  dat,
  circ_space = 360,
  weights_sd = 10,
  xvar = "abs_td_dist",
  yvar = "bias_to_distr_corr",
  by = c("subject_exp", "expName", "noise")
)

bias_summary <- bias_curve[
  dist >= 1 & dist <= 44,
  .(bias_percent = 100 * mean(delta)),
  by = .(subject_exp, expName, noise)
]

counts <- dat[, .(n_trials = .N), by = .(subject_exp, expName, noise)]
memory_noise <- merge(
  bias_summary, counts,
  by = c("subject_exp", "expName", "noise"),
  sort = FALSE
)

subject_number <- as.integer(
  sub("^S([0-9]+)\\..*$", "\\1", memory_noise$subject_exp)
)
memory_noise[, participant := ifelse(
  expName == "color_1",
  sprintf("E1_S%i", subject_number),
  sprintf("E1_HV_S%i", subject_number)
)]
memory_noise[, experiment := ifelse(
  expName == "color_1", "Exp. 1", "Exp. 1 HV"
)]
memory_noise[, high_noise_sd := ifelse(expName == "color_1", 20, 45)]
memory_noise[, relative_noise := ifelse(
  noise == "low - high", "target less noisy", "target more noisy"
)]

memory_noise$participant <- factor(memory_noise$participant)
memory_noise$experiment <- factor(
  memory_noise$experiment,
  levels = c("Exp. 1", "Exp. 1 HV")
)
memory_noise$relative_noise <- factor(
  memory_noise$relative_noise,
  levels = c("target less noisy", "target more noisy"),
  ordered = TRUE
)

memory_noise <- as.data.frame(memory_noise[
  order(experiment, participant, relative_noise),
  .(participant, experiment, high_noise_sd, relative_noise,
    bias_percent, n_trials)
])
row.names(memory_noise) <- NULL

dir.create("data", showWarnings = FALSE)
con <- file("data/memory_noise.R", open = "wt")
cat("# Generated from data-raw/memory_noise.R\nmemory_noise <- ", file = con)
dput(memory_noise, con)
close(con)
