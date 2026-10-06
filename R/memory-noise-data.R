#' Memory bias under relative-noise manipulations
#'
#' A compact participant-level dataset derived from Experiments 1A and 1B of
#' Chetverikov and Hansmann-Roth (2026). In Exp. 1A the high-noise stimulus had
#' SD = 20 degrees; in Exp. 1B it had SD = 45 degrees. Low-noise stimuli had
#' SD = 5 degrees in both samples.
#'
#' Only unequal-noise trials are included. Bias is estimated with the same
#' weighted probability-density asymmetry approach used in the paper and
#' averaged over target/non-target dissimilarities from 1 to 44 degrees, the
#' range in which Experiments 1A and 1B showed the relative-noise interaction.
#' Positive values indicate attraction toward the competing non-target and
#' negative values indicate repulsion.
#'
#' \itemize{
#'   \item participant. An anonymized participant identifier, unique across
#'     Experiments 1A and 1B.
#'   \item experiment. Between-subject sample: \code{"Exp. 1A"} or
#'     \code{"Exp. 1B"}.
#'   \item high_noise_sd. High stimulus-noise standard deviation used in the
#'     sample (20 or 45 degrees).
#'   \item relative_noise. Ordered within-subject factor:
#'     \code{"target less noisy"} or \code{"target more noisy"}.
#'   \item bias_percent. Density-asymmetry bias in percentage points. Positive
#'     values indicate attraction toward the non-target; negative values
#'     indicate repulsion.
#'   \item n_trials. Number of non-outlier unequal-noise trials contributing
#'     to the weighted bias curve.
#' }
#'
#' @docType data
#' @usage data(memory_noise)
#' @name memory_noise
#' @format A data.frame with 70 observations and 6 variables.
#' @keywords datasets
#' @references Chetverikov, A., & Hansmann-Roth, S. (2026). Noise in
#'   Competing Representations Determines the Direction of Memory Biases.
#'   eLife, 15, RP111380. \doi{10.7554/eLife.111380.1}
#' @source Derived from the public behavioral dataset at
#'   \url{https://osf.io/kqb8t/}. The package subset is based on the
#'   canonical CHR2026 representation used for model comparison.
#'
NULL
