# Resubmission after review

This is a resubmission after review, version 1.1.0. The package has not yet
been published on CRAN and succeeds the GitHub-only apastats package.

## Response to the review

* Removed single quotes around APA in the Title and Description. APA is an
  acronym, not a package or software name.
* Replaced T/F logical abbreviations with TRUE/FALSE in code and examples,
  including the plot_pointrange defaults and generated usage documentation.
* Added return documentation to apastats-deprecated.Rd and reviewed the exported
  function documentation for the output class, structure, and meaning.
* Unwrapped short executable examples and replaced the remaining dontrun example
  with donttest. Replaced the slow BayesFactor example with a small t-test.
  All examples, including donttest examples, passed the check below.
* Audited package code for global assignments, including <<- and writes to
  .GlobalEnv; none remain. Incomplete-cell diagnostics are now part of the
  warning/error instead of being printed separately.

Also corrected HTML rendering of column-name placeholders, replaced a deprecated
brms posterior-sample API, and fixed a missing plotmath formatting return value.

Added the CA correlation-adjustment option to get_adjusted_ci(), keeping CM as
the within-subject default. CA results matched superb 1.0.1 in 16 numerical
comparisons spanning mixed groups, four summary statistics, CI/SE, and
single/difference purposes.

## Full local validation of version 1.1.0 (2026-10-22)

Built the source tarball and checked it on Windows 11, R 4.6.1, using
R CMD check --as-cran --timings with _R_CHECK_DONTTEST_EXAMPLES_=true:

0 errors | 0 warnings | 2 notes

* New submission: the package has not yet had an accepted CRAN release.
* A lastMiKTeXException temporary file from the local MiKTeX installation.
  Both the PDF and HTML manuals built successfully.

All 197 test assertions passed, with no warnings or skips. Regular examples,
donttest examples, and vignette rebuilding passed. The brms example was
verified with a real fitted model. A temporary check library supplied
StanHeaders 2.32.10 compatible with rstan 2.32.7; the normal installed library
was not changed.

## GitHub validation

* The pkgdown build and deployment passed:
  https://github.com/achetverikov/APAstats/actions/runs/39790337381
* All eight comparisons against the former superb-backed implementation passed:
  https://github.com/achetverikov/APAstats/actions/runs/39790482637
* The Linux R-CMD-check workflow explicitly runs donttest examples and supplies
  the C++ headers needed to compile the Bayesian example. Its latest result:
  https://github.com/achetverikov/APAstats/actions/workflows/R-CMD-check.yaml

The GitHub workflow also matches StanHeaders to rstan 2.32.7 when that version
is installed. No dependency version is pinned in the package DESCRIPTION.
