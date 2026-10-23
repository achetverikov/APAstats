# Resubmission after review

This is a resubmission of apastats2 1.1.0 following CRAN review.

## Changes addressing the review

* Removed single quotes around the APA acronym in the Title and Description.
* Replaced T/F logical abbreviations with TRUE/FALSE, including the
  plot_pointrange defaults and generated usage documentation.
* Added return-value documentation to apastats-deprecated.Rd and reviewed
  exported functions for output class, structure, and meaning.
* Unwrapped short executable examples and replaced the remaining dontrun
  wrapper with donttest. Replaced the slow BayesFactor example with a small
  t-test. All examples, including donttest examples, passed.
* Audited package code for global assignments, including <<- and writes to
  .GlobalEnv; none remain. get_adjusted_ci() also no longer looks up
  precision functions by name in the calling or global environment; custom
  precision functions must be passed explicitly as arguments.

## Check results

* Windows 11, R 4.6.1, full R CMD check --as-cran including donttest examples:
  0 errors, 0 warnings, 2 notes. All 199 test assertions passed.
* GitHub Actions, Ubuntu, R 4.6.1, R CMD check --as-cran --no-manual including
  donttest examples: 0 errors, 0 warnings, 1 note.

The "New submission" note is expected because the package has not yet been
published on CRAN. The additional Windows note reports the elapsed time of the
apa.brmsfit example, which is wrapped in donttest because it compiles a Stan
model (about 4 s CPU time, most of the elapsed time is compilation).
