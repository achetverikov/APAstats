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
  .GlobalEnv; none remain.

## Check results

* Windows 11, R 4.6.1, full R CMD check --as-cran including donttest examples:
  0 errors, 0 warnings, 2 notes. All 197 test assertions passed.
* GitHub Actions, Ubuntu, R 4.6.1, R CMD check --as-cran --no-manual including
  donttest examples: 0 errors, 0 warnings, 1 note.

The "New submission" note is expected because the package has not yet been
published on CRAN. The additional Windows note concerns a local MiKTeX
temporary file; both PDF and HTML manuals built successfully.
