# apastats2 1.0.3

* Added direct `apa.glm` support for `lmerTest::lmer` model objects via `lmerModLmerTest` dispatch.
* Fixed `apa.glm` mixed-model class checks so `lmerTest` fits use the correct coefficient, df, and p-value handling.
* Documented that omitting `term` returns a formatted summary table for all coefficients and added a `lmerTest::lmer` example.

# apastats2 1.0.2

* apastats2 is set as the main branch
* Added a package vignette (`apastats2-intro`) and updated README documentation.
* Migrated legacy dot-named utility functions to snake_case canonical names.
* Kept legacy dot-named utility APIs as deprecated wrappers for backward compatibility.
* Replaced deprecated `ggplot2::aes_string()` usage with tidy-eval mapping.
* Updated package metadata/build ignores for vignettes and Git-related files.

# apastats 0.5

* The first and the last release before switching to apastats2
