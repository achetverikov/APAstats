# apastats2 1.1.0

* Added correlation-adjusted intervals to `get_adjusted_ci()` with `adjustments = list(decorrelation = "CA")`. Correlations are computed separately within each between-subject group after cell aggregation and incomplete-subject removal. Cousineau-Morey (`"CM"`) remains the default.
* `get_adjusted_ci()` no longer looks up `SE.<name>`/`CI.<name>` precision functions by name; for summary functions without a built-in formula, pass `errorbar` as a function.
* Compacted interval-precision selection, ANOVA and regression formatting, and confidence-interval text formatting while preserving existing output conventions.
* Simplified `plot_pointrange()` to use the same interval columns for between- and within-subject summaries, including margin summaries.
* Made incomplete-cell diagnostics part of the warning or error so that suppressing a warning also suppresses its diagnostic table.
* Replaced deprecated `brms::posterior_samples()` with `brms::as_draws_matrix()`, retaining exact coefficient selection and optional transformations.
* Fixed `format_results(type = "plotmath")` returning `NULL` when the input contains no comma.
* Addressed CRAN review: removed quotes around APA in the package metadata, replaced `T`/`F` logical abbreviations, documented return classes and meanings (including deprecated functions), added package links, and corrected HTML rendering of column-name placeholders.
* Made examples executable or marked long examples with `\donttest{}`, replaced the slow BayesFactor example with a small t-test, and added regression coverage for interval calculations, formatting, and plot summaries.

# apastats2 1.0.6

* `get_adjusted_ci()` now supports correlation-adjusted (`decorrelation = "CA"`) intervals for within-subject and mixed designs, using the mean pairwise condition correlation separately within each between-subject group. The default remains Cousineau-Morey (`"CM"`).
* CA supports the existing aggregation, confidence-level, precision-function, missing-subject, and difference-purpose options. Undefined correlations produce informative errors. The implementation has no runtime dependency on `superb`; regression tests compare against it when available.

# apastats2 1.0.5

* Removed the internal `summarySE()`, `summarySEwithin()` and `normDataWithin()` helpers; `plot_pointrange()` now computes margins with `get_adjusted_ci()`, which also fixes `add_margin = TRUE` with `within_subj = TRUE`.
* `get_adjusted_ci()` no longer warns that the `drop` argument is ignored when reporting incomplete cells.
* `base_breaks()` no longer prints the computed breaks.
* Fixed typos in the documentation, added a spelling word list, and made the `apa.BFBayesFactor()` example faster.
* Changed the maintainer e-mail address.

# apastats2 1.0.4
* Added the `memory_noise` example dataset derived from Chetverikov & Hansmann-Roth (2026), using unequal-noise trials and density-asymmetry bias scores; Exp. 1A vs. Exp. 1B is the between-subject factor and target relative noise is the within-subject factor.
* Removed the legacy `faces` example dataset; maintained examples now use `memory_noise` or built-in datasets.
* Added `get_adjusted_ci()` as the general interval engine for between-subject, within-subject, and mixed designs; `plot_pointrange()` now uses it for both between- and within-subject summaries.
* Reimplemented Cousineau-Morey intervals internally, removing the `superb`/`reshape2` dependency and the development-branch `Remotes` entry while documenting the implementation's basis in the `superb` framework.
* `get_adjusted_ci()` supports arbitrary scalar summary functions, with superb-compatible SE/CI formulas for common statistics and custom precision-function hooks. The old `get_superb_ci()` name remains as a deprecated compatibility wrapper.
* Added `apa()` support for `stats::aov()` models with `Error()` terms (`aovlist` objects), fixing #9.
* Added `apa()` support for `afex::aov_ez`, `afex::aov_car`, and `afex::aov_4` results via `afex_aov` dispatch.
* Added deprecated `describe.afex()` compatibility wrapper.

# apastats2 1.0.3

* Added direct `apa.glm` support for `lmerTest::lmer` model objects via `lmerModLmerTest` dispatch.
* Added direct `apa.glm` support for `lme4::glmer` model objects via `glmerMod` dispatch.
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
