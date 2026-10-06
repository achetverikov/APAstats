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
