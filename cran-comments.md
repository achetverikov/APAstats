# Submission

This is a new submission, version 1.0.5. The package succeeds the GitHub-only
`apastats` package and has no reverse dependencies.

## R CMD check results

Checked with `R CMD check --as-cran` on the built tarball:

* Local Windows 11, R 4.6.1: 0 errors | 0 warnings | 2 notes
* win-builder (devel and release): 0 errors | 0 warnings | 1 note

The only note from win-builder is "New submission". The second local note is a
`lastMiKTeXException` file left in the temp directory by the local MiKTeX
installation, not by the package.
