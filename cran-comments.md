## CRAN submission status

This is a new submission of `backgammonboard` 0.1.0.

## Test environments

Final release-candidate checks are pending after the CRAN cleanup commit.

Planned final checks:

- local source build and `R CMD check --as-cran`
- clean installation from the built source tarball
- check without `backgammoncalculator` installed to verify optional GNUID support is conditional
- check with released `backgammoncalculator` 0.1.0 installed
- Winbuilder using current R-devel

## Expected check note

`backgammoncalculator` is an optional package in `Suggests` and is not in a
mainstream R repository. It is used only for complete GNUID input. Ordinary
XGID parsing and rendering do not require it. Its source and installation
location are documented in `DESCRIPTION` and the README:

<https://github.com/backgammonsimplified/backgammoncalculator>

Tests that require `backgammoncalculator` use
`testthat::skip_if_not_installed("backgammoncalculator")`.

If the final CRAN check reports only the new-submission/non-mainstream-Suggests
NOTE, explain this optional dependency in the CRAN submission comment.
