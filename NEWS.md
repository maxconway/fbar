# fbar 0.5.2.9000

- Fix for new CRAN check for 'return' without '()'
- Lots of changes to take into account changes in dependencies
- Simplified installation documentation

# fbar 0.5.2

- Fix to getBiGG function
- Fix to vignette

# fbar 0.4.4

- New function for retrieving models from BiGG
- Changes to model data files
- New data file to help specify exchange reactions
- Changed some function argument defaults

# fbar 0.3.4

- Test updates and tidying to work with new version of ROI
- New tests
- Spelling corrections
- Moving to assuming availability of ecos solver. All other solvers still supported

# fbar 0.3.3

- Spelling corrections

# fbar 0.3.2

- Minor updates for Cran release

# fbar 0.3.1

- Created website using pkgdown

# fbar 0.3.0

- Moved to new NSE framework

# fbar 0.2.2

- Deprecated `expanded_to_gurobi`, `expanded_to_glpk` and `reactiontbl_to_gurobi`. ROI will now be the only backend.

# fbar 0.2.1

- Updated and corrected some test code
- Added test and fix for situation where metabolite is used multiple times in one reaction string
- Small updates to vignettes and readme

# fbar 0.2.0

- Added new functions for metabolite parsing
- Added more documentation

# fbar 0.1.23

- Added DONTRUN to some examples which take too long on some platforms

# fbar 0.1.19

- New vignette

# fbar 0.1.18

- variety of bug fixes

# fbar 0.1.17

- Started moving to using ROI.plugin.ecos for examples and tests, since it has no dependencies
- New function to convert from an expanded model back to a reaction table
- Fixed a bug in the total flux minimization code of find_fluxes_df

# fbar 0.1.16

- Changes to tests to reduce chatter further

# fbar 0.1.15

- Description changes to remove notes

# fbar 0.1.14

- Changed the title
- Changed the description
- Typo fixed

# fbar 0.1.13

- Checks and filters in tests, vignette and examples to remove implicit dependency on ROI.plugin.glpk.
- Other pdates and improvements to documentation and examples.
- Updated tests for cleaner output

# fbar 0.1.12

- Tweaks and updates for CRAN submission

# fbar 0.1.9

- Started using travis for integration

# fbar 0.1.8

- Test updates

# fbar 0.1.1

- Changed optimization framework - can now use one of a choice of optimizers
- Lots more documentation
- Name changes and refactoring


