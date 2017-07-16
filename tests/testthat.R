suppressMessages(library(testthat))
suppressMessages(library(fbar))
suppressMessages(library(tidyverse))
suppressMessages(library(ROI.plugin.ecos))

suppressMessages(walk(intersect(ROI::ROI_registered_solvers(), ROI::ROI_installed_solvers()), requireNamespace))
  

test_check("fbar")
