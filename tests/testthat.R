suppressMessages(library(testthat))
suppressMessages(library(fbar))
suppressMessages(library(tidyverse))
suppressMessages(library(ROI.plugin.ecos))

suppressMessages(walk(ROI::ROI_available_solvers(), requireNamespace))
  

test_check("fbar")
