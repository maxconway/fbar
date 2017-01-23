suppressMessages(library(testthat))
suppressMessages(library(fbar))
suppressMessages(library(tidyverse))

suppressMessages(walk(ROI::ROI_available_solvers(), requireNamespace))
  

test_check("fbar")
