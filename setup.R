#
#   Setup needed R packages
#
#   This scripts provides two way to setup/install the R packages needed for
#   replication. 
#   
#   The first and preferred one is to use the **checkpoint** package. This 
#   will install a snapshot of the R packages as they existed on CRAN around
#   the time we last ran the results in the paper. To do this run the two 
#   lines below or source this script.
#
#   The second way is to manually install the 26 or so needed packages. 
#   They are listed below in a section that you can manually run.
#

if (!"checkpoint" %in% rownames(installed.packages())) {
  install.packages("checkpoint")
}

library("checkpoint")

# installs packages using this CRAN snapshopt to a local directory
checkpoint("2019-08-15")


# Check for and if needed set up output directory structure
outdirs <- list(
  "output", 
  "output/figures",
  "output/figures-robustness",
  "output/models",
  "output/tables"
)
need <- !sapply(outdirs, dir.exists)
if (any(need)) {
  input <- readline("One or more output directories do not exist, can I create them? (y/n)?: ")
  if (input=="y") {
    sapply(outdirs[need], dir.create)
  } else {
    stop(paste0("missing output directories, please create them:\n", paste0(outdirs[need], collapse = "\n")))
  }
}


# Manual package install
# not run
manual_install <- function() {
  
  packages <- c("broom", "caret", "cowplot", "doMC", "dplyr", "forcats",
                "futile.logger", "ggplot2", "ggrepel", "ggstance", "hrbrthemes",
                "kableExtra", "knitr", "lme4", "nlme", "readr", "recipes",
                "scoringRules", "stargazer", "states", "stringr", "tibble",
                "tidyr", "tidyverse", "xgboost")
  need <- !packages %in% rownames(installed.packages())
  if (any(need)) {
    sapply(packages[need], install.packages)
  }
}

