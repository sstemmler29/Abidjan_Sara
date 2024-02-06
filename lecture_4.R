

## Installing packages in in R 
install.packages('readr')
library(readr)


## install more than 1 package
install.packages(c('readr', 'ggplot2', 'tidyr'))
library(readr)
library(ggplot2)
library(tidyr)


# Or check if installed/ install, and load 

packages = c('readr', 'ggplot2', 'tidyr')


check_install_load_package <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
