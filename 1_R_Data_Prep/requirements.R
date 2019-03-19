# Requires: R package
# Modifies: Nothing
# Effects: Installs package 
install_pkgs <- function(pkg) { 
  
  # Install package if it isn't already
  if (!(pkg %in% installed.packages()[, "Package"])){ 
    
    install.packages(pkg, repos='http://cran.us.r-project.org')
  }
  
  library(pkg, character.only = TRUE)
  
  
} # end install_pkgs()

# load library packages
pkg_list <- c("data.table", "dplyr","lubridate", "tidyr","stringr","purrr", "chunked")

lapply(pkg_list, install_pkgs)


########### Set folder paths here #############
# input and output folder paths
input_folder = ""
output_folder = ""
