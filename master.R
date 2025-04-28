# Set the working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Clear console
cat("\014")

source("occupations_IPUMS_download")
source("occupations.R")
source("cotton_IPUMS_download.R")
source("cotton_database.R")
source("cotton_normalization.R")
source("cotton_production.R")
source("figures.R")
source("elasticities.R")