setwd(tryCatch(getSrcDirectory(function(dummy) {dummy}), error = function(e) "."))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("dplyr", "purrr", "readr", "ipumsr", "stringr")

options(timeout = 3600)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# --- IPUMS API Setup ---

# Function to get API key from user or environment
get_api_key <- function() {
  api_key_env <- Sys.getenv("IPUMS_API_KEY")
  
  if (nzchar(api_key_env)) {
    cat("Using IPUMS API key found in environment variable IPUMS_API_KEY.\n")
    api_key <- api_key_env
  } else {
    ipums_url <- "https://account.ipums.org/api_keys"
    cat("This script requires an IPUMS API key, which can be obtained at", ipums_url, "\n")
    api_key <- readline(prompt = "Please enter your API key: ")
    
    # Set the key in the current session environment
    if (nzchar(api_key)) {
      Sys.setenv(IPUMS_API_KEY = api_key)
      cat("IPUMS API key has been set for this session.\n")
    }
  }
  
  if (!nzchar(api_key)) {
    stop("API key is required to download IPUMS data. Exiting.")
  }
  cat("IPUMS API key accepted for this session.\n")
  
  return(api_key)
}

# Create necessary directories
required_dirs <- c("Data")

# Create each directory if it doesn't exist
for (dir_path in required_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Get API key
api_key <- get_api_key()

# --- Download NHGIS Data ---

cat("\n=== Downloading NHGIS Data ===\n")

# Set the default IPUMS collection for NHGIS
set_ipums_default_collection("nhgis")

# Define NHGIS extract for 1900 Agriculture Data, Table NT7
nhgis_extract <- define_extract_nhgis(
  description = "1900 Census Agriculture Data - Race of Farmer by Detailed Management",
  datasets = ds_spec(
    "1900_cAg",
    data_tables = "NT7",
    geog_levels = "county"
  )
)

# Submit and wait for extract
cat("Submitting NHGIS extract request...\n")
nhgis_submitted <- submit_extract(nhgis_extract, api_key = api_key)

cat("Waiting for NHGIS extract to complete...\n")
nhgis_ready <- wait_for_extract(nhgis_submitted, api_key = api_key)

# Download extract
cat("Downloading NHGIS extract files...\n")
nhgis_files <- download_extract(
  nhgis_ready, 
  download_dir = "Data", 
  api_key = api_key, 
  overwrite = FALSE
)

# --- Download IPUMS USA Data ---

cat("\n=== Downloading IPUMS USA Data ===\n")

# Set the default collection to USA
set_ipums_default_collection("usa")

# Define USA extract for 1900 Census data
# Using var_spec to set case selections where possible
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "1900 Census Microdata for Cotton Belt Analysis",
  samples = "us1900m",
  variables = list("STATEICP", "OCC1950", "COUNTYICP", "RACE", "HISPAN", "BPL", "RELATE", "SERIAL"),
  data_format = "fixed_width"
)

# Submit and wait for extract
cat("Submitting USA extract request...\n")
usa_submitted <- submit_extract(usa_extract, api_key = api_key)

cat("Waiting for USA extract to complete...\n")
usa_ready <- wait_for_extract(usa_submitted, api_key = api_key)

# Download extract
cat("Downloading USA extract files...\n")
usa_files <- download_extract(
  usa_ready, 
  download_dir = "Data", 
  api_key = api_key, 
  overwrite = FALSE
)

# --- Processing Downloaded Data ---

cat("\n=== Processing Downloaded Data ===\n")

# Load NHGIS data
cat("Loading NHGIS data...\n")
nhgis_data <- read_nhgis(nhgis_files["data"], verbose = FALSE)

# Write to tenancy.csv
cat("Writing NHGIS data to tenancy.csv...\n")
write_csv(nhgis_data, "Data/tenancy.csv")

# Load USA data from DDI file
cat("Loading USA data...\n")
usa_ddi <- read_ipums_ddi(usa_files)
usa_data <- read_ipums_micro(usa_ddi, verbose = FALSE)

# Write to census.csv
cat("Writing USA data to census.csv...\n")
write_csv(usa_data, "Data/census.csv")

cat("\n=== Data Processing Complete ===\n")
cat("The following files have been created:\n")
cat("1. Data/tenancy.csv\n")
cat("2. Data/census.csv\n")