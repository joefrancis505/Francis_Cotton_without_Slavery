# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load required packages
packages <- c("sf", "dplyr", "purrr", "readr", "ipumsr", "stringr")
new_packages <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# Create logger with custom verbosity level
log_level <- 1  # 0 = errors only, 1 = normal, 2 = verbose
log <- function(msg, level = 1, newline = TRUE) {
  if (level <= log_level) {
    cat(msg, if(newline) "\n" else "")
  }
}

# --- Directory Setup ---
required_dirs <- c("Data/IPUMS_Raw", "Data/Shapefiles")
invisible(sapply(required_dirs, function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}))

# --- IPUMS API Setup ---
get_api_key <- function() {
  api_key_env <- Sys.getenv("IPUMS_API_KEY")
  
  if (nzchar(api_key_env)) {
    log("Using IPUMS API key from environment variable.")
    return(api_key_env)
  } 
  
  log("This script requires an IPUMS API key from https://account.ipums.org/api_keys")
  api_key <- readline(prompt = "Please enter your API key: ")
  
  if (nzchar(api_key)) {
    Sys.setenv(IPUMS_API_KEY = api_key)
    log("IPUMS API key set for this session.")
  } else {
    stop("API key is required to download NHGIS data.")
  }
  
  return(api_key)
}

# Set default collection and get API key
set_ipums_default_collection("nhgis")
api_key <- get_api_key()

# --- Data Specs Configuration ---
# Define years and data directory
years <- c("1840", "1850", "1860", "1870", "1880", "1890", "1900")
download_dir <- "Data/IPUMS_Raw"

# Data specs lookup table
data_specs_config <- list(
  "1840" = list(
    pop = list(dataset = "1840_cPopX", tables = c("NT4", "NT5", "NT6")),
    ag = list(dataset = "1840_cAg", tables = c("NT2"))
  ),
  "1850" = list(
    pop = list(dataset = "1850_cPAX", tables = c("NT1", "NT2", "NT6")),
    ag = list(dataset = "1850_cAg", tables = c("NT2", "NT3", "NT6"))
  ),
  "1860" = list(
    pop = list(dataset = "1860_cPAX", tables = c("NT1", "NT6")),
    ag = list(dataset = "1860_cAg", tables = c("NT1", "NT2", "NT5"))
  ),
  "1870" = list(
    pop = list(dataset = "1870_cPAX", tables = c("NT2", "NT4")),
    ag = list(dataset = "1870_cAg", tables = c("NT1", "NT2", "NT3", "NT9"))
  ),
  "1880" = list(
    pop = list(dataset = "1880_cPAX", tables = c("NT2", "NT4")),
    ag = list(dataset = "1880_cAg", tables = c("NT1", "NT9", "NT11", "NT15A"))
  ),
  "1890" = list(
    pop = list(dataset = "1890_cPHAM", tables = c("NT1", "NT2", "NT6")),
    ag = list(dataset = "1890_cAg", tables = c("NT1", "NT8", "NT9A", "NT21"))
  ),
  "1900" = list(
    pop = list(dataset = "1900_cPHAM", tables = c("NT1", "NT2", "NT7")),
    ag = list(dataset = "1900_cAg", tables = c("NT1", "NT10", "NT11", "NT13", "NT39"))
  )
)

# --- Utility Functions ---
# Function to extract zip files efficiently
extract_zip <- function(zip_file, extract_dir) {
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
    log(sprintf("Extracting: %s", basename(zip_file)), level = 2)
    tryCatch(utils::unzip(zip_file, exdir = extract_dir),
             error = function(e) warning(sprintf("Failed to extract %s: %s", basename(zip_file), e$message)))
    return(TRUE)
  }
  return(FALSE)
}

# Function to recursively extract all zip files in a directory (improved)
extract_all_zips <- function(directory) {
  repeat {
    zip_files <- list.files(directory, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
    if (length(zip_files) == 0) break
    
    log(sprintf("Found %d zip files to extract", length(zip_files)), level = 2)
    extracted_any <- FALSE
    
    for (zip_file in zip_files) {
      extract_dir <- gsub("\\.zip$", "", zip_file)
      if (extract_zip(zip_file, extract_dir)) {
        extracted_any <- TRUE
      }
    }
    
    if (!extracted_any) break
  }
}

# Function to get data specifications for a year
get_data_specs <- function(year) {
  config <- data_specs_config[[year]]
  if (is.null(config)) stop(paste("No dataset definitions for year", year))
  
  pop_specs <- ds_spec(
    config$pop$dataset, 
    data_tables = config$pop$tables,
    geog_levels = "county"
  )
  
  ag_specs <- ds_spec(
    config$ag$dataset,
    data_tables = config$ag$tables,
    geog_levels = "county"
  )
  
  return(list(pop_specs, ag_specs))
}

# --- Main Functions ---
# Download data for a specific year
download_year_data <- function(year, api_key, download_dir) {
  year_dir <- file.path(download_dir, year)
  if (!dir.exists(year_dir)) dir.create(year_dir)
  
  log(sprintf("\n--- Downloading data for year: %s ---", year))
  
  # Get data specifications
  data_specs <- get_data_specs(year)
  shapefile_name <- paste0("us_county_", year, "_tl2000")
  
  # Define and submit extract
  combined_extract <- define_extract_nhgis(
    description = paste("Combined data for", year),
    datasets = data_specs,
    shapefiles = shapefile_name
  )
  
  log("Submitting extract request...")
  submitted_extract <- submit_extract(combined_extract, api_key = api_key)
  
  log("Waiting for extract to complete...")
  ready_extract <- wait_for_extract(submitted_extract, api_key = api_key)
  
  # Download extract
  log("Downloading extract files...")
  downloaded_files <- download_extract(
    ready_extract, 
    download_dir = year_dir, 
    api_key = api_key, 
    overwrite = FALSE
  )
  
  # Extract all zip files (single extraction process)
  log("Extracting all zip files...")
  extract_all_zips(year_dir)
  
  log(sprintf("Data for %s downloaded and extracted.", year))
  
  return(list(
    year = year,
    files = downloaded_files,
    dir = year_dir
  ))
}

# Organize shapefiles (improved)
organize_shapefiles <- function(year_data) {
  year <- year_data$year
  year_dir <- year_data$dir
  
  shapefile_dest_dir <- file.path("Data/Shapefiles", year)
  if (!dir.exists(shapefile_dest_dir)) dir.create(shapefile_dest_dir, recursive = TRUE)
  
  # Find shapefiles
  shp_files <- list.files(year_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  if (length(shp_files) == 0) {
    warning(paste("No shapefile found for year", year))
    return(FALSE)
  }
  
  # Process each shapefile
  for (shp_file in shp_files) {
    # Get all related files efficiently
    base_dir <- dirname(shp_file)
    base_name <- tools::file_path_sans_ext(basename(shp_file))
    related_pattern <- paste0("^", base_name, "\\.[a-zA-Z0-9]+$")
    
    # Get and copy related files
    related_files <- list.files(base_dir, pattern = related_pattern, full.names = TRUE)
    new_files <- related_files[!file.exists(file.path(shapefile_dest_dir, basename(related_files)))]
    
    if (length(new_files) > 0) {
      file.copy(new_files, shapefile_dest_dir)
      log(sprintf("Copied %d files for %s", length(new_files), base_name), level = 2)
    }
  }
  
  return(TRUE)
}

# Process CSV files (improved)
process_csv_files <- function(year_data) {
  year <- year_data$year
  year_dir <- year_data$dir
  
  # Find all CSV files (excluding codebooks)
  csv_files <- list.files(year_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  csv_files <- csv_files[!grepl("codebook", csv_files)]
  
  if (length(csv_files) == 0) {
    warning(paste("No CSV files found for year", year))
    return(NULL)
  }
  
  # Read all CSV files
  data_tables <- list()
  for (csv_path in csv_files) {
    ds_match <- str_match(basename(csv_path), "_ds(\\d+)_")
    if (length(ds_match) > 0 && !is.na(ds_match[1, 2])) { 
      ds_id <- paste0("ds", ds_match[1, 2])
      log(sprintf("Reading dataset %s...", ds_id), level = 2)
      
      data_tables[[ds_id]] <- tryCatch(
        read_csv(csv_path, show_col_types = FALSE) %>% 
          mutate(GISJOIN = as.character(GISJOIN)),
        error = function(e) {
          warning(sprintf("Error reading %s: %s", basename(csv_path), e$message))
          NULL
        }
      )
    }
  }
  
  # Filter out and combine tables
  data_tables <- data_tables[!sapply(data_tables, is.null)]
  if (length(data_tables) == 0) {
    warning(paste("Failed to read any valid data tables for year", year))
    return(NULL)
  }
  
  log(sprintf("Joining %d data tables for year %s...", length(data_tables), year))
  
  # Efficient join
  combined_data <- Reduce(function(x, y) {
    full_join(x, y %>% select(GISJOIN, where(is.numeric)), by = "GISJOIN")
  }, data_tables)
  
  # Add year column
  combined_data$year <- as.integer(year)
  
  return(combined_data)
}

# Main function
main <- function() {
  log("=== Starting IPUMS Data Downloader ===")
  
  # Download data for all years
  all_downloads <- setNames(lapply(years, function(year) {
    download_year_data(year, api_key, download_dir)
  }), years)
  
  # Organize shapefiles
  log("\n=== Organizing Shapefiles ===")
  lapply(all_downloads, organize_shapefiles)
  
  # Process CSV files
  log("\n=== Processing Census CSV Files ===")
  all_data <- lapply(all_downloads, process_csv_files)
  valid_data <- all_data[!sapply(all_data, is.null)]
  
  # Merge all census data
  log("\n=== Merging Census Data ===")
  merged_data <- bind_rows(valid_data)
  output_file <- "Data/census_NHGIS.csv"
  write_csv(merged_data, output_file)
  
  log(sprintf("All census data merged into %s", output_file))
  log(sprintf("Total rows: %d, Total columns: %d", nrow(merged_data), ncol(merged_data)))
  log(sprintf("Years included: %s", paste(sort(unique(merged_data$year)), collapse=", ")))
  
  log("\n=== Data Processing Complete ===")
}

# Run main function
main()