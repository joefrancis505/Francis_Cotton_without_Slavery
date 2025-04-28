# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Configuration parameters
config <- list(
  years = seq(1840, 1900, by = 10),
  raster_resolution = 200,
  output_dir = "Data",
  raw_data_dir = "Data/Raw",
  normalized_data_dir = "Data/Normalized"
)

# Required packages - only those actually needed
packages <- c("sf", "terra", "dplyr", "purrr", "readr", "exactextractr", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages silently
invisible(lapply(packages, library, character.only = TRUE))

# Create necessary directories for output
create_directories <- function() {
  # Create main output directory if it doesn't exist
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
    cat("Created directory:", config$output_dir, "\n")
  }
  
  # Create subdirectories for intermediary files
  for (dir in c(config$raw_data_dir, config$normalized_data_dir)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    }
  }
}

# Function to fix geometries in shapefiles
fix_geometries <- function(sf_object) {
  sf_object %>%
    st_make_valid() %>%
    st_buffer(0)
}

# Function to join shapefile with census data
join_data <- function(sf_object, census_data) {
  sf_object %>%
    left_join(census_data, by = "GISJOIN")
}

# Function to calculate densities
calculate_densities <- function(sf_object) {
  sf_object %>%
    mutate(across(c(census_pop, enslaved, improved, unimproved, cotton), 
                  ~ . / SHAPE_AREA, 
                  .names = "{.col}_density"))
}

# Function to find shapefile for a specific year
find_shapefile <- function(year) {
  year_str <- as.character(year)
  shapefile_dir <- paste0("Data/Shapefiles/", year_str)
  
  if (!dir.exists(shapefile_dir)) {
    stop(paste("Shapefile directory not found:", shapefile_dir))
  }
  
  shp_files <- list.files(shapefile_dir, pattern = "\\.shp$", 
                          full.names = TRUE, recursive = FALSE)
  
  if (length(shp_files) == 0) {
    stop(paste("No shapefile found in directory:", shapefile_dir))
  }
  
  return(shp_files[1])
}

# Rasterize_data function
rasterize_data <- function(sf_object, resolution = config$raster_resolution) {
  bbox <- st_bbox(sf_object)
  crs_proj4 <- st_crs(sf_object)$proj4string
  
  # Create raster template
  raster_template <- rast(xmin=bbox["xmin"], xmax=bbox["xmax"], 
                          ymin=bbox["ymin"], ymax=bbox["ymax"], 
                          resolution=resolution)
  
  # Set CRS for the raster template using proj4string
  crs(raster_template) <- crs_proj4
  
  # Define the variables to rasterize based on what's available
  potential_variables <- c("census_pop_density", "cotton_density", 
                           "enslaved_density", "improved_density", "unimproved_density")
  variables <- potential_variables[potential_variables %in% names(sf_object)]
  
  # Rasterize each variable
  raster_list <- lapply(variables, function(variable) {
    sf_object[[variable]] <- as.numeric(sf_object[[variable]])
    r <- rasterize(vect(sf_object), raster_template, field = variable, fun = "sum")
    return(r)
  })
  
  if (length(raster_list) > 0) {
    raster_stack <- do.call(c, raster_list)
    names(raster_stack) <- variables
  } else {
    stop("No variables were successfully rasterized.")
  }
  
  return(raster_stack)
}

# Zonal_stats function
zonal_stats <- function(raster_stack, zones_sf) {
  stats <- exact_extract(raster_stack, zones_sf, fun = "mean", 
                         append_cols = c("GISJOIN", "STATENAM"), progress = FALSE)
  return(stats)
}

# Simplified convert_to_absolute function
convert_to_absolute <- function(zonal_stats_df, zones_sf) {
  # Get column names that start with "mean."
  mean_cols <- names(zonal_stats_df)[grepl("^mean\\.", names(zonal_stats_df))]
  
  # Join data
  result <- zones_sf %>%
    left_join(zonal_stats_df, by = c("GISJOIN", "STATENAM"))
  
  # For each mean column, calculate the absolute value
  for (mean_col in mean_cols) {
    var_name <- gsub("^mean\\.(.+)_density$", "\\1", mean_col)
    result <- result %>%
      mutate(!!var_name := !!sym(mean_col) * SHAPE_AREA)
  }
  
  # Standardize state column handling
  if ("STATENAM" %in% names(result) && !"state" %in% names(result)) {
    result <- result %>% rename(state = STATENAM)
  } else if ("state" %in% names(result) && "STATENAM" %in% names(result)) {
    result <- result %>% select(-STATENAM)
  }
  
  # Remove the mean columns
  result <- result %>% select(-all_of(mean_cols))
  
  return(result)
}

# Load all data needed throughout the script
load_data <- function() {
  data_list <- list()
  
  cat("Reading cotton database from Data/cotton_database.csv\n")
  data_list$cotton_data <- read_csv("Data/cotton_database.csv", show_col_types = FALSE)
  
  cat("Reading regions data from Data/regions.csv\n")
  data_list$regions_data <- read_csv("Data/regions.csv", show_col_types = FALSE)
  
  cat("Reading 1900 county boundaries\n")
  shapefile_path_1900 <- find_shapefile("1900")
  data_list$zones_1900 <- st_read(shapefile_path_1900, quiet = TRUE) %>%
    fix_geometries()
  
  return(data_list)
}

# Process raw data for each year - consolidated logic
process_raw_data <- function(data_list) {
  cat("\n=== Processing Raw Data ===\n")
  
  year_strings <- as.character(config$years)
  
  # Process all years
  for (year_str in year_strings) {
    cat(sprintf("\nProcessing raw data for year: %s\n", year_str))
    
    # First check if we need to create the raw data file
    raw_file <- file.path(config$raw_data_dir, paste0(year_str, "_raw.csv"))
    
    if (!file.exists(raw_file)) {
      cat(sprintf("Raw data file %s not found. Creating from data...\n", raw_file))
      
      # Find shapefile in directory
      shapefile_path <- find_shapefile(year_str)
      
      # Read shapefile
      counties_sf <- tryCatch({
        sf_obj <- st_read(shapefile_path, quiet = TRUE)
        sf_obj <- fix_geometries(sf_obj)
        
        sf_obj %>%
          mutate(GISJOIN = as.character(GISJOIN))
      }, error = function(e) {
        warning(sprintf("Error reading shapefile for year %s: %s", year_str, e$message))
        return(NULL)
      })
      
      if (is.null(counties_sf)) {
        next
      }
      
      # Filter cotton data for this year
      year_cotton_data <- data_list$cotton_data %>% 
        filter(year == as.integer(year_str))
      
      # Join everything at once - keep all columns from cotton data
      counties_df <- counties_sf %>%
        st_drop_geometry() %>%
        select(GISJOIN) %>% # Keep only GISJOIN from shapefile
        left_join(year_cotton_data, by = "GISJOIN") %>%
        left_join(data_list$regions_data %>% select(GISJOIN, region), by = "GISJOIN")
      
      # Handle special cases for certain years after joining
      if (as.integer(year_str) > 1860 && !"enslaved" %in% names(counties_df)) {
        counties_df$enslaved <- 0
      }
      
      # Save data for this year
      write_csv(counties_df, raw_file)
      cat("Raw data for year", year_str, "has been saved to", raw_file, "\n")
    } else {
      cat(sprintf("Raw data file %s already exists. Using existing file.\n", raw_file))
    }
  }
  
  # Create compiled database
  compile_cotton_study_database(year_strings)
}

# Compile cotton study database from raw files
compile_cotton_study_database <- function(years) {
  cat("\n=== Compiling Cotton Study Database ===\n")
  
  year_dataframes <- list()
  
  # Read raw data for each year
  for (yr in years) {
    raw_file <- file.path(config$raw_data_dir, paste0(yr, "_raw.csv"))
    if (file.exists(raw_file)) {
      cat(sprintf("Reading raw data for year %s...\n", yr))
      year_dataframes[[yr]] <- read_csv(raw_file, show_col_types = FALSE)
    } else {
      warning(sprintf("Raw data file %s not found. Skipping.", raw_file))
    }
  }
  
  # Check if any years were processed
  if (length(year_dataframes) == 0) {
    stop("Failed to find any raw data files for Cotton Study database.")
  }
  
  # Combine all years
  cat("Combining all processed years...\n")
  all_counties <- bind_rows(year_dataframes)
  
  # Write the final database
  output_file <- file.path(config$output_dir, "Cotton_study_database.csv")
  write_csv(all_counties, output_file)
  
  cat(sprintf("Cotton Study Database created with %d rows covering %d years.\n", 
              nrow(all_counties), length(unique(all_counties$year))))
  
  return(all_counties)
}

# Process year function
process_year <- function(year, cotton_data, zones_1900, regions_data) {
  cat(sprintf("\nProcessing year: %s\n", year))
  
  if (year != "1900") {
    # Find shapefile
    shapefile_path <- find_shapefile(year)
    
    # Read county shapefile
    counties <- st_read(shapefile_path, quiet = TRUE) %>%
      fix_geometries()
    
    # Join with cotton data and process
    counties_with_data <- join_data(counties, cotton_data)
    counties_filtered <- counties_with_data %>% filter(census_pop > 0)
    counties_densities <- calculate_densities(counties_filtered)
    
    # Rasterize and get zonal statistics
    raster_data <- rasterize_data(counties_densities)
    zonal_results <- zonal_stats(raster_data, zones_1900)
    
    # Convert to absolute values
    final_results <- convert_to_absolute(zonal_results, zones_1900)
  } else {
    # Process 1900 data without rasterization
    final_results <- zones_1900 %>%
      left_join(cotton_data, by = "GISJOIN")
    
    # Ensure 'state' column is correctly named
    if ("STATENAM" %in% names(final_results) && !"state" %in% names(final_results)) {
      final_results <- final_results %>% rename(state = STATENAM)
    }
  }
  
  # Add region information and select required columns
  final_results <- final_results %>%
    left_join(regions_data %>% select(GISJOIN, region), by = "GISJOIN") %>%
    select(GISJOIN, state, region, census_pop, enslaved, improved, unimproved, cotton) %>%
    st_drop_geometry()
  
  return(final_results)
}

# Perform processing for all years - more efficient
perform_processing <- function(data_list) {
  cat("\n=== Performing Processing for All Years ===\n")
  
  # Process all years including 1900
  for (year in config$years) {
    year_str <- as.character(year)
    output_file <- file.path(config$normalized_data_dir, paste0(year, "_normalized.csv"))
    
    # Skip if output already exists
    if (file.exists(output_file)) {
      cat(sprintf("Output file for year %s already exists. Skipping.\n", year))
      next
    }
    
    result <- process_year(year_str, 
                           filter(data_list$cotton_data, year == !!year), 
                           data_list$zones_1900,
                           data_list$regions_data)
    
    # Save result for this year
    write_csv(result, output_file)
    cat(sprintf("Finished processing year %s\n", year))
    
    # Clean up to save memory
    rm(result)
    gc(verbose = FALSE)
  }
}

# Compile panel data
compile_panel_data <- function() {
  cat("\n=== Compiling Panel Data ===\n")
  
  panel_data <- map_dfr(config$years, function(year) {
    cat(sprintf("Processing year %d...\n", year))
    file_name <- file.path(config$normalized_data_dir, paste0(year, "_normalized.csv"))
    
    data <- read_csv(file_name, show_col_types = FALSE)
    data <- data %>% mutate(year = year)
    
    return(data)
  })
  
  # Ensure consistent column order
  all_columns <- c("GISJOIN", "state", "region", "year", "census_pop", 
                   "enslaved", "improved", "unimproved", "cotton")
  
  panel_data <- panel_data %>%
    select(all_of(intersect(all_columns, names(panel_data))))
  
  # Write panel data to CSV
  output_file <- file.path(config$output_dir, "Cotton_panel_data.csv")
  write_csv(panel_data, output_file)
  
  cat(sprintf("Panel data compiled successfully. Output file: %s\n", output_file))
  return(panel_data)
}

# Main function
main <- function() {
  cat("Starting the cotton database normalization processing...\n")
  
  # Create all necessary directories
  create_directories()
  
  # Load all data once
  data_list <- load_data()
  
  # Make sure we have the Cotton_study_database.csv
  db_file <- file.path(config$output_dir, "Cotton_study_database.csv")
  if (!file.exists(db_file)) {
    cat("Cotton study database not found. Creating raw data and database...\n")
    process_raw_data(data_list)
    
    # Update cotton data from newly created database
    data_list$cotton_data <- read_csv(db_file, show_col_types = FALSE)
  }
  
  # Perform processing
  perform_processing(data_list)
  
  # Compile panel data
  compile_panel_data()
  
  # Free memory
  rm(data_list)
  gc(verbose = FALSE)
  
  cat("All operations completed successfully.\n")
}

# Run the main function
main()