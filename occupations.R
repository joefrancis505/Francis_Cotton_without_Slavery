# Set the working directory to the script's location
setwd(tryCatch(getSrcDirectory(function(dummy) {dummy}), error = function(e) "."))
cat("\014") # Clear console

# Load required libraries - prefer dplyr for most operations but keep data.table for fast loading
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(data.table)

# --- Configuration ---
# Define file paths
census_file <- "Data/census.csv"
tenancy_file <- "Data/tenancy.csv"
county_region_file <- "Data/regions.csv"

# Define cotton belt regions (ID = Name mapping)
cotton_belt_regions <- c(
  "1" = "Piedmont",
  "2" = "Coastal Plain",
  "4" = "River Bottom",
  "5" = "Eastern Hilly",
  "6" = "Western Hilly",
  "7" = "Western Prairie"
)
cotton_belt_region_ids <- as.numeric(names(cotton_belt_regions))

# Set seed for reproducible random assignment
set.seed(505)

# Centralize column definitions
cols_census_needed <- c("SERIAL", "STATEICP", "COUNTYICP", "OCC1950", "RACE", "HISPAN", "BPL", "RELATE")
cols_tenancy_required <- c("STATEICP", "COUNTYICP", "AYJ001", "AYJ002", "AYJ003", "AYJ004", "AYJ005", 
                           "AYJ006", "AYJ007", "AYJ008", "AYJ009", "AYJ010", "AYJ011", "AYJ012")
cols_to_sum <- c("white", "white_hispanic", "white_nonhispanic", "black", "other", 
                 "native_white", "immigrant_white", "total")
output_cols <- list(
  raw = c("section", "occupation", "white", "white_hispanic", "white_nonhispanic", 
          "black", "other", "native_white", "immigrant_white", "total"),
  group_pct = c("section", "occupation", "white", "white_hispanic", "white_nonhispanic", 
                "black", "other", "native_white", "immigrant_white"),
  total_pct = c("section", "occupation", "white", "white_hispanic", "white_nonhispanic", 
                "black", "other", "native_white", "immigrant_white", "total")
)

# --- Data Loading ---
cat("Loading data...\n")

# Load county region data
county_region_data <- fread(county_region_file, data.table = FALSE)

# Filter to only include counties in cotton belt regions
cotton_belt_counties <- county_region_data %>%
  filter(region %in% cotton_belt_region_ids) %>%
  select(ICPSRST, ICPSRCTY)

# Load tenancy data
farm_tenancy_data <- fread(tenancy_file, data.table = FALSE)

# Create a lookup for faster filtering
county_lookup <- paste(cotton_belt_counties$ICPSRST, cotton_belt_counties$ICPSRCTY, sep = "_")

# Load and filter census data
cat("Reading and filtering census data...\n")
census_dt <- fread(
  census_file, 
  select = cols_census_needed,
  showProgress = TRUE,
  nThread = parallel::detectCores() - 1
)

# Convert to data frame and filter to only cotton belt counties
census_data <- as.data.frame(census_dt)
rm(census_dt) # Free memory
census_data$county_key <- paste(census_data$STATEICP, census_data$COUNTYICP, sep = "_")
census_data <- census_data %>%
  filter(county_key %in% county_lookup) %>%
  select(-county_key)
gc() # Free memory

# Prepare county region data and join with census data
county_regions_to_join <- county_region_data %>%
  select(ICPSRST, ICPSRCTY, region)

census_with_region <- census_data %>%
  left_join(
    county_regions_to_join,
    by = c("STATEICP" = "ICPSRST", "COUNTYICP" = "ICPSRCTY")
  )

cat("Data loaded and pre-filtered by region.\n")

# --- Helper Functions ---
# Get head of household occupations
get_head_occupations <- function(data) {
  data %>%
    filter(RELATE == 1) %>%
    select(SERIAL, head_occ = OCC1950)
}

# Process farm ratios for specific regions
process_farm_ratios <- function(farm_data, county_region_map, target_region_ids) {
  # Check for required columns
  if (!all(cols_tenancy_required %in% names(farm_data))) {
    stop("Missing required columns in farm tenancy data", call. = FALSE)
  }
  
  region_specific_farm_data <- farm_data %>%
    left_join(
      county_region_map,
      by = c("STATEICP" = "ICPSRST", "COUNTYICP" = "ICPSRCTY")
    ) %>%
    filter(region %in% target_region_ids)
  
  if (nrow(region_specific_farm_data) == 0) {
    warning(paste("No farm tenancy data found for specified region(s). Ratios NA."), call. = FALSE)
    return(data.frame(
      white_owners = 0, white_tenants = 0, black_owners = 0, black_tenants = 0,
      white_owner_ratio = NA_real_, white_tenant_ratio = NA_real_,
      black_owner_ratio = NA_real_, black_tenant_ratio = NA_real_
    ))
  }
  
  summary_ratios <- region_specific_farm_data %>%
    summarise(
      white_owners = sum(AYJ001 + AYJ002 + AYJ003, na.rm = TRUE),
      white_tenants = sum(AYJ004 + AYJ005 + AYJ006, na.rm = TRUE),
      black_owners = sum(AYJ007 + AYJ008 + AYJ009, na.rm = TRUE),
      black_tenants = sum(AYJ010 + AYJ011 + AYJ012, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      white_total_farmers = white_owners + white_tenants,
      white_owner_ratio = ifelse(white_total_farmers > 0, white_owners / white_total_farmers, NA_real_),
      white_tenant_ratio = ifelse(white_total_farmers > 0, white_tenants / white_total_farmers, NA_real_),
      black_total_farmers = black_owners + black_tenants,
      black_owner_ratio = ifelse(black_total_farmers > 0, black_owners / black_total_farmers, NA_real_),
      black_tenant_ratio = ifelse(black_total_farmers > 0, black_tenants / black_total_farmers, NA_real_)
    )
  
  return(summary_ratios)
}

# Process agricultural occupations
process_agricultural <- function(data, farm_ratios) {
  ag_data <- data %>%
    mutate(
      occupation = case_when(
        OCC1950 == 100 ~ "Farmers",
        OCC1950 == 123 ~ "Farm tenants",
        OCC1950 == 810 ~ "Farm foremen",
        OCC1950 %in% c(820, 830) & !is.na(head_occ) & head_occ %in% c(100, 123) ~ "Farm laborers (family)",
        OCC1950 %in% c(820, 830) ~ "Farm laborers (other)",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(occupation))
  
  farmer_data <- ag_data %>%
    filter(occupation == "Farmers") %>%
    mutate(
      occupation = case_when(
        RACE == 1 ~ if_else(runif(n()) <= farm_ratios$white_owner_ratio, "Farm owners", "Farm tenants", missing = "Farm tenants"),
        RACE == 2 ~ if_else(runif(n()) <= farm_ratios$black_owner_ratio, "Farm owners", "Farm tenants", missing = "Farm tenants"),
        TRUE ~ "Farm owners"
      )
    )
  
  bind_rows(
    farmer_data,
    ag_data %>% filter(occupation != "Farmers")
  )
}

# Process non-agricultural occupations
process_nonagricultural <- function(data) {
  data %>%
    mutate(
      occupation = case_when(
        OCC1950 >= 0 & OCC1950 <= 99 ~ "Professional and technical",
        OCC1950 >= 200 & OCC1950 <= 290 ~ "Managers, officials, and proprietors",
        OCC1950 >= 300 & OCC1950 <= 390 ~ "Clerical and kindred",
        OCC1950 >= 400 & OCC1950 <= 490 ~ "Sales workers",
        OCC1950 >= 500 & OCC1950 <= 595 ~ "Craftsmen",
        OCC1950 >= 600 & OCC1950 <= 690 ~ "Operatives",
        OCC1950 >= 700 & OCC1950 <= 720 ~ "Household servants",
        OCC1950 >= 730 & OCC1950 <= 790 ~ "Other service workers",
        OCC1950 >= 910 & OCC1950 <= 970 ~ "Laborers",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(occupation))
}

# Format table helper
format_table_for_text <- function(df, title) {
  table_text <- capture.output(
    knitr::kable(df, format = "pipe", caption = title, align = c('l', 'l', rep('r', ncol(df) - 2)))
  )
  return(c(table_text, ""))
}

# Consolidated table creation function
create_tables <- function(ag_data, nonag_data, region_name, analysis_type) {
  # Define occupation ordering
  farm_order <- c("Farm owners" = 1, "Farm tenants" = 2, "Farm foremen" = 3, 
                  "Farm laborers (family)" = 4, "Farm laborers (other)" = 5)
  nonfarm_order <- c("Professional and technical" = 6, "Managers, officials, and proprietors" = 7, 
                     "Clerical and kindred" = 8, "Sales workers" = 9, "Craftsmen" = 10, 
                     "Operatives" = 11, "Household servants" = 12, "Other service workers" = 13, 
                     "Laborers" = 14)
  
  # Combine data
  combined_data <- bind_rows(
    mutate(ag_data, section = "Farm", order = farm_order[occupation]),
    mutate(nonag_data, section = "Non-farm", order = nonfarm_order[occupation])
  ) %>%
    filter(!is.na(order)) %>%
    arrange(order)
  
  if(nrow(combined_data) == 0) {
    warning(paste("No data for tables for region:", region_name), call. = FALSE)
    no_data_text <- paste("No data found for", region_name, "(", analysis_type, ")")
    return(list(
      raw = NULL, 
      text_outputs = list(
        raw = c(paste(no_data_text, "- Raw Counts"), ""),
        group = c(paste(no_data_text, "- Group Percentages"), ""),
        total = c(paste(no_data_text, "- Total Percentages"), "")
      ),
      region = region_name,
      type = analysis_type
    ))
  }
  
  # Calculate raw counts
  raw_counts <- combined_data %>%
    group_by(section, occupation, order) %>%
    summarise(
      white = sum(RACE == 1, na.rm = TRUE),
      white_hispanic = sum(RACE == 1 & HISPAN > 0, na.rm = TRUE),
      white_nonhispanic = sum(RACE == 1 & HISPAN == 0, na.rm = TRUE),
      black = sum(RACE == 2, na.rm = TRUE),
      other = sum(RACE > 2, na.rm = TRUE),
      native_white = sum(RACE == 1 & BPL < 100, na.rm = TRUE),
      immigrant_white = sum(RACE == 1 & BPL >= 100, na.rm = TRUE),
      total = n(),
      .groups = "drop"
    ) %>%
    arrange(order) %>%
    select(-order)
  
  # Calculate subtotals
  subtotals <- raw_counts %>%
    group_by(section) %>%
    summarise(
      occupation = "Subtotal",
      across(all_of(cols_to_sum), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate total
  overall_total <- raw_counts %>%
    summarise(
      section = "Total",
      occupation = "Total",
      across(all_of(cols_to_sum), sum, na.rm = TRUE)
    )
  
  # Combine results
  final_raw_counts <- bind_rows(
    raw_counts %>% filter(section == "Farm"),
    subtotals %>% filter(section == "Farm"),
    raw_counts %>% filter(section == "Non-farm"),
    subtotals %>% filter(section == "Non-farm"),
    overall_total
  )
  
  all_text_output <- list()
  
  # 1. Raw Counts Table
  raw_formatted <- final_raw_counts %>%
    mutate(across(where(is.numeric), ~format(., big.mark = ",", trim = TRUE)))
  raw_title <- paste0("Occupational Distribution - ", region_name, " (", analysis_type, ") - Raw Counts")
  all_text_output$raw <- format_table_for_text(
    raw_formatted %>% select(all_of(output_cols$raw)),
    raw_title
  )
  
  # 2. Within-Group Percentages Table
  totals_row <- final_raw_counts %>% filter(occupation == "Total")
  group_title <- paste0("Within-Group Percentages - ", region_name, " (", analysis_type, ")")
  
  if(nrow(totals_row) == 1 && totals_row$total > 0) {
    group_pct_cols <- cols_to_sum[cols_to_sum != "total"]
    
    group_pct_calculated <- final_raw_counts %>%
      mutate(across(all_of(group_pct_cols),
                    ~ ifelse(totals_row[[cur_column()]] > 0 & !is.na(.x),
                             (.x / totals_row[[cur_column()]]) * 100,
                             NA_real_))) %>%
      mutate(across(all_of(group_pct_cols),
                    ~ ifelse(occupation == "Total", 100.0, .x)))
    
    group_pct_formatted <- group_pct_calculated %>%
      mutate(across(all_of(group_pct_cols),
                    ~ifelse(is.na(.), "N/A", sprintf("%.1f%%", round(., 1))))) %>%
      mutate(total = format(total, big.mark = ",", trim = TRUE))
    
    all_text_output$group <- format_table_for_text(
      group_pct_formatted %>% select(all_of(output_cols$group_pct)),
      group_title
    )
  } else {
    all_text_output$group <- c(paste("!! Cannot calculate Group percentages for", region_name,
                                     "(", analysis_type, ") due to zero totals. !!"), "")
  }
  
  # 3. Total Percentages Table
  total_title <- paste0("Total Percentages - ", region_name, " (", analysis_type, ")")
  
  if(nrow(totals_row) == 1 && totals_row$total > 0) {
    total_pct_calculated <- final_raw_counts %>%
      mutate(across(all_of(cols_to_sum), ~ (.x / totals_row$total) * 100))
    
    total_pct_formatted <- total_pct_calculated %>%
      mutate(across(all_of(cols_to_sum), ~sprintf("%.1f%%", round(., 1))))
    
    all_text_output$total <- format_table_for_text(
      total_pct_formatted %>% select(all_of(output_cols$total_pct)),
      total_title
    )
  } else {
    all_text_output$total <- c(paste("!! Cannot calculate Total percentages for", region_name, 
                                     "(", analysis_type, ") due to zero totals. !!"), "")
  }
  
  return(list(
    raw = final_raw_counts,
    text_outputs = all_text_output,
    region = region_name,
    type = analysis_type
  ))
}

# Main analysis function
run_analysis_for_region <- function(filtered_census_data, farm_data, county_map, region_ids = NULL, 
                                    region_name = "All Cotton Belt", analysis_type) {
  cat(paste0("\n===== ANALYZING REGION: ", region_name, " (", analysis_type, ") =====\n"))
  
  # Filter to specific region if specified
  if (!is.null(region_ids)) {
    data_for_analysis <- filtered_census_data %>%
      filter(region %in% region_ids)
  } else {
    data_for_analysis <- filtered_census_data %>%
      filter(region %in% cotton_belt_region_ids)
  }
  
  if (nrow(data_for_analysis) == 0) {
    cat(paste0("  No data for region: ", region_name, ". Skipping.\n"))
    no_data_text <- paste("No data found for", region_name, "(", analysis_type, ")")
    return(list(
      raw = NULL, 
      text_outputs = list(
        raw = c(paste(no_data_text, "- Raw Counts"), ""),
        group = c(paste(no_data_text, "- Group Percentages"), ""),
        total = c(paste(no_data_text, "- Total Percentages"), "")
      ),
      region = region_name,
      type = analysis_type
    ))
  }
  
  cat(paste0("  Processing ", nrow(data_for_analysis), " individuals for region: ", region_name, "\n"))
  
  # Get farm ratios for the region
  current_region_ids <- if (!is.null(region_ids)) region_ids else cotton_belt_region_ids
  farm_ratios <- process_farm_ratios(farm_data, county_map, current_region_ids)
  
  # Process the data
  head_occs <- get_head_occupations(data_for_analysis)
  processed_data <- data_for_analysis %>%
    select(SERIAL, STATEICP, COUNTYICP, OCC1950, RACE, HISPAN, BPL, RELATE, region) %>%
    left_join(head_occs, by = "SERIAL")
  
  ag_results <- process_agricultural(processed_data, farm_ratios)
  nonag_results <- process_nonagricultural(processed_data)
  
  # Create tables
  results <- create_tables(ag_results, nonag_results, region_name, analysis_type)
  
  return(results)
}

# --- Execution ---
# Create output directory if needed
output_dir <- "Results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("Created 'Results' directory.\n")
}

analysis_types <- c("occupations", "heads")

for (current_analysis_type in analysis_types) {
  cat(paste("\n\n<<<<< STARTING ANALYSIS TYPE:", toupper(current_analysis_type), ">>>>>\n"))
  
  # Prepare data based on analysis type
  if (current_analysis_type == "occupations") {
    input_data <- census_with_region
    cat("Using data for ALL individuals.\n")
  } else {
    input_data <- census_with_region %>% filter(RELATE == 1)
    cat("Using data for HOUSEHOLD HEADS only.\n")
  }
  
  if (nrow(input_data) == 0) {
    warning(paste("No data available for analysis type:", current_analysis_type), call. = FALSE)
    next
  }
  
  # Initialize storage
  all_regions_raw_data_list <- list()
  all_regions_text_output_list <- list()
  
  # Run analysis for All Cotton Belt
  all_cotton_belt_results <- run_analysis_for_region(
    filtered_census_data = input_data,
    farm_data = farm_tenancy_data,
    county_map = county_regions_to_join,
    region_ids = NULL,
    region_name = "All Cotton Belt",
    analysis_type = current_analysis_type
  )
  
  if (!is.null(all_cotton_belt_results$raw)) {
    all_regions_raw_data_list[["All Cotton Belt"]] <- mutate(all_cotton_belt_results$raw, 
                                                             region = all_cotton_belt_results$region)
  }
  all_regions_text_output_list <- c(all_regions_text_output_list, list(all_cotton_belt_results$text_outputs))
  
  # Run analysis for each individual region
  for (region_id_chr in names(cotton_belt_regions)) {
    region_id_num <- as.numeric(region_id_chr)
    region_name_str <- cotton_belt_regions[[region_id_chr]]
    
    region_results <- run_analysis_for_region(
      filtered_census_data = input_data,
      farm_data = farm_tenancy_data,
      county_map = county_regions_to_join,
      region_ids = region_id_num,
      region_name = region_name_str,
      analysis_type = current_analysis_type
    )
    
    if (!is.null(region_results$raw)) {
      all_regions_raw_data_list[[region_name_str]] <- mutate(region_results$raw, 
                                                             region = region_results$region)
    }
    all_regions_text_output_list <- c(all_regions_text_output_list, list(region_results$text_outputs))
  }
  
  # Save CSV output
  if (length(all_regions_raw_data_list) > 0) {
    combined_raw_df <- bind_rows(all_regions_raw_data_list)
    
    # Define column order for CSV
    csv_col_order <- c("region", "section", "occupation", "white", "white_hispanic", "white_nonhispanic", 
                       "black", "other", "native_white", "immigrant_white", "total")
    
    # Ensure all columns exist before reordering
    existing_cols <- csv_col_order[csv_col_order %in% names(combined_raw_df)]
    other_cols <- names(combined_raw_df)[!names(combined_raw_df) %in% existing_cols]
    combined_raw_df <- combined_raw_df %>% select(all_of(existing_cols), all_of(other_cols))
    
    csv_filename <- file.path(output_dir, paste0(current_analysis_type, ".csv"))
    write.csv(combined_raw_df, csv_filename, row.names = FALSE, na = "")
    cat(paste0("\nCombined raw counts CSV saved to: ", csv_filename, "\n"))
  }
  
  # Save text output
  if (length(all_regions_text_output_list) > 0) {
    final_text_to_write <- c()
    first_region <- TRUE
    for (region_output in all_regions_text_output_list) {
      if (!first_region) {
        final_text_to_write <- c(final_text_to_write, "\n", paste(rep("=", 120), collapse=""), "\n")
      }
      final_text_to_write <- c(final_text_to_write, 
                               unlist(region_output$raw), 
                               unlist(region_output$group), 
                               unlist(region_output$total))
      first_region <- FALSE
    }
    
    txt_filename <- file.path(output_dir, paste0(current_analysis_type, ".txt"))
    writeLines(final_text_to_write, txt_filename)
    cat(paste0("Combined formatted text tables saved to: ", txt_filename, "\n"))
  }
  
  cat(paste("\n<<<<< FINISHED ANALYSIS TYPE:", toupper(current_analysis_type), ">>>>>\n"))
}

cat("\n\nAnalysis complete. Files saved to the 'output' directory.\n")