# Load required libraries
library(readODS)
library(dplyr)
library(lmtest)
library(sandwich)

# Set working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# --- Settings ---
data_file <- "Data/data.ods" 
data_sheet <- "data"
results_dir <- "Results"
results_file <- file.path(results_dir, "elasticities.txt")

# --- Data Loading and Processing (Silent) ---

# Load data - Wrap in suppressMessages/suppressWarnings if libraries are noisy
cotton_data_raw <- read_ods(data_file, sheet = data_sheet)

# Process data: Filter (now starting from 1820), Clean, Log, Lag, Trend
cotton_data_processed <- cotton_data_raw %>%
  # *** MODIFICATION: Start from 1820 to calculate lags for 1821 ***
  filter(year >= 1820 & year <= 1860) %>%
  # Arrange by year is crucial for correct lagging
  arrange(year) %>% 
  # Clean, Log, Lag, Trend in one mutate step
  mutate(
    # Clean numeric columns
    across(c(us_m, us_p, india_m, india_p), 
           ~ as.numeric(gsub(",", "", as.character(.x))),
           .names = "{.col}"),
    
    # Create log transformations (handle non-positive values)
    log_us_m = dplyr::if_else(us_m > 0, base::log(us_m), NA_real_), 
    log_us_p = dplyr::if_else(us_p > 0, base::log(us_p), NA_real_),
    log_india_m = dplyr::if_else(india_m > 0, base::log(india_m), NA_real_),
    log_india_p = dplyr::if_else(india_p > 0, base::log(india_p), NA_real_),
    
    # Create lag variables (now using dplyr::lag)
    log_us_m_lag1 = dplyr::lag(log_us_m, 1),
    log_us_p_lag1 = dplyr::lag(log_us_p, 1),
    log_india_m_lag1 = dplyr::lag(log_india_m, 1),
    log_india_p_lag1 = dplyr::lag(log_india_p, 1),
    
    # Create time trend (based on row number *after* filtering)
    # Note: trend will start from 1 for year 1820
    time_trend = row_number() 
  ) %>% 
  # Keep only necessary columns for analysis (optional, but cleaner)
  select(year, starts_with("log_"), time_trend)

# --- Analysis Function (prints to console/sink) ---
analyze_elasticity <- function(data, qty_var, lag_qty_var, lag_price_var, label) {
  # Note: Price var itself isn't used in model, only its lag
  required_cols = c(qty_var, lag_qty_var, lag_price_var, "time_trend")
  if (!all(required_cols %in% names(data))) {
    stop("Internal Error: Missing required columns for model '", label, "': ", 
         paste(required_cols[!required_cols %in% names(data)], collapse=", "))
  }
  
  # Drop rows with NA values (this will remove 1820 due to its lags being NA)
  model_data <- data %>%
    select(all_of(required_cols)) %>%
    na.omit()
  
  formula_str <- paste(qty_var, "~", lag_qty_var, "+", lag_price_var, "+ time_trend")
  cat("\n--- Analyzing Model for", label, "---\n") # This WILL be printed/saved
  cat("Formula:", formula_str, "\n")               # This WILL be printed/saved
  
  # Check if data remains after NA removal
  if (nrow(model_data) == 0) {
    cat("ERROR: No complete observations available for this model after removing NAs.\n")
    cat(" Regression cannot be run for:", label, "\n")
    return(list(label = label, elasticity = NA, se = NA, pval = NA, n = 0))
  }
  # Sample size N should now be 40 (1821-1860)
  cat("Sample: N =", nrow(model_data), "\n")       # This WILL be printed/saved
  
  # Run regression
  model <- lm(as.formula(formula_str), data = model_data)
  
  # Calculate Newey-West standard errors
  nw_test <- coeftest(model, vcov = NeweyWest(model, lag = NULL, prewhite = FALSE)) 
  cat("\n--- Coefficients with Newey-West (HAC) Standard Errors ---\n") # This WILL be printed/saved
  print(nw_test)                                                        # This WILL be printed/saved
  
  # Extract elasticity (coefficient on lagged price)
  if (!lag_price_var %in% rownames(nw_test)){
    cat("ERROR: Coefficient for", lag_price_var, "not found in model results.\n")
    return(list(label = label, elasticity = NA, se = NA, pval = NA, n = nrow(model_data)))
  }
  elasticity <- nw_test[lag_price_var, "Estimate"]
  se <- nw_test[lag_price_var, "Std. Error"]
  pval <- nw_test[lag_price_var, "Pr(>|t|)"]
  
  cat("\n--- Price Elasticity (Coefficient on", lag_price_var, ") ---\n") # This WILL be printed/saved
  cat("Estimate:", round(elasticity, 3), "\n")                             # This WILL be printed/saved
  cat("Std. Error:", round(se, 3), "\n")                                  # This WILL be printed/saved
  cat("p-value:", round(pval, 3), "\n")                                    # This WILL be printed/saved
  
  return(list(label = label, elasticity = elasticity, se = se, pval = pval, n = nrow(model_data)))
}

# --- Prepare for Output ---
# Create Results directory if it doesn't exist
if (!dir.exists(results_dir)) {
  cat("Creating directory:", results_dir, "\n") # Print this message to console only
  dir.create(results_dir)
}

# *** MODIFICATION: Start sinking output to file AND console ***
# All subsequent cat() and print() calls will go to both destinations
sink(results_file, split = TRUE) 

# --- Run Analyses ---
cat("--- Running Analyses ---\n") # This WILL be printed/saved

# Run analysis for US cotton
# Pass the processed data frame
us_results <- analyze_elasticity(
  data = cotton_data_processed, 
  qty_var = "log_us_m", 
  lag_qty_var = "log_us_m_lag1", 
  lag_price_var = "log_us_p_lag1", 
  label = "UK Imports (US Cotton)"
)

# Run analysis for Indian cotton
# Pass the processed data frame
india_results <- analyze_elasticity(
  data = cotton_data_processed, 
  qty_var = "log_india_m", 
  lag_qty_var = "log_india_m_lag1", 
  lag_price_var = "log_india_p_lag1", 
  label = "UK Imports (Indian Cotton)"
)

# --- Print Summary Table ---
cat("\n======================================================\n") # This WILL be printed/saved
cat("SUMMARY OF ELASTICITY ESTIMATES (from lag price coeff)\n") # This WILL be printed/saved
cat("Analysis includes years 1821-1860 (N=40)\n")             # Clarify sample years
cat("======================================================\n") # This WILL be printed/saved
cat(sprintf("%-25s %12s %12s %12s %5s\n", "Series", "Elasticity", "Std. Error", "p-value", "N")) # This WILL be printed/saved
cat(strrep("-", 68), "\n") # Separator line                       # This WILL be printed/saved

print_results_row <- function(results) { # This helper function is defined locally
  if (!is.null(results) && !is.na(results$elasticity)) {
    cat(sprintf("%-25s %12.3f %12.3f %12.3f %5d\n", 
                results$label, results$elasticity, results$se, results$pval, results$n))
  } else if (!is.null(results)) {
    cat(sprintf("%-25s %12s %12s %12s %5d\n", results$label, "NA", "NA", "NA", results$n))
  } else {
    cat(sprintf("%-25s %12s %12s %12s %5s\n", "Unknown Series", "ERROR", "ERROR", "ERROR", "N/A"))
  }
}

print_results_row(us_results)
print_results_row(india_results)
cat(strrep("-", 68), "\n")

# *** MODIFICATION: Stop sinking output ***
sink() 

cat("\nAnalysis complete. Results saved to:", results_file, "\n") # Print final message to console only