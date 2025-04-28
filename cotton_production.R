# Cotton Production by Region: 1840-1900
# This script recreates Table 4 showing the percentage distribution
# of cotton production across the main cotton belt regions

# Create output directory if it doesn't exist
dir.create("Figures", showWarnings = FALSE)

# ----- Load or create the raw data -----
# Input the data from Table 4
cotton_data <- data.frame(
  year = c(1840, 1850, 1860, 1870, 1880, 1890, 1900),
  coastal_plain = c(42, 38, 38, 36, 30, 30, 27),
  piedmont = c(24, 25, 13, 14, 18, 18, 15),
  eastern_hills = c(13, 23, 17, 15, 15, 9, 10),
  river_bottom = c(16, 9, 16, 15, 13, 13, 11),
  western_hills = c(4, 4, 12, 15, 16, 15, 17),
  western_prairie = c(1, 1, 3, 5, 7, 11, 17)
)

# Calculate "Other" category (difference to make 100%)
cotton_data$row_sum <- rowSums(cotton_data[, 2:7])
cotton_data$other <- 100 - cotton_data$row_sum

# ----- Generate Table -----
# Format the data for printing
create_cotton_table <- function() {
  # Create text output file
  sink("Results/cotton_table.txt")
  
  # Print Table header
  cat("\n")
  cat("                       Table 4\n")
  cat("       Cotton Production by Region, 1840-1900\n\n")
  
  # Column headers
  cat(sprintf("     %-9s %-9s %-9s %-9s %-9s %-9s %-9s\n", 
              "Coastal", "Piedmont", "Eastern", "River", "Western", "Western", "Other"))
  cat(sprintf("     %-9s %-9s %-9s %-9s %-9s %-9s %-9s\n", 
              "plain", "", "hills", "bottom", "hills", "prairie", ""))
  
  # Table rows
  for (i in 1:nrow(cotton_data)) {
    cat(sprintf("%d   %-9d %-9d %-9d %-9d %-9d %-9d %-9d\n", 
                cotton_data$year[i],
                cotton_data$coastal_plain[i],
                cotton_data$piedmont[i],
                cotton_data$eastern_hills[i],
                cotton_data$river_bottom[i],
                cotton_data$western_hills[i],
                cotton_data$western_prairie[i],
                cotton_data$other[i]))
  }
  
  # Table footer
  cat("\n")
  cat("Note: The table shows the percentage distribution of the physical quantity of\n")
  cat("cotton produced, as reported to the census. The totals may not reach 100 because\n")
  cat("some production occurred outside the six main cotton belt regions.\n")
  
  # Close the text file
  sink()
  
  cat("Table saved to 'Results/cotton_table.txt'\n")
}

# ----- Run the functions -----
create_cotton_table()

cat("All outputs completed successfully.\n")