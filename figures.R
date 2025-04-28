# Load required libraries
library(readODS)
library(dplyr)

# Set working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror (only needed if installing packages)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Create Figures directories
dir.create("Figures", showWarnings = FALSE)

# Function to set up plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1, right_margin = 1) {
  # Convert inches to points (1 inch = 72 points)
  width_pt <- width * 72
  height_pt <- height * 72

  # Set margins in inches c(bottom, left, top, right)
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  
  # Set other plot parameters
  par(family = "sans",
      cex = 1.3,      # Base text size
      cex.axis = 1.3, # Axis annotation size relative to cex
      cex.lab = 1.3,  # Axis label size relative to cex
      tck = 0.01,    # Tick mark length (<0 means outside, >0 means inside) - changed to match common style
      lwd = 0.5,      # Base line width for box, axes, ticks
      las = 1,        # Axis labels always horizontal
      mgp = c(2.8, 0.8, 0)) # Margin line for axis title, axis labels, axis line
}

# Figure 1: Labor Force Costs, 1800--1860
Figure_1 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    # Error handling for file reading
    tryCatch({
      data <- read_ods("Data/data.ods", sheet = "data")
    }, error = function(e) {
      stop("Error reading data.ods: ", e$message, call. = FALSE)
    })
    
    # Check if required columns exist
    required_cols <- c("year", "f_cost", "e_cost")
    if (!all(required_cols %in% names(data))) {
      stop("Missing required columns in data.ods sheet 'data': ", 
           paste(setdiff(required_cols, names(data)), collapse=", "), call. = FALSE)
    }
    
    plot(c(1800, 1860), c(0, 300),
         type = "n",
         xlab = " ",
         ylab = "$ per laborer",
         xlim = c(1800, 1860),
         ylim = c(0, 300),
         xaxs = "i", # Style 'internal': axes do not extend beyond data range
         yaxs = "i",
         axes = FALSE) # Turn off default axes
    
    # Custom axes
    axis(1, at = seq(1800, 1860, by = 10), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 300, by = 50), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = 0.4) # Use default labels=TRUE
    box(lwd = 0.5) # Draw plot border
    
    # Plot data lines
    lines(data$year, data$f_cost, col = "black", lwd = 2)
    lines(data$year, data$e_cost, col = "black", lwd = 1)
    
    # Add text labels
    text(1805, 85.3, "Captive", adj = c(0.5, 0.5), cex = 1.3)
    text(1810, 198, "Industrial", adj = c(0.5, 0.5), cex = 1.3)
  }
  
  # Console plot
  # plot_function() # Removed console plot for brevity when running the whole script
  
  # PDF plot
  pdf_file <- "Figures/labor.pdf"
  message("Creating plot: ", pdf_file)
  pdf(pdf_file, width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  message("Finished plot: ", pdf_file)
}

# Figure 2: Cotton Production and the Value of the Enslaved, 1800-1860
Figure_2 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    data <- read_ods("Data/data.ods", sheet = "data")
    
    # Check the range of cotton_pc to ensure we set appropriate limits
    cotton_max <- max(data$cotton_pc, na.rm = TRUE)
    cotton_ylim <- ceiling(cotton_max / 20) * 20  # Round up to nearest 20
    
    # Set up the primary y-axis (left) for enslaved_v
    plot(c(1800, 1860), c(0, 300),
         type = "n",
         xlab = " ",
         ylab = "$ per person",
         xlim = c(1800, 1860),
         ylim = c(0, 1000),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1800, 1860, by = 10), lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 1000, by = 200), lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    if(all(c("year", "enslaved_v", "cotton_pc") %in% names(data))) {
      # Plot enslaved_v on the left axis
      lines(data$year, data$enslaved_v, col = "black", lwd = 1)
      
      # Create and set up the secondary y-axis (right) for cotton_pc
      par(new = TRUE)
      plot(data$year, data$cotton_pc,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "",
           xlim = c(1800, 1860),
           ylim = c(0, 80),
           xaxs = "i",
           yaxs = "i")
      
      # Add right axis with the same formatting as the left axis
      # Using las=1 to keep the numbers horizontal
      axis(4, at = seq(0, 80, by = 20), lwd = 0, lwd.ticks = 0.5, padj = 0.4, las = 1)
      
      # Add right axis label with the same formatting as the left
      usr <- par("usr")  # returns c(x1, x2, y1, y2)
      x_offset <- diff(usr[1:2]) * 0.1185  # roughly mimics mtext line=2.5 spacing
      text(x = usr[2] + x_offset, y = 40, labels = "$ per person", srt = 270, xpd = TRUE, cex = 1.3)
      
      # Plot cotton_pc on the right axis
      lines(data$year, data$cotton_pc, col = "black", lwd = 2)
      
      # Add text labels
      text(1808.5, 31.7, "Value (left)", adj = c(0.5, 0.5), cex = 1.3)
      text(1818.9, 5, "Cotton (right)", adj = c(0.5, 0.5), cex = 1.3)
    }
  }
  
  # Console plot
  plot_function()
  
  # PDF plot
  pdf("Figures/value.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
}

# Figure 3a: British Cotton Imports, 1800--1860: (a) Prices
Figure_3a <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height) 
    
    tryCatch({
      cotton_data <- read_ods("Data/data.ods", sheet = "data")
    }, error = function(e) {
      stop("Error reading data.ods: ", e$message, call. = FALSE)
    })
    
    required_cols <- c("year", "us_p", "india_p")
    if (!all(required_cols %in% names(cotton_data))) {
      stop("Missing required columns in data.ods sheet 'data': ", 
           paste(setdiff(required_cols, names(cotton_data)), collapse=", "), call. = FALSE)
    }
    
    # Filter data for the specified range (1800-1860)
    plot_data <- cotton_data %>% filter(year >= 1800 & year <= 1860)
    
    # Define y-axis ticks and labels for log scale
    y_ticks <- c(1, 10, 100)
    y_labels <- format(y_ticks, scientific = FALSE, drop0trailing = TRUE)
    
    plot(c(1810, 1860), c(1, 100),
         type = "n",
         xlab = " ",
         ylab = "Pence per pound",
         xlim = c(1800, 1860),
         ylim = c(1, 100),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE,
         log = "y")
    
    axis(1, at = seq(1800, 1860, by = 10), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 0.5, padj = 0.4) 
    box(lwd = 0.5)
    
    # Use lwd=2 for US, lwd=1 for India (similar to labor/land plots)
    lines(plot_data$year, plot_data$us_p, col = "black", lwd = 2) 
    lines(plot_data$year, plot_data$india_p, col = "black", lwd = 1)
    
    # Add floating text labels (adjust coordinates manually as needed)
    text(1809.3, 36, "American", adj = c(0, 0.5), cex = 1.3)   # Example positions
    text(1807.8, 9.87, "Indian", adj = c(0, 0.5), cex = 1.3)
  }
  
  # PDF plot
  pdf_file <- "Figures/us_p.pdf"
  message("Creating plot: ", pdf_file)
  pdf(pdf_file, width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  message("Finished plot: ", pdf_file)
}

# Figure 3b: British Cotton Imports, 1800--1860: (b) Quantities
Figure_3b <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height) # Wider margin for y-axis labels
    
    tryCatch({
      cotton_data <- read_ods("Data/data.ods", sheet = "data")
    }, error = function(e) {
      stop("Error reading data.ods: ", e$message, call. = FALSE)
    })
    
    required_cols <- c("year", "us_m", "india_m")
    if (!all(required_cols %in% names(cotton_data))) {
      stop("Missing required columns in data.ods sheet 'data': ", 
           paste(setdiff(required_cols, names(cotton_data)), collapse=", "), call. = FALSE)
    }
    
    # Filter data for the specified range (1800-1860)
    plot_data <- cotton_data %>% filter(year >= 1800 & year <= 1860)
    
    # Define y-axis ticks and labels for log scale
    y_ticks <- c(0.01, 0.1, 1, 10, 100, 1000, 10000)
    y_labels <- format(y_ticks, scientific = FALSE, drop0trailing = TRUE, trim=TRUE)
    
    plot(c(1800, 1860), c(1, 10000),
         type = "n",
         xlab = " ",
         ylab = "Million pounds",
         xlim = c(1800, 1860),
         ylim = c(0.01, 10000),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE,
         log = "y")
    
    axis(1, at = seq(1800, 1860, by = 10), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 0.5, padj = 0.4) 
    box(lwd = 0.5)
    
    # Use lwd=2 for US, lwd=1 for India
    lines(plot_data$year, plot_data$us_m, col = "black", lwd = 2)
    lines(plot_data$year, plot_data$india_m, col = "black", lwd = 1)
    
    # Add floating text labels (adjust coordinates manually as needed)
    text(1803.5, 95, "American", adj = c(0, 0.5), cex = 1.3)
    text(1802, 0.205, "Indian", adj = c(0, 0.5), cex = 1.3)
  }
  
  # PDF plot
  pdf_file <- "Figures/us_m.pdf"
  message("Creating plot: ", pdf_file)
  pdf(pdf_file, width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  message("Finished plot: ", pdf_file)
}

# Figure 4: Cotton Production, 1800-1900
Figure_4 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    tryCatch({
      cotton_data <- read_ods("Data/data.ods", sheet = "data")
    }, error = function(e) {
      stop("Error reading data.ods: ", e$message, call. = FALSE)
    })
    
    # Check if cotton column exists
    if (!("cotton" %in% names(cotton_data))) {
      stop("Missing required column 'cotton' in data.ods sheet 'data'", call. = FALSE)
    }
    
    # Filter data for the specified range (1800-1900)
    plot_data <- cotton_data %>% filter(year >= 1800 & year <= 1900)
    
    # Define y-axis ticks and labels for log scale
    y_ticks <- c(0.01, 0.1, 1, 10)
    y_labels <- format(y_ticks, scientific = FALSE, drop0trailing = TRUE)
    
    plot(c(1800, 1900), c(10, 10000),
         type = "n",
         xlab = " ",
         ylab = "Billion pounds of lint",
         xlim = c(1800, 1900),
         ylim = c(0.01, 10),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE,
         log = "y")
    
    # Custom axes with ticks every 20 years as requested
    axis(1, at = seq(1800, 1900, by = 20), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 0.5, padj = 0.4) 
    box(lwd = 0.5)
    
    # Plot data line
    lines(plot_data$year, plot_data$cotton, col = "black", lwd = 1)
  }
  
  # PDF plot
  pdf_file <- "Figures/cotton_usp.pdf"
  message("Creating plot: ", pdf_file)
  pdf(pdf_file, width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  message("Finished plot: ", pdf_file)
}

# Figure 5: Land in the Cotton Belt, 1850-1900
Figure_5 <- function() {
  plot_width <- 9.2
  plot_height <- 5.5
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    tryCatch({
      # Read the CSV file
      data_raw <- read.csv("Data/Cotton_panel_data.csv")
      
      # Filter to include only regions 1, 2, 4, 5, 6, 7
      data_filtered <- data_raw[data_raw$region %in% c(1, 2, 4, 5, 6, 7), ]
      
      # Aggregate by year: sum improved, unimproved, and census_pop
      data_agg <- aggregate(
        cbind(improved, unimproved, census_pop) ~ year, 
        data = data_filtered, 
        FUN = sum
      )
      
      # Calculate acres per person
      data2 <- data.frame(
        year = data_agg$year,
        improved = data_agg$improved / data_agg$census_pop,
        unimproved = data_agg$unimproved / data_agg$census_pop
      )
      
    }, error = function(e) {
      stop("Error processing Cotton_panel_data.csv: ", e$message, call. = FALSE)
    })
    
    plot(c(1850, 1900), c(0, 20),
         type = "n",
         xlab = " ",
         ylab = "Acres per person",
         xlim = c(1850, 1900),
         ylim = c(0, 20),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    axis(1, at = seq(1850, 1900, by = 10), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = -0.1)
    axis(2, at = seq(0, 20, by = 5), labels = TRUE, lwd = 0, lwd.ticks = 0.5, padj = 0.4)
    box(lwd = 0.5)
    
    lines(data2$year, data2$unimproved, col = "black", lwd = 1)
    lines(data2$year, data2$improved, col = "black", lwd = 2)
    
    text(1854, 4.93, "Improved", adj = c(0, 0.5), cex = 1.3)
    text(1855, 16.83, "Unimproved", adj = c(0, 0.5), cex = 1.3)
  }
  
  # PDF plot
  pdf_file <- "Figures/land.pdf"
  message("Creating plot: ", pdf_file)
  pdf(pdf_file, width = plot_width, height = plot_height)
  plot_function()
  dev.off()
  message("Finished plot: ", pdf_file)
}

# --- Execute all plotting functions ---

Figure_1()
Figure_2()
Figure_3a()
Figure_3b()
Figure_4()
Figure_5()


message("All plots generated successfully.")