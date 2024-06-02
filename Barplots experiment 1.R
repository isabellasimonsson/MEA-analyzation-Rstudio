library(ggplot2)
library(stringr)
library(dplyr)

# Load data
setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 1/240217")
filtered.data.for.plotting.conditions <- read.table('filtered.data.for.plotting.conditions.txt', sep='\t', header=TRUE, check.names = FALSE)
filtered.metadata <- read.table('filtered.well.metadata.txt', sep='\t', header=TRUE, check.names = FALSE)
filtered.significant.cells <- read.table('filtered.significant.cells.txt', sep='\t', header=TRUE, check.names = FALSE)
filtered.significant.coating <- read.table('filtered.significant.coating.txt', sep='\t', header=TRUE, check.names = FALSE)

# Number of covered Electrodes --------------------------------------------
# Prepare the data for plotting
number.of.covered.electrodes <- t(filtered.data.for.plotting.conditions["Number of Covered Electrodes",])

# Convert to data frame
number.of.covered.electrodes <- as.data.frame(number.of.covered.electrodes)
filtered.metadata <- as.data.frame(t(filtered.metadata))

# Add a column for conditions
number.of.covered.electrodes$Condition <- rownames(number.of.covered.electrodes)
# Reset row names
rownames(number.of.covered.electrodes) <- rownames(filtered.metadata)

# Merge the data frames by row names
number.of.covered.electrodes <- merge(filtered.metadata, number.of.covered.electrodes, by = "row.names")

# Rename columns for clarity
colnames(number.of.covered.electrodes) <- c("Well", "Cells", "Coating", "Electrodes", "Condition")
# Convert Mean and SD columns to numeric
number.of.covered.electrodes$Electrodes <- as.numeric(as.character(number.of.covered.electrodes$Electrodes))

# Extract relevant p-values
filtered.covered.electrodes.cells <- filtered.significant.cells[grepl("Number of Covered Electrodes", rownames(filtered.significant.cells)), 
                                                                c("Adjusted p-value", "Asterisks")]

filtered.covered.electrodes.coating <- filtered.significant.coating[grepl("Number of Covered Electrodes", rownames(filtered.significant.coating)), 
                                                                    c("Adjusted p-value", "Asterisks")]

# Define the pattern for coatings and cell densities
coating_pattern <- "(PLO \\+ LN521|LN521)"
density_pattern <- "(50k|100k|150k)"

# Combine the patterns to match conditions separated by a hyphen
pattern <- paste0("(", coating_pattern, "|", density_pattern, ")-(", 
                  coating_pattern, "|", density_pattern, ")")

# Extract the entire matched pattern
cells <- if (nrow(filtered.covered.electrodes.cells) > 0) {
  str_extract(rownames(filtered.covered.electrodes.cells), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

coating <- if (nrow(filtered.covered.electrodes.coating) > 0) {
  str_extract(rownames(filtered.covered.electrodes.coating), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

# Function to separate the matched conditions
extract_conditions <- function(matched_string) {
  matches <- unlist(strsplit(matched_string, "-"))
  return(matches)
}

# Apply the function to extract cells
separated_cells <- lapply(cells, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Apply the function to extract coating
separated_coating <- lapply(coating, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Convert the list to a data frame
if (length(separated_cells) > 0) {
  cells_df <- as.data.frame(do.call(rbind, separated_cells))
  colnames(cells_df) <- c("Cells1", "Cells2")
} else {
  cells_df <- data.frame(Cells1 = character(0), Cells2 = character(0))
}

if (length(separated_coating) > 0) {
  coating_df <- as.data.frame(do.call(rbind, separated_coating))
  colnames(coating_df) <- c("Coating1", "Coating2")
} else {
  coating_df <- data.frame(Coating1 = character(0), Coating2 = character(0))
}

# Ensure the new columns match the number of rows in filtered.covered.Electrodes
num_rows_cells <- nrow(filtered.covered.electrodes.cells)
num_rows_coating <- nrow(filtered.covered.electrodes.coating)

# Add Cells1 and Cells2 columns
if (nrow(cells_df) > 0) {
  filtered.covered.electrodes.cells$Cells1 <- cells_df[, "Cells1"]
  filtered.covered.electrodes.cells$Cells2 <- cells_df[, "Cells2"]
} else {
  filtered.covered.electrodes.cells$Cells1 <- rep(NA, num_rows_cells)
  filtered.covered.electrodes.cells$Cells2 <- rep(NA, num_rows_cells)
}

# Add Coating1 and Coating2 columns
if (nrow(coating_df) > 0) {
  filtered.covered.electrodes.coating$Coating1 <- coating_df[, "Coating1"]
  filtered.covered.electrodes.coating$Coating2 <- coating_df[, "Coating2"]
} else {
  filtered.covered.electrodes.coating$Coating1 <- rep(NA, num_rows_coating)
  filtered.covered.electrodes.coating$Coating2 <- rep(NA, num_rows_coating)
}

# Calculate the mean values for each Cells level
mean.number.of.covered.electrodes.cells <- number.of.covered.electrodes %>%
  group_by(Cells) %>%
  summarize(mean.electrodes.cells = mean(Electrodes))

# Calculate the mean values for each Coating level
mean.number.of.covered.electrodes.coating <- number.of.covered.electrodes %>%
  group_by(Coating) %>%
  summarize(mean.electrodes.coating = mean(Electrodes))

# Generate plots for Cells and Coating
line_shift = 2
line_heights <- seq(min(number.of.covered.electrodes$Electrodes) + 7 + line_shift, length.out = max(num_rows_cells, num_rows_coating), by = line_shift)

# Plot for Cells
# Reorder factor levels based on the order of appearance in mean_number_of_covered_electrodes$Cells
filtered.covered.electrodes.cells$Cells1 <- factor(filtered.covered.electrodes.cells$Cells1, levels = unique(mean_number_of_covered_electrodes$Cells))
filtered.covered.electrodes.cells$Cells2 <- factor(filtered.covered.electrodes.cells$Cells2, levels = unique(mean_number_of_covered_electrodes$Cells))

# Recalculate the midpoint between Cells1 and Cells2 for x-position of asterisks
filtered.covered.electrodes.cells$midpoint_x <- as.numeric(filtered.covered.electrodes.cells$Cells1) + 
  0.5 * (as.numeric(filtered.covered.electrodes.cells$Cells2) - as.numeric(filtered.covered.electrodes.cells$Cells1))

number.of.covered.electrodes.plot.cells <- ggplot() +
  geom_bar(data = mean.number.of.covered.electrodes.cells, aes(x = Cells, y = mean.electrodes.cells, fill = Cells), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.covered.electrodes.cells,
               aes(x = Cells1, xend = Cells2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.covered.electrodes.cells,
               aes(x = Cells1, xend = Cells1, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the start
  geom_segment(data = filtered.covered.electrodes.cells,
               aes(x = Cells2, xend = Cells2, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the end
  geom_text(data = filtered.covered.electrodes.cells,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Number of Covered Electrodes by Cells", x = NULL, y = "Number of Electrodes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.covered.electrodes.plot.cells)


# Plot for Coating
# Reorder factor levels based on the order of appearance in number.of.covered.Electrodes$coating
filtered.covered.electrodes.coating$Coating1 <- factor(filtered.covered.electrodes.coating$Coating1, levels = unique(number.of.covered.electrodes$Coating))
filtered.covered.electrodes.coating$Coating2 <- factor(filtered.covered.electrodes.coating$Coating2, levels = unique(number.of.covered.electrodes$Coating))

# Recalculate the midpoint between coating1 and coating2 for x-position of asterisks
filtered.covered.electrodes.coating$midpoint_x <- as.numeric(filtered.covered.electrodes.coating$Coating1) + 
  0.5 * (as.numeric(filtered.covered.electrodes.coating$Coating2) - as.numeric(filtered.covered.electrodes.coating$Coating1))

number.of.covered.electrodes.plot.coating <- ggplot() +
  geom_bar(data = mean.number.of.covered.electrodes.coating, aes(x = Coating, y = mean.electrodes.coating, fill = Coating), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.covered.electrodes.coating,
               aes(x = Coating1, xend = Coating2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.covered.electrodes.coating,
               aes(x = Coating1, xend = Coating1, y = line_heights - 1, yend = line_heights + 1)) + # Add vertical line at the start
  geom_segment(data = filtered.covered.electrodes.coating,
               aes(x = Coating2, xend = Coating2, y = line_heights - 1, yend = line_heights + 1)) + # Add vertical line at the end
  geom_text(data = filtered.covered.electrodes.coating,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Number of Covered Electrodes by coating", x = NULL, y = "Number of Electrodes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.covered.electrodes.plot.coating)


# Save the plots to the working directory
ggsave("number.of.covered.electrodes.plot.cells.png", plot = number.of.covered.electrodes.plot.cells, width = 10, height = 6)
ggsave("number.of.covered.electrodes.plot.coating.png", plot = number.of.covered.electrodes.plot.coating, width = 10, height = 6)




# Number of Active Electrodes ---------------------------------------------
# Prepare the data for plotting
number.of.active.electrodes <- t(filtered.data.for.plotting.conditions["Number of Active Electrodes",])

# Convert to data frame
number.of.active.electrodes <- as.data.frame(number.of.active.electrodes)

# Add a column for conditions
number.of.active.electrodes$Condition <- rownames(number.of.active.electrodes)
# Reset row names
rownames(number.of.active.electrodes) <- rownames(filtered.metadata)

# Merge the data frames by row names
number.of.active.electrodes <- merge(filtered.metadata, number.of.active.electrodes, by = "row.names")

# Rename columns for clarity
colnames(number.of.active.electrodes) <- c("Well", "Cells", "Coating", "Electrodes", "Condition")
# Convert Mean and SD columns to numeric
number.of.active.electrodes$Electrodes <- as.numeric(as.character(number.of.active.electrodes$Electrodes))

# Extract relevant p-values
filtered.active.electrodes.cells <- filtered.significant.cells[grepl("Number of Active Electrodes", rownames(filtered.significant.cells)), 
                                                                c("Adjusted p-value", "Asterisks")]

filtered.active.electrodes.coating <- filtered.significant.coating[grepl("Number of Active Electrodes", rownames(filtered.significant.coating)), 
                                                                    c("Adjusted p-value", "Asterisks")]

# Define the pattern for coatings and cell densities
coating_pattern <- "(PLO \\+ LN521|LN521)"
density_pattern <- "(50k|100k|150k)"

# Combine the patterns to match conditions separated by a hyphen
pattern <- paste0("(", coating_pattern, "|", density_pattern, ")-(", 
                  coating_pattern, "|", density_pattern, ")")

# Extract the entire matched pattern
cells <- if (nrow(filtered.active.electrodes.cells) > 0) {
  str_extract(rownames(filtered.active.electrodes.cells), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

coating <- if (nrow(filtered.active.electrodes.coating) > 0) {
  str_extract(rownames(filtered.active.electrodes.coating), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

# Function to separate the matched conditions
extract_conditions <- function(matched_string) {
  matches <- unlist(strsplit(matched_string, "-"))
  return(matches)
}

# Apply the function to extract cells
separated_cells <- lapply(cells, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Apply the function to extract coating
separated_coating <- lapply(coating, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Convert the list to a data frame
if (length(separated_cells) > 0) {
  cells_df <- as.data.frame(do.call(rbind, separated_cells))
  colnames(cells_df) <- c("Cells1", "Cells2")
} else {
  cells_df <- data.frame(Cells1 = character(0), Cells2 = character(0))
}

if (length(separated_coating) > 0) {
  coating_df <- as.data.frame(do.call(rbind, separated_coating))
  colnames(coating_df) <- c("Coating1", "Coating2")
} else {
  coating_df <- data.frame(Coating1 = character(0), Coating2 = character(0))
}

# Ensure the new columns match the number of rows in filtered.active.Electrodes
num_rows_cells <- nrow(filtered.active.electrodes.cells)
num_rows_coating <- nrow(filtered.active.electrodes.coating)

# Add Cells1 and Cells2 columns
if (nrow(cells_df) > 0) {
  filtered.active.electrodes.cells$Cells1 <- cells_df[, "Cells1"]
  filtered.active.electrodes.cells$Cells2 <- cells_df[, "Cells2"]
} else {
  filtered.active.electrodes.cells$Cells1 <- rep(NA, num_rows_cells)
  filtered.active.electrodes.cells$Cells2 <- rep(NA, num_rows_cells)
}

# Add Coating1 and Coating2 columns
if (nrow(coating_df) > 0) {
  filtered.active.electrodes.coating$Coating1 <- coating_df[, "Coating1"]
  filtered.active.electrodes.coating$Coating2 <- coating_df[, "Coating2"]
} else {
  filtered.active.electrodes.coating$Coating1 <- rep(NA, num_rows_coating)
  filtered.active.electrodes.coating$Coating2 <- rep(NA, num_rows_coating)
}


# Calculate the mean values for each Cells level
mean.number.of.active.electrodes.cells <- number.of.active.electrodes %>%
  group_by(Cells) %>%
  summarize(mean.electrodes.cells = mean(Electrodes))

# Calculate the mean values for each Coating level
mean.number.of.active.electrodes.coating <- number.of.active.electrodes %>%
  group_by(Coating) %>%
  summarize(mean.electrodes.coating = mean(Electrodes))


# Generate plots for Cells and Coating
line_shift = 2
line_heights <- seq(min(number.of.active.electrodes$Electrodes) + 5 + line_shift, length.out = max(num_rows_cells, num_rows_coating), by = line_shift)

# Plot for Cells
# Reorder factor levels based on the order of appearance in number.of.active.Electrodes$Cells
filtered.active.electrodes.cells$Cells1 <- factor(filtered.active.electrodes.cells$Cells1, levels = unique(number.of.active.electrodes$Cells))
filtered.active.electrodes.cells$Cells2 <- factor(filtered.active.electrodes.cells$Cells2, levels = unique(number.of.active.electrodes$Cells))

# Recalculate the midpoint between Cells1 and Cells2 for x-position of asterisks
filtered.active.electrodes.cells$midpoint_x <- as.numeric(filtered.active.electrodes.cells$Cells1) + 
  0.5 * (as.numeric(filtered.active.electrodes.cells$Cells2) - as.numeric(filtered.active.electrodes.cells$Cells1))

number.of.active.electrodes.plot.cells <- ggplot() +
  geom_bar(data = mean.number.of.active.electrodes.cells, aes(x = Cells, y = mean.electrodes.cells, fill = Cells), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.active.electrodes.cells,
               aes(x = Cells1, xend = Cells2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.active.electrodes.cells,
               aes(x = Cells1, xend = Cells1, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the start
  geom_segment(data = filtered.active.electrodes.cells,
               aes(x = Cells2, xend = Cells2, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the end
  geom_text(data = filtered.active.electrodes.cells,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Number of Active Electrodes by Cells", x = NULL, y = "Number of Electrodes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.active.electrodes.plot.cells)


# Plot for Coating
# Reorder factor levels based on the order of appearance in number.of.active.Electrodes$coating
filtered.active.electrodes.coating$Coating1 <- factor(filtered.active.electrodes.coating$Coating1, levels = unique(number.of.active.electrodes$Coating))
filtered.active.electrodes.coating$Coating2 <- factor(filtered.active.electrodes.coating$Coating2, levels = unique(number.of.active.electrodes$Coating))

# Recalculate the midpoint between coating1 and coating2 for x-position of asterisks
filtered.active.electrodes.coating$midpoint_x <- as.numeric(filtered.active.electrodes.coating$Coating1) + 
  0.5 * (as.numeric(filtered.active.electrodes.coating$Coating2) - as.numeric(filtered.active.electrodes.coating$Coating1))

number.of.active.electrodes.plot.coating <- ggplot() +
  geom_bar(data = mean.number.of.active.electrodes.coating, aes(x = Coating, y = mean.electrodes.coating, fill = Coating), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.active.electrodes.coating,
               aes(x = Coating1, xend = Coating2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.active.electrodes.coating,
               aes(x = Coating1, xend = Coating1, y = line_heights - 1, yend = line_heights + 1)) + # Add vertical line at the start
  geom_segment(data = filtered.active.electrodes.coating,
               aes(x = Coating2, xend = Coating2, y = line_heights - 1, yend = line_heights + 1)) + # Add vertical line at the end
  geom_text(data = filtered.active.electrodes.coating,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Number of Active Electrodes by coating", x = NULL, y = "Number of Electrodes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.active.electrodes.plot.coating)


# Save the plots to the working directory
ggsave("number.of.active.electrodes.plot.cells.png", plot = number.of.active.electrodes.plot.cells, width = 10, height = 6)
ggsave("number.of.active.electrodes.plot.coating.png", plot = number.of.active.electrodes.plot.coating, width = 10, height = 6)



# Mean Firing Rate (Hz) ---------------------------------------------------

# Prepare the data for plotting
firing.rate <- t(filtered.data.for.plotting.conditions["Mean Firing Rate (Hz) per well",])

# Convert to data frame
firing.rate <- as.data.frame(firing.rate)

# Add a column for conditions
firing.rate$Condition <- rownames(firing.rate)
# Reset row names
rownames(firing.rate) <- rownames(filtered.metadata)

# Merge the data frames by row names
firing.rate <- merge(filtered.metadata, firing.rate, by = "row.names")

# Rename columns for clarity
colnames(firing.rate) <- c("Well", "Cells", "Coating", "Rate", "Condition")
# Convert Mean and SD columns to numeric
firing.rate$Rate <- as.numeric(as.character(firing.rate$Rate))

# Extract relevant p-values
filtered.firing.rate.cells <- filtered.significant.cells[grepl("Mean Firing Rate", rownames(filtered.significant.cells)), 
                                                               c("Adjusted p-value", "Asterisks")]

filtered.firing.rate.coating <- filtered.significant.coating[grepl("Mean Firing Rate", rownames(filtered.significant.coating)), 
                                                                   c("Adjusted p-value", "Asterisks")]

# Define the pattern for coatings and cell densities
coating_pattern <- "(PLO \\+ LN521|LN521)"
density_pattern <- "(50k|100k|150k)"

# Combine the patterns to match conditions separated by a hyphen
pattern <- paste0("(", coating_pattern, "|", density_pattern, ")-(", 
                  coating_pattern, "|", density_pattern, ")")

# Extract the entire matched pattern
cells <- if (nrow(filtered.firing.rate.cells) > 0) {
  str_extract(rownames(filtered.firing.rate.cells), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

coating <- if (nrow(filtered.firing.rate.coating) > 0) {
  str_extract(rownames(filtered.firing.rate.coating), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

# Function to separate the matched conditions
extract_conditions <- function(matched_string) {
  matches <- unlist(strsplit(matched_string, "-"))
  return(matches)
}

# Apply the function to extract cells
separated_cells <- lapply(cells, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Apply the function to extract coating
separated_coating <- lapply(coating, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Convert the list to a data frame
if (length(separated_cells) > 0) {
  cells_df <- as.data.frame(do.call(rbind, separated_cells))
  colnames(cells_df) <- c("Cells1", "Cells2")
} else {
  cells_df <- data.frame(Cells1 = character(0), Cells2 = character(0))
}

if (length(separated_coating) > 0) {
  coating_df <- as.data.frame(do.call(rbind, separated_coating))
  colnames(coating_df) <- c("Coating1", "Coating2")
} else {
  coating_df <- data.frame(Coating1 = character(0), Coating2 = character(0))
}

# Ensure the new columns match the number of rows in filtered.firing.rate
num_rows_cells <- nrow(filtered.firing.rate.cells)
num_rows_coating <- nrow(filtered.firing.rate.coating)

# Add Cells1 and Cells2 columns
if (nrow(cells_df) > 0) {
  filtered.firing.rate.cells$Cells1 <- cells_df[, "Cells1"]
  filtered.firing.rate.cells$Cells2 <- cells_df[, "Cells2"]
} else {
  filtered.firing.rate.cells$Cells1 <- rep(NA, num_rows_cells)
  filtered.firing.rate.cells$Cells2 <- rep(NA, num_rows_cells)
}

# Add Coating1 and Coating2 columns
if (nrow(coating_df) > 0) {
  filtered.firing.rate.coating$Coating1 <- coating_df[, "Coating1"]
  filtered.firing.rate.coating$Coating2 <- coating_df[, "Coating2"]
} else {
  filtered.firing.rate.coating$Coating1 <- rep(NA, num_rows_coating)
  filtered.firing.rate.coating$Coating2 <- rep(NA, num_rows_coating)
}


# Calculate the mean values for each Cells level
mean.firing.rate.cells <- firing.rate %>%
  group_by(Cells) %>%
  summarize(mean.rate.cells = mean(Rate))

# Calculate the mean values for each Coating level
mean.firing.rate.coating <- firing.rate %>%
  group_by(Coating) %>%
  summarize(mean.rate.coating = mean(Rate))



# Generate plots for Cells and Coating
line_shift = 2
line_heights <- seq(min(firing.rate$Rate) + 5 + line_shift, length.out = max(num_rows_cells, num_rows_coating), by = line_shift)

# Plot for Cells
# Reorder factor levels based on the order of appearance in firing.rate$Cells
filtered.firing.rate.cells$Cells1 <- factor(filtered.firing.rate.cells$Cells1, levels = unique(firing.rate$Cells))
filtered.firing.rate.cells$Cells2 <- factor(filtered.firing.rate.cells$Cells2, levels = unique(firing.rate$Cells))

# Recalculate the midpoint between Cells1 and Cells2 for x-position of asterisks
filtered.firing.rate.cells$midpoint_x <- as.numeric(filtered.firing.rate.cells$Cells1) + 
  0.5 * (as.numeric(filtered.firing.rate.cells$Cells2) - as.numeric(filtered.firing.rate.cells$Cells1))

firing.rate.plot.cells <- ggplot() +
  geom_bar(data = mean.firing.rate.cells, aes(x = Cells, y = mean.rate.cells, fill = Cells), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.firing.rate.cells,
               aes(x = Cells1, xend = Cells2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.firing.rate.cells,
               aes(x = Cells1, xend = Cells1, y = line_heights - 0.5, yend = line_heights + 0.5)) + # Add vertical line at the start
  geom_segment(data = filtered.firing.rate.cells,
               aes(x = Cells2, xend = Cells2, y = line_heights - 0.5, yend = line_heights + 0.5)) + # Add vertical line at the end
  geom_text(data = filtered.firing.rate.cells,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Firing Rate by Cells", x = NULL, y = "Firing Rate (Hz)") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(firing.rate.plot.cells)


# Plot for Coating
# Reorder factor levels based on the order of appearance in firing.rate$coating
filtered.firing.rate.coating$Coating1 <- factor(filtered.firing.rate.coating$Coating1, levels = unique(firing.rate$Coating))
filtered.firing.rate.coating$Coating2 <- factor(filtered.firing.rate.coating$Coating2, levels = unique(firing.rate$Coating))

# Recalculate the midpoint between coating1 and coating2 for x-position of asterisks
filtered.firing.rate.coating$midpoint_x <- as.numeric(filtered.firing.rate.coating$Coating1) + 
  0.5 * (as.numeric(filtered.firing.rate.coating$Coating2) - as.numeric(filtered.firing.rate.coating$Coating1))

firing.rate.plot.coating <- ggplot() +
  geom_bar(data = mean.firing.rate.coating, aes(x = Coating, y = mean.rate.coating, fill = Coating), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.firing.rate.coating,
               aes(x = Coating1, xend = Coating2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.firing.rate.coating,
               aes(x = Coating1, xend = Coating1, y = line_heights - 0.5, yend = line_heights + 0.5)) + # Add vertical line at the start
  geom_segment(data = filtered.firing.rate.coating,
               aes(x = Coating2, xend = Coating2, y = line_heights - 0.5, yend = line_heights + 0.5)) + # Add vertical line at the end
  geom_text(data = filtered.firing.rate.coating,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Firing Rate by coating", x = NULL, y = "Firing Rate (Hz)") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(firing.rate.plot.coating)


# Save the plots to the working directory
ggsave("firing.rate.plot.cells.png", plot = firing.rate.plot.cells, width = 10, height = 6)
ggsave("firing.rate.plot.coating.png", plot = firing.rate.plot.coating, width = 10, height = 6)





# Resistance --------------------------------------------------------------
# Prepare the data for plotting
resistance <- t(filtered.data.for.plotting.conditions["Mean Resistance (kOhms) per electrode",])

# Convert to data frame
resistance <- as.data.frame(resistance)

# Add a column for conditions
resistance$Condition <- rownames(resistance)
# Reset row names
rownames(resistance) <- rownames(filtered.metadata)

# Merge the data frames by row names
resistance <- merge(filtered.metadata, resistance, by = "row.names")

# Rename columns for clarity
colnames(resistance) <- c("Well", "Cells", "Coating", "Resistance", "Condition")
# Convert Mean and SD columns to numeric
resistance$Resistance <- as.numeric(as.character(resistance$Resistance))

# Extract relevant p-values
filtered.resistance.cells <- filtered.significant.cells[grepl("Mean Resistance", rownames(filtered.significant.cells)), 
                                                               c("Adjusted p-value", "Asterisks")]

filtered.resistance.coating <- filtered.significant.coating[grepl("Mean Resistance", rownames(filtered.significant.coating)), 
                                                                   c("Adjusted p-value", "Asterisks")]

# Define the pattern for coatings and cell densities
coating_pattern <- "(PLO \\+ LN521|LN521)"
density_pattern <- "(50k|100k|150k)"

# Combine the patterns to match conditions separated by a hyphen
pattern <- paste0("(", coating_pattern, "|", density_pattern, ")-(", 
                  coating_pattern, "|", density_pattern, ")")

# Extract the entire matched pattern
cells <- if (nrow(filtered.resistance.cells) > 0) {
  str_extract(rownames(filtered.resistance.cells), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

coating <- if (nrow(filtered.resistance.coating) > 0) {
  str_extract(rownames(filtered.resistance.coating), pattern)
} else {
  character(0) # Return an empty character vector if no matches
}

# Function to separate the matched conditions
extract_conditions <- function(matched_string) {
  matches <- unlist(strsplit(matched_string, "-"))
  return(matches)
}

# Apply the function to extract cells
separated_cells <- lapply(cells, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Apply the function to extract coating
separated_coating <- lapply(coating, function(cond) {
  if (!is.na(cond)) {
    extract_conditions(cond)
  } else {
    c(NA, NA) # If no match, return NA for both conditions
  }
})

# Convert the list to a data frame
if (length(separated_cells) > 0) {
  cells_df <- as.data.frame(do.call(rbind, separated_cells))
  colnames(cells_df) <- c("Cells1", "Cells2")
} else {
  cells_df <- data.frame(Cells1 = character(0), Cells2 = character(0))
}

if (length(separated_coating) > 0) {
  coating_df <- as.data.frame(do.call(rbind, separated_coating))
  colnames(coating_df) <- c("Coating1", "Coating2")
} else {
  coating_df <- data.frame(Coating1 = character(0), Coating2 = character(0))
}

# Ensure the new columns match the number of rows in filtered.active.Resistance
num_rows_cells <- nrow(filtered.resistance.cells)
num_rows_coating <- nrow(filtered.resistance.coating)

# Add Cells1 and Cells2 columns
if (nrow(cells_df) > 0) {
  filtered.resistance.cells$Cells1 <- cells_df[, "Cells1"]
  filtered.resistance.cells$Cells2 <- cells_df[, "Cells2"]
} else {
  filtered.resistance.cells$Cells1 <- rep(NA, num_rows_cells)
  filtered.resistance.cells$Cells2 <- rep(NA, num_rows_cells)
}

# Add Coating1 and Coating2 columns
if (nrow(coating_df) > 0) {
  filtered.resistance.coating$Coating1 <- coating_df[, "Coating1"]
  filtered.resistance.coating$Coating2 <- coating_df[, "Coating2"]
} else {
  filtered.resistance.coating$Coating1 <- rep(NA, num_rows_coating)
  filtered.resistance.coating$Coating2 <- rep(NA, num_rows_coating)
}

# Calculate the mean values for each Cells level
mean.resistance.cells <- resistance %>%
  group_by(Cells) %>%
  summarize(resistance.cells = mean(Resistance))

# Calculate the mean values for each Coating level
mean.resistance.coating <- resistance %>%
  group_by(Coating) %>%
  summarize(resistance.coating = mean(Resistance))

# Generate plots for Cells and Coating
line_shift = 2
line_heights <- seq(min(resistance$Resistance) + 5 + line_shift, length.out = max(num_rows_cells, num_rows_coating), by = line_shift)

# Plot for Cells
# Reorder factor levels based on the order of appearance in number.of.active.Resistance$Cells
filtered.resistance.cells$Cells1 <- factor(filtered.resistance.cells$Cells1, levels = unique(resistance$Cells))
filtered.resistance.cells$Cells2 <- factor(filtered.resistance.cells$Cells2, levels = unique(resistance$Cells))

# Recalculate the midpoint between Cells1 and Cells2 for x-position of asterisks
filtered.resistance.cells$midpoint_x <- as.numeric(filtered.resistance.cells$Cells1) + 
  0.5 * (as.numeric(filtered.resistance.cells$Cells2) - as.numeric(filtered.resistance.cells$Cells1))

resistance.plot.cells <- ggplot() +
  geom_bar(data = mean.resistance.cells, aes(x = Cells, y = resistance.cells, fill = Cells), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.resistance.cells,
               aes(x = Cells1, xend = Cells2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.resistance.cells,
               aes(x = Cells1, xend = Cells1, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the start
  geom_segment(data = filtered.resistance.cells,
               aes(x = Cells2, xend = Cells2, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the end
  geom_text(data = filtered.resistance.cells,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Resistance by Cells", x = NULL, y = "Resistance (kOhms)") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(resistance.plot.cells)


# Plot for Coating
# Reorder factor levels based on the order of appearance in number.of.active.Resistance$coating
filtered.resistance.coating$Coating1 <- factor(filtered.resistance.coating$Coating1, levels = unique(resistance$Coating))
filtered.resistance.coating$Coating2 <- factor(filtered.resistance.coating$Coating2, levels = unique(resistance$Coating))

# Recalculate the midpoint between coating1 and coating2 for x-position of asterisks
filtered.resistance.coating$midpoint_x <- as.numeric(filtered.resistance.coating$Coating1) + 
  0.5 * (as.numeric(filtered.resistance.coating$Coating2) - as.numeric(filtered.resistance.coating$Coating1))

resistance.plot.coating <- ggplot() +
  geom_bar(data = mean.resistance.coating, aes(x = Coating, y = resistance.coating, fill = Coating), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.resistance.coating,
               aes(x = Coating1, xend = Coating2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.resistance.coating,
               aes(x = Coating1, xend = Coating1, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the start
  geom_segment(data = filtered.resistance.coating,
               aes(x = Coating2, xend = Coating2, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the end
  geom_text(data = filtered.resistance.coating,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Resistance by coating", x = NULL, y = "Resistance (kOhms)") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(resistance.plot.coating)


# Save the plots to the working directory
ggsave("resistance.plot.cells.png", plot = resistance.plot.cells, width = 10, height = 6)
ggsave("resistance.plot.coating.png", plot = resistance.plot.coating, width = 10, height = 6)


