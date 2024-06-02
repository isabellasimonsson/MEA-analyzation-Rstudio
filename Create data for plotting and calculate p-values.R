# Loading files and renaming colnames -----------------------------------------------------------

setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 2/50k/D89")
well.data.50k <- read.table('well.data.txt', sep = '\t', header = TRUE)
well.metadata.50k <- read.table('filtered.well.metadata.txt', sep = '\t', header = TRUE)
summary.matrix.50k <- read.table('summary.matrix.txt', sep = '\t', header = TRUE)

# Add "50k" prefix to column names in well.data.50k
colnames(well.data.50k) <- paste("50k", colnames(well.data.50k), sep = " ")
# Add "50k" prefix to column names in well.metadata.50k
colnames(well.metadata.50k) <- paste("50k", colnames(well.metadata.50k), sep = " ")
# Add "50k" prefix to column names in summary.matrix.50k
colnames(summary.matrix.50k) <- paste("50k", colnames(summary.matrix.50k), sep = " ")

setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 2/90k/D89")
well.data.90k <- read.table('well.data.txt', sep = '\t', header = TRUE)
well.metadata.90k <- read.table('filtered.well.metadata.txt', sep = '\t', header = TRUE)
summary.matrix.90k <- read.table('summary.matrix.txt', sep = '\t', header = TRUE)

# Add "90k" prefix to column names in well.data.90k
colnames(well.data.90k) <- paste("90k", colnames(well.data.90k), sep = " ")
# Add "90k" prefix to column names in well.metadata.90k
colnames(well.metadata.90k) <- paste("90k", colnames(well.metadata.90k), sep = " ")
# Add "90k" prefix to column names in summary.matrix.90k
colnames(summary.matrix.90k) <- paste("90k", colnames(summary.matrix.90k), sep = " ")

#Set working directory
setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 2/Results/D89")



# Combine matrices --------------------------------------------------------

#Combine well.data
# Get unique row names from all matrices
all_row_names <- unique(c(rownames(well.data.50k), rownames(well.data.90k)))

# Create a new matrix with unique row names and appropriate dimensions
combined.matrix <- matrix(
  NA,
  nrow = length(all_row_names),
  ncol = ncol(well.data.50k) + ncol(well.data.90k),
  dimnames = list(all_row_names, NULL)
)

# Fill the combined matrix with data from all matrices
fill_matrix <- function(data, matrix_name, start_col_index, end_col_index, combined.matrix, all_row_names) {
  for (i in 1:nrow(combined.matrix)) {
    row_name <- all_row_names[i]
    
    if (row_name %in% rownames(data)) {
      subset_data <- data[row_name, , drop = FALSE]
      subset_values <- as.matrix(subset_data)  # Include all columns
      
      # Check if the number of columns in subset_values matches the number of target columns
      if (ncol(subset_values) == (end_col_index - start_col_index + 1)) {
        combined.matrix[i, start_col_index:end_col_index] <- subset_values
      } else {
        cat("Error: Dimensions mismatch for row", row_name, "in", matrix_name, "\n")
      }
    } else {
      combined.matrix[i, start_col_index:end_col_index] <- NA
      cat("Row", row_name, "not found in", matrix_name, "- filled with NA\n")
    }
  }
  return(combined.matrix)
}

combined.matrix <- fill_matrix(well.data.50k, "well.data.50k", 1, ncol(well.data.50k), combined.matrix, all_row_names)
combined.matrix <- fill_matrix(well.data.90k, "well.data.90k", ncol(well.data.50k) + 1, ncol(well.data.50k) + ncol(well.data.90k), combined.matrix, all_row_names)

# Set column names of combined.matrix
colnames(combined.matrix) <- c(colnames(well.data.50k), colnames(well.data.90k))

#Filter out rows with mostly 0 or NA values
unfiltered.combined.matrix <- combined.matrix
# Count non-zero values in each row
non_zero_count <- rowSums(unfiltered.combined.matrix != 0, na.rm = TRUE)
# Identify rows with more than 36 non-zero values
valid_rows <- non_zero_count > 1
# Subset combined.matrix to keep only rows with more than 36 non-zero values
combined.matrix <- combined.matrix[valid_rows, ]


# Combine metadata matrices
combined.metadata <- cbind(well.metadata.50k, well.metadata.90k)



#Combine summary matrices

# Get unique row names from all matrices
all_row_names_summary_matrix <- unique(c(rownames(summary.matrix.50k), rownames(summary.matrix.90k)))

# Create a new matrix with unique row names and appropriate dimensions
combined.summary.matrix <- matrix(
  NA,
  nrow = length(all_row_names_summary_matrix),
  ncol = ncol(summary.matrix.50k) + ncol(summary.matrix.90k),
  dimnames = list(all_row_names_summary_matrix, NULL)
)


combined.summary.matrix <- fill_matrix(summary.matrix.50k, "summary.matrix.50k", 1, ncol(summary.matrix.50k), combined.summary.matrix, all_row_names_summary_matrix)
combined.summary.matrix <- fill_matrix(summary.matrix.90k, "summary.matrix.90k", ncol(summary.matrix.50k) + 1, ncol(summary.matrix.50k) + ncol(summary.matrix.90k), combined.summary.matrix, all_row_names_summary_matrix)

# Set column names of combined.matrix
colnames(combined.summary.matrix) <- c(colnames(summary.matrix.50k), colnames(summary.matrix.90k))

#Filter out rows with mostly 0 or NA values
unfiltered.combined.summary.matrix <- combined.summary.matrix
# Count non-zero values in each row
non_zero_count <- rowSums(unfiltered.combined.summary.matrix != 0, na.rm = TRUE)
# Identify rows with more than 36 non-zero values
valid_rows <- non_zero_count > 5
# Subset combined.matrix to keep only rows with more than 36 non-zero values
combined.summary.matrix <- combined.summary.matrix[valid_rows, ]





# Group wells based on condition ------------------------------------------

# Define a function to extract the condition combination for each well
get.condition.combination <- function(column) {
  paste(combined.metadata[, column], collapse = ", ")
}

# Initialize a list to store the groups
well.groups <- list()

# Iterate over the columns of the metadata matrix
for (col in colnames(combined.metadata)) {
  # Get the condition combination for the current well
  condition.combination <- get.condition.combination(col)
  
  # Check if the group already exists
  if (condition.combination %in% names(well.groups)) {
    well.groups[[condition.combination]] <- c(well.groups[[condition.combination]], col)
  } else {
    well.groups[[condition.combination]] <- col
  }
}



# Add a Condition row to the combined.metadata matrix ---------------------


# Define a function to extract the condition combination for each well
get.condition.combination <- function(column) {
  paste(combined.metadata[, column], collapse = ", ")
}

# Initialize a list to store the groups
well.groups <- list()

# Iterate over the columns of the metadata matrix
for (col in colnames(combined.metadata)) {
  # Get the condition combination for the current well
  condition.combination <- get.condition.combination(col)
  
  # Check if the group already exists
  if (condition.combination %in% names(well.groups)) {
    well.groups[[condition.combination]] <- c(well.groups[[condition.combination]], col)
  } else {
    well.groups[[condition.combination]] <- col
  }
}

# Initialize a vector to store the condition of each well
condition_row <- rep(NA, ncol(combined.metadata))
names(condition_row) <- colnames(combined.metadata)

# Iterate over the well.groups to fill the condition_row
for (condition.combination in names(well.groups)) {
  wells.in.condition.group <- well.groups[[condition.combination]]
  condition_row[wells.in.condition.group] <- condition.combination
}

# Add the condition_row to combined.metadata
combined.metadata <- rbind(combined.metadata, Condition = condition_row)




# Sum the well data based on their conditions -----------------------------

# Initialize a list to store the summarized data for each condition group
summary.per.group <- list()

# Iterate over each condition group
for (condition.combination in names(well.groups)) {
  wells.in.condition.group <- well.groups[[condition.combination]] # Get the wells belonging to the current condition group
  data.in.condition.group <- combined.summary.matrix[, wells.in.condition.group, drop = FALSE] # Get the relevant columns from the summary matrix for the current condition group
  summed.data <- rowSums(data.in.condition.group, na.rm = TRUE) # Sum the values for each row within the current condition group
  summary.per.group[[condition.combination]] <- summed.data # Store the summed data for this condition group in a list
}

# Combine the summarized data vectors into a single matrix
summary.matrix.condition <- do.call(rbind, summary.per.group)
# Transpose the summarized matrix
summary.matrix.condition <- t(summary.matrix.condition)
# Set row names based on measurement types
rownames(summary.matrix.condition) <- rownames(combined.summary.matrix)
# Remove "Mean" from row names in summary.matrix.condition
rownames(summary.matrix.condition) <- gsub("^Mean ", "", rownames(summary.matrix.condition))

# Print the summarized matrix
View(summary.matrix.condition)




# Extract the mean data per electrode for each well -------------------------------------

# Find row indices with names starting with "Mean"
mean_rows <- grep("^Mean", rownames(combined.matrix))
well.mean.data <- combined.matrix[mean_rows, ]

# Loop through each row name and remove "Mean"
new_row_names <- gsub("Mean ", "", rownames(well.mean.data))

# Assign the modified row names back to the data frame
rownames(well.mean.data) <- new_row_names



# Sum the mean data of the electrodes from all wells within each group --------------------------------------

# Initialize a list to store the summarized data for each condition group
summary.per.group <- list()

# Iterate over each condition group
for (condition.combination in names(well.groups)) {
  wells.in.condition.group <- well.groups[[condition.combination]] # Get the wells belonging to the current condition group
  data.in.condition.group <- well.mean.data[, wells.in.condition.group, drop = FALSE] # Get the relevant columns from the summary matrix for the current condition group
  summed.data <- rowSums(data.in.condition.group) # Sum the values for each measurement type (rows) within the current condition group
  summary.per.group[[condition.combination]] <- summed.data # Store the summed data for this condition group in a list
}


condition.mean.data <- do.call(cbind, summary.per.group) # Combine the summarized data vectors into a single matrix
rownames(condition.mean.data) <- rownames(well.mean.data) # Set row names based on measurement types
colnames(condition.mean.data) <- names(well.groups) # Set column names based on unique condition combinations

# Print the summary matrix grouped by conditions
View(condition.mean.data)



# Calculate mean and standard deviation -----------------------------------

# Define a function to calculate means for rows if they exist
calculate.mean.if.present <- function(row.name, matrix, counts) {
  if (row.name %in% rownames(matrix)) {
    mean.values <- matrix[row.name, ] / counts
    return(mean.values)
  } else {
    return(NA)  # If the row is not present, return NA
  }
}


# Calculate the number of wells in each condition group
num.wells.per.condition <- sapply(well.groups, length)

# Define a function to calculate standard deviations for rows if they exist
calculate.sd.if.present <- function(matrix, row_name, well.groups) {
  sd_values <- numeric(length(well.groups))  # Initialize a vector to store standard deviations for each condition
  for (i in seq_along(well.groups)) {
    group_columns <- well.groups[[i]]  # Get the columns belonging to the current condition group
    group_data <- matrix[row_name, group_columns, drop = FALSE]  # Extract data for the current condition group
    sd_values[i] <- sd(group_data, na.rm = TRUE)  # Calculate standard deviation for the group data
  }
  return(sd_values)
}


# Initialize data.for.plotting.conditions matrix
data.for.plotting.conditions <- matrix(NA, nrow = 2 * nrow(condition.mean.data), ncol = length(well.groups))
rownames(data.for.plotting.conditions) <- rep(rownames(well.mean.data), each = 2)
colnames(data.for.plotting.conditions) <- colnames(condition.mean.data)


# Calculate mean and standard deviation for each row if they are present in the matrix
for (i in 1:nrow(well.mean.data)) {
  row_name <- rownames(well.mean.data)[i]
  mean_values <- calculate.mean.if.present(row_name, summary.matrix.condition, num.wells.per.condition)
  group_columns <- well.groups[[condition.combination]] # Get the columns belonging to the current condition group
  sd_values <- calculate.sd.if.present(well.mean.data, row_name, well.groups)  # Pass well.groups as an argument
  data.for.plotting.conditions[i * 2 - 1, ] <- mean_values
  data.for.plotting.conditions[i * 2, ] <- sd_values
}


# Rename the rows to reflect mean and standard deviation
for (i in 1:(nrow(data.for.plotting.conditions)/2)) {
  mean_row_index <- i * 2 - 1
  sd_row_index <- i * 2
  row_name <- rownames(data.for.plotting.conditions)[mean_row_index]
  if (row_name != "") {
    rownames(data.for.plotting.conditions)[mean_row_index] <- paste0("Mean ", row_name)
    rownames(data.for.plotting.conditions)[sd_row_index] <- paste0("Standard deviation ", row_name)
  }
}


# Loop through each row and update the row names accordingly
for (i in 1:nrow(data.for.plotting.conditions)) {
  # Get the current row name
  current_row_name <- rownames(data.for.plotting.conditions)[i]
  
  # Check if the current row name contains "Resistance (kOhms)"
  if (grepl("Resistance \\(kOhms\\)", current_row_name)) {
    # Add "per electrode" to the row name
    new_row_name <- paste(current_row_name, "per electrode", sep = " ")
  } else {
    # Add "per well" to the row name
    new_row_name <- paste(current_row_name, "per well", sep = " ")
  }
  
  # Update the row name in the data frame
  rownames(data.for.plotting.conditions)[i] <- new_row_name
}




# Add Number of covered electrodes, Number of active electrodes and Total number of spikes to data.for.plotting.conditions matrix--------------------------------

# Extract the first three rows from well.data by their names
first.three.rows <- combined.matrix[c("Number of Covered Electrodes", "Number of Active Electrodes", "Total Number of Spikes"), ]
# Initialize a list to store the sums for each group
sums.per.group <- list()
# Iterate over each group defined in well.groups
for (group_name in names(well.groups)) {
  group_columns <- well.groups[[group_name]] # Get the columns belonging to the current group
  group_sum <- rowSums(first.three.rows[, group_columns, drop = FALSE], na.rm = TRUE) # Sum the rows within the current group
  sums.per.group[[group_name]] <- group_sum # Store the sum in the list with the group name as key
}
# Combine the sums for all groups into a single matrix
sums.matrix <- do.call(cbind, sums.per.group)

data.for.plotting.conditions <- rbind(num.wells.per.condition,sums.matrix,data.for.plotting.conditions)
rownames(data.for.plotting.conditions)[1] <- "Number of wells per condition"


# Calculate mean resistance per electrode ---------------------------------

data.for.plotting.conditions["Mean Resistance (kOhms) per electrode",] <- data.for.plotting.conditions["Mean Resistance (kOhms) per electrode",]/(data.for.plotting.conditions["Number of Covered Electrodes",]/num.wells.per.condition)


# Filter out rows with NA or 0 values -------------------------------------

# Create a logical vector identifying rows with any non-zero and non-NA values
rows_to_keep <- apply(data.for.plotting.conditions, 1, function(row) any(row != 0 & !is.na(row)))
# Filter the data to keep only the identified rows
filtered.data.for.plotting.conditions <- data.for.plotting.conditions[rows_to_keep, ]
# Print the filtered data
View(filtered.data.for.plotting.conditions)



# Create matrix to store values from Tukey's HSD ------------------------

# Extract condition names from well.groups
conditions <- names(well.groups)
# Convert conditions to levels
condition <- factor(conditions)
# Convert factor to character vector
condition.levels <- levels(condition)


nlevels.condition.parameter <- length(levels(condition))

# Calculate the number of parameters needed in the matrix
nparameters.condition.parameter <- nlevels.condition.parameter - 1

# Set column names for filtered.parameter based on factor levels
colnames.condition.parameter <- if (nparameters.condition.parameter > 0) {
  paste(levels(condition)[-1], "condition", sep = " ")
} else {
  character(0)
}

colnames.filtered.parameter <- c(colnames.condition.parameter)

# Create matrix to store the coefficients
nmeasurement=nrow(combined.matrix) #Create an empty vector with the length of combined.matrix
filtered.parameter <- matrix(NA, nrow = nmeasurement, ncol = nparameters.condition.parameter)
colnames(filtered.parameter) <- colnames.filtered.parameter



# Create matrix to store p-values-----------------------------------------

# Calculate the number of parameters for condition
nparameters.condition.pvalue <- 1 # There's only one p-value for the condition

# Create matrix to store the p-values
filtered.pvalue <- matrix(NA, nrow = nmeasurement, ncol = nparameters.condition.pvalue)
colnames(filtered.pvalue) <- c("Condition")

# Matrix for condition data
nlevels.condition <- length(levels(factor(names(well.groups)))) * (length(levels(factor(names(well.groups)))) - 1) / 2
tukey.condition <- matrix(NA, nrow = nmeasurement * nlevels.condition, ncol = 4)
colnames(tukey.condition) <- c("Mean difference", "Lower bound", "Upper bound", "Adjusted p-value")
row_names_condition <- character(nrow(tukey.condition))



# Calculate p-value based on condition ------------------------------------

for (i in 1:nmeasurement) {
  sample.data <- as.data.frame(cbind(t(combined.metadata[4,]), t(combined.matrix)[, i]))
  colnames(sample.data) <- c("Condition", "Measurement") # Add column names to the data frame (the measurement varies for each loop)
  
  # Initialize an empty character vector to store condition names
  condition_names <- rep(NA, ncol(combined.matrix))
  
  # Perform ANOVA
  anova <- aov(Measurement ~ Condition, data = sample.data)
  
  # Save coefficients
  filtered.parameter[i, 1:nparameters.condition.parameter] <- anova$coefficients[2:(nparameters.condition.parameter + 1)]
  
  # Save p-values
  summary.filtered.anova <- summary(anova)
  filtered.pvalue[i, 1] <- summary.filtered.anova[[1]]$"Pr(>F)"[1] # Save the p-value 1 ("Condition") from the result
  
  # Perform Tukey's HSD test
  tukey.result <- TukeyHSD(anova)
  
  start.index.condition <- (i - 1) * nlevels.condition + 1
  end.index.condition <- i * nlevels.condition
  
  tukey.result.condition <- tukey.result[["Condition"]]
  result.rows <- nrow(tukey.result.condition)
  
  if (result.rows != nlevels.condition) {
    warning(paste("Row", i, ": The number of comparisons in Tukey's HSD result does not match the expected number of levels. Expected:", nlevels.condition, "Got:", result.rows))
    next
  }
  
  tukey.condition[start.index.condition:(start.index.condition + result.rows - 1), ] <- tukey.result.condition
  
  measurement.names_condition <- rownames(combined.matrix) # Extract measurement names from well.data
  all.row.names_condition <- rownames(tukey.result.condition) # Initialize row names
  measurement.ids_condition <- rep(measurement.names_condition[i], each = nlevels.condition) # Create measurement identifiers using measurement names
  row.names_condition <- paste(all.row.names_condition, measurement.ids_condition) # Combine row names with measurement identifiers
  row_names_condition[start.index.condition:(start.index.condition + result.rows - 1)] <- row.names_condition
}

# Assign row names to tukey.condition
rownames(tukey.condition) <- row_names_condition

# Create a function to assign asterisks based on p-value
add_asterisks <- function(p_value) {
  if (is.na(p_value)) {
    return(NA)  # Return NA for missing values
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# Apply the function to the adjusted p-values
asterisks <- sapply(tukey.condition[, "Adjusted p-value"], add_asterisks)

# Add the asterisks as a new column to the tukey.condition matrix
tukey.condition <- cbind(tukey.condition, Asterisks = asterisks)

# View the updated tukey.condition matrix
print(head(tukey.condition))
View(tukey.condition)





# Filter out rows with significant adjusted p-values --------------------------------

# Define a threshold for significant p-value
threshold <- 0.05

# Extract significant rows from tukey.cells
significant.condition <- tukey.condition[tukey.condition[, "Adjusted p-value"] < threshold, ] # Extract significant rows from tukey.cells
filtered.significant.condition <- significant.condition[!apply(is.na(significant.condition) | significant.condition == "", 1, all), ] # Filter out rows with NA values


# View the significant matrices
View(filtered.significant.condition)




# Save files to the working directory -------------------------------------

write.table(filtered.data.for.plotting.conditions, file = "filtered.data.for.plotting.conditions.txt", sep = "\t", quote = FALSE, col.names = TRUE, row.names = TRUE)
write.table(filtered.significant.condition, file = "filtered.significant.condition.txt", sep = "\t", quote = FALSE, col.names = TRUE, row.names = TRUE) 
write.table(tukey.condition, file = "tukey.condition.txt", sep = "\t", quote = FALSE, col.names = TRUE, row.names = TRUE) 
write.table(combined.metadata, file = "combined.metadata.txt", sep = "\t", quote = FALSE, col.names = TRUE, row.names = TRUE) 
write.table(combined.matrix, file = "combined.matrix.txt", sep = "\t", quote = FALSE, col.names = TRUE, row.names = TRUE) 
writeLines(condition.levels, "condition.levels.txt")

