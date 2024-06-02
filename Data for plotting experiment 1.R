# Loading files and renaming colnames -----------------------------------------------------------

setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 1/240229")
well.data <- read.table('well.data.txt', sep = '\t', header = TRUE)
well.metadata <- read.table('filtered.well.metadata.txt', sep = '\t', header = TRUE)
summary.matrix <- read.table('summary.matrix.txt', sep = '\t', header = TRUE)


# Group wells based on condition ------------------------------------------

# Define a function to extract the condition combination for each well
get.condition.combination <- function(column) {
  paste(well.metadata[, column], collapse = ", ")
}

# Initialize a list to store the groups
well.groups <- list()

# Iterate over the columns of the metadata matrix
for (col in colnames(well.metadata)) {
  # Get the condition combination for the current well
  condition.combination <- get.condition.combination(col)
  
  # Check if the group already exists
  if (condition.combination %in% names(well.groups)) {
    well.groups[[condition.combination]] <- c(well.groups[[condition.combination]], col)
  } else {
    well.groups[[condition.combination]] <- col
  }
}



# Add a Condition row to the well.metadata matrix ---------------------


# Define a function to extract the condition combination for each well
get.condition.combination <- function(column) {
  paste(well.metadata[, column], collapse = ", ")
}

# Initialize a list to store the groups
well.groups <- list()

# Iterate over the columns of the metadata matrix
for (col in colnames(well.metadata)) {
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
condition_row <- rep(NA, ncol(well.metadata))
names(condition_row) <- colnames(well.metadata)

# Iterate over the well.groups to fill the condition_row
for (condition.combination in names(well.groups)) {
  wells.in.condition.group <- well.groups[[condition.combination]]
  condition_row[wells.in.condition.group] <- condition.combination
}

# Add the condition_row to well.metadata
well.metadata <- rbind(well.metadata, Condition = condition_row)




# Sum the well data based on their conditions -----------------------------

# Initialize a list to store the summarized data for each condition group
summary.per.group <- list()

# Iterate over each condition group
for (condition.combination in names(well.groups)) {
  wells.in.condition.group <- well.groups[[condition.combination]] # Get the wells belonging to the current condition group
  data.in.condition.group <- summary.matrix[, wells.in.condition.group, drop = FALSE] # Get the relevant columns from the summary matrix for the current condition group
  summed.data <- rowSums(data.in.condition.group, na.rm = TRUE) # Sum the values for each row within the current condition group
  summary.per.group[[condition.combination]] <- summed.data # Store the summed data for this condition group in a list
}

# Combine the summarized data vectors into a single matrix
summary.matrix.condition <- do.call(rbind, summary.per.group)
# Transpose the summarized matrix
summary.matrix.condition <- t(summary.matrix.condition)
# Set row names based on measurement types
rownames(summary.matrix.condition) <- rownames(summary.matrix)
# Remove "Mean" from row names in summary.matrix.condition
rownames(summary.matrix.condition) <- gsub("^Mean ", "", rownames(summary.matrix.condition))

# Print the summarized matrix
View(summary.matrix.condition)




# Extract the mean data per electrode for each well -------------------------------------

# Find row indices with names starting with "Mean"
mean_rows <- grep("^Mean", rownames(well.data))
well.mean.data <- well.data[mean_rows, ]

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
first.three.rows <- well.data[c("Number of Covered Electrodes", "Number of Active Electrodes", "Total Number of Spikes"), ]
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






# Save files to the working directory -------------------------------------

write.table(filtered.data.for.plotting.conditions, file = "filtered.data.for.plotting.conditions.txt", sep = "\t", quote = FALSE, col.names = TRUE, row.names = TRUE)

