
#Set Your Working Directory
setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 2/90k/D89")



# Loading electrode data -----------------------------------------------------------

electrode.average = read.table('Excel Electrode averages.txt',sep='\t',header=TRUE)
rownames(electrode.average) <- electrode.average[[1]] # Extract the first column as row names
electrode.average <- electrode.average[, -1] # Remove the first column from the data frame
options(scipen = 999)



# Filtering data ----------------------------------------------------------

# Determine which values are neither NA nor 0 and convert them to TRUE, others to FALSE
electrode.non.zero <- electrode.average != 0 & !is.na(electrode.average)

# Count the number of TRUE values (non-zero electrodes) in each row
electrode.count.nonzero <- apply(electrode.non.zero, 1, sum)

# Filter rows with at least 10 TRUE values
filtered.rows <- electrode.count.nonzero >= 10

# Apply the filtering to the original data
filtered.electrode.average <- electrode.average[filtered.rows, ]




# Filter out uncovered electrodes -----------------------------------------

# Determine which resistance values are at least 18 kOhms and not NA
resistance.above.threshold <- filtered.electrode.average["Resistance (kOhms)", ] >= 18 & !is.na(filtered.electrode.average["Resistance (kOhms)", ])

# Filter out columns where resistance is less than 18 kOhms or NA
covered.electrodes <- filtered.electrode.average[, resistance.above.threshold]

# View the filtered data
View(covered.electrodes)




# Create one group per well -----------------------------------------------

# Create a function to extract electrode groups
extract.electrode.group <- function(column.names) {
  electrode.groups <- unique(sub("_.*", "", column.names))
  group.counts <- sapply(electrode.groups, function(group) {
    sum(grepl(paste0("^", group), column.names))
  })
  return(list(groups = electrode.groups, counts = group.counts))
}

# Extract electrode groups and their counts
electrode.info <- extract.electrode.group(colnames(covered.electrodes))
electrode.groups <- electrode.info$groups
electrode.counts <- electrode.info$counts




# Sum all electrodes within each well ---------------------------------

# Sum each row within the electrode groups
summary.per.group <- lapply(1:nrow(covered.electrodes), function(row.index) {
  row.summary <- sapply(electrode.groups, function(group) {
    group.columns <- grep(paste0("^", group), colnames(covered.electrodes))
    sum(covered.electrodes[row.index, group.columns])
  })
  return(row.summary)
})

# Convert the list to a matrix
summary.matrix <- t(simplify2array(summary.per.group))
rownames(summary.matrix) <- rownames(covered.electrodes) # Add row names
colnames(summary.matrix) <- electrode.groups # Add column names



# Calculate mean and standard deviation -----------------------------------------------

# Define a function to calculate means for rows if they exist
calculate.mean.if.present <- function(row.name, matrix, counts) {
  if (row.name %in% rownames(matrix)) {
    mean.values <- matrix[row.name, ] / counts
    return(mean.values)
  } else {
    return(NA)  # If the row is not present, return NA
  }
}

# Define a function to calculate standard deviation for rows if they exist
calculate.sd.if.present <- function(row.name, matrix) {
  if (row.name %in% rownames(matrix)) {
    sd.values <- apply(matrix[row.name, , drop = FALSE], 2, sd)
    return(sd.values)
  } else {
    return(NA)  # If the row is not present, return NA
  }
}


# Initialize well.data matrix
well.data <- matrix(NA, nrow = 2 * nrow(covered.electrodes), ncol = length(electrode.groups))
rownames(well.data) <- rep(rownames(summary.matrix), each = 2)
colnames(well.data) <- colnames(summary.matrix)

# Calculate mean and standard deviation for each row if they are present in the matrix
for (i in 1:nrow(summary.matrix)) {
  row_name <- rownames(summary.matrix)[i]
  mean_values <- calculate.mean.if.present(row_name, summary.matrix, electrode.counts)
  well.data[i * 2 - 1, ] <- mean_values
  
  # Iterate over each group and calculate standard deviation within each group for each measurement
  for (group_index in 1:length(electrode.groups)) {
    group <- electrode.groups[group_index]
    group_columns <- grep(paste0("^", group), colnames(covered.electrodes))
    
    # Subset the data for the current group
    group_data <- covered.electrodes[i, group_columns]
    
    # Check if the subset is not empty
    if (length(group_data) > 0) {
      # Calculate standard deviation within the group
      group_sd <- sd(group_data, na.rm = TRUE)
      
      # Store the standard deviation in the results matrix
      well.data[i * 2, group_index] <- group_sd
    }
  }
}


# Rename the rows to reflect mean and standard deviation
for (i in 1:(nrow(well.data)/2)) {
  mean_row_index <- i * 2 - 1
  sd_row_index <- i * 2
  row_name <- rownames(well.data)[mean_row_index]
  if (row_name != "") {
    rownames(well.data)[mean_row_index] <- paste0("Mean ", row_name)
    rownames(well.data)[sd_row_index] <- paste0("Standard deviation ", row_name)
  }
}


# Calculate number of active electrodes per well --------------------------

# Initialize a vector to store the counts for each group
active.electrode.counts <- numeric(length(electrode.groups))

# Iterate over each group
for (i in seq_along(electrode.groups)) {
  group <- electrode.groups[i]
  
  # Get the column indices corresponding to the electrodes in the current group
  group.columns <- grep(paste0("^", group), colnames(covered.electrodes))
  
  # Check the condition for each electrode in the group and sum the TRUE values
  active.count <- sum(covered.electrodes["Mean Firing Rate (Hz)", group.columns] >= (5/60))
  
  # Replace NA values with 0
  if (is.na(active.count)) {
    active.count <- 0
  }
  
  active.electrode.counts[i] <- active.count
}





# Add all relevant rows to the well.data matrix -------------------------

#Add number of covered electrodes and total number of spikes to well.data
electrode.counts.matrix <- as.matrix(t(electrode.counts))
well.data <- rbind(electrode.counts.matrix, active.electrode.counts,summary.matrix["Number of Spikes",],well.data)
rownames(well.data)[1] <- "Number of Covered Electrodes"
rownames(well.data)[2] <- "Number of Active Electrodes"
rownames(well.data)[3] <- "Total Number of Spikes"

# View the resulting matrix
View(well.data)




# Load and filter the well metadata -----------------------------------------------------------

well.metadata = read.table('Excel metadata wells.txt',sep='\t',header=TRUE) 
rownames(well.metadata) <- well.metadata[[1]] # Extract the first column as row names
well.metadata <- well.metadata[, -1] # Remove the first column from the data frame

summary.cols <- colnames(summary.matrix)# Get the names of the columns in summary.matrix
filtered.well.metadata <- well.metadata[, intersect(names(well.metadata), summary.cols)] # Filter the well.metadata columns based on matching names


# Save files to wd -------------------------------------------------

write.table(well.data, file = "well.data.txt", sep = "\t", quote = FALSE) 
write.table(filtered.well.metadata, file = "filtered.well.metadata.txt", sep = "\t", quote = FALSE) 
write.table(summary.matrix, file = "summary.matrix.txt", sep = "\t", quote = FALSE) 

