# Loading files and renaming colnames-----------------------------------------------------------
setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 1/240312")
well.data <- read.table('well.data.txt', sep='\t', header=TRUE)
well.metadata <- read.table('filtered.well.metadata.txt', sep='\t', header=TRUE)

# Create levels -------------------------------------------------------------------

# Convert categorical variables into factors in the well.metadata matrix
cells <- as.factor(t(well.metadata)[, 1])
coating <- as.factor(t(well.metadata)[, 2])

# Create matrix to store coefficients from ANOVA-------------------------------------

# Determine the number of levels in each factor variable
nlevels.cells.parameter <- length(levels(cells))
nlevels.coating.parameter <- length(levels(coating))

# Calculate the number of parameters for cells and coating
nparameters.cells.parameter <- nlevels.cells.parameter - 1
nparameters.coating.parameter <- nlevels.coating.parameter - 1

# Set column names for filtered.parameter based on factor levels
colnames.cells.parameter <- paste(levels(cells)[-1], " cells", sep = "")
colnames.coating.parameter <- paste(levels(coating)[-1], " coating", sep = "")
colnames.filtered.parameter <- c(colnames.cells.parameter, colnames.coating.parameter)

# Create matrix to store the coefficients
nmeasurement <- nrow(well.data)
filtered.parameter <- matrix(NA, nrow = nmeasurement, ncol = nparameters.cells.parameter + nparameters.coating.parameter)
colnames(filtered.parameter) <- colnames.filtered.parameter

# Create matrix to store p-values from ANOVA-----------------------------------------

# Calculate the number of parameters for cells and coating
nparameters.cells.pvalue <- 1
nparameters.coating.pvalue <- 1

# Create matrix to store the p-values
filtered.pvalue <- matrix(NA, nrow = nmeasurement, ncol = nparameters.cells.pvalue + nparameters.coating.pvalue)
colnames(filtered.pvalue) <- c("Cell density", "Coating")

# Create matrices to store values from Tukey's HSD ------------------------

# Matrix for cell density data
nlevels.cells <- choose(length(levels(cells)), 2)
tukey.cells <- matrix(NA, nrow = nmeasurement * nlevels.cells, ncol = 4)
colnames(tukey.cells) <- c("Mean difference", "Lower bound", "Upper bound", "Adjusted p-value")
row_names_cells <- character(nrow(tukey.cells))

# Matrix for coating data
nlevels.coating <- choose(length(levels(coating)), 2)
tukey.coating <- matrix(NA, nrow = nmeasurement * nlevels.coating, ncol = 4)
colnames(tukey.coating) <- c("Mean difference", "Lower bound", "Upper bound", "Adjusted p-value")
row_names_coating <- character(nrow(tukey.coating))

# Calculate ANOVA and Tukey's test ----------------------------------------

# Calculate ANOVA
for (i in 1:nmeasurement) {
  sample.data <- data.frame(Cells = cells, Coating = coating, Measurement = t(well.data)[, i])
  
  # Check if factors have at least two levels
  if (length(unique(sample.data$Cells)) < 2 || length(unique(sample.data$Coating)) < 2) {
    cat("Skipping iteration", i, "due to insufficient factor levels.\n")
    next
  }
  
  anova <- tryCatch(aov(Measurement ~ Cells + Coating, data = sample.data), 
                    error = function(e) {
                      cat("ANOVA failed at iteration", i, "with error:", e$message, "\n")
                      return(NULL)
                    })
  
  if (is.null(anova)) next
  
  # Save coefficients
  if (length(anova$coefficients) > 1) {
    filtered.parameter[i, 1:nparameters.cells.parameter] <- anova$coefficients[2:(nparameters.cells.parameter + 1)]
    filtered.parameter[i, (nparameters.cells.parameter + 1):(nparameters.cells.parameter + nparameters.coating.parameter)] <- anova$coefficients[(nparameters.cells.parameter + 2):(nparameters.cells.parameter + nparameters.coating.parameter + 1)]
  } else {
    cat("ANOVA coefficients are not as expected at iteration", i, "\n")
  }
  
  # Save p-values
  summary.filtered.anova <- summary(anova)
  if (length(summary.filtered.anova[[1]]$"Pr(>F)") >= 2) {
    filtered.pvalue[i, 1] <- summary.filtered.anova[[1]]$"Pr(>F)"[1] # p-value for "Cells"
    filtered.pvalue[i, 2] <- summary.filtered.anova[[1]]$"Pr(>F)"[2] # p-value for "Coating"
  } else {
    cat("ANOVA summary does not have expected p-values at iteration", i, "\n")
  }
  
  # Perform Tukey's HSD test
  tukey.result <- tryCatch(TukeyHSD(anova), 
                           error = function(e) {
                             cat("TukeyHSD failed at iteration", i, "with error:", e$message, "\n")
                             return(NULL)
                           })
  
  if (is.null(tukey.result)) next
  
  if (!is.null(tukey.result[["Cells"]])) {
    start.index.cells <- (i - 1) * nlevels.cells + 1
    end.index.cells <- i * nlevels.cells
    tukey.result.cells <- tukey.result[["Cells"]]
    tukey.cells[start.index.cells:end.index.cells, ] <- tukey.result.cells
    
    measurement.names.cells <- rownames(well.data)
    all.row.names.cells <- rownames(tukey.result.cells)
    measurement.ids.cells <- rep(measurement.names.cells[i], each = nlevels.cells)
    row.names.cells <- paste(all.row.names.cells, measurement.ids.cells)
    row_names_cells[start.index.cells:end.index.cells] <- row.names.cells
  } else {
    cat("Tukey's test for Cells is not available at iteration", i, "\n")
  }
  
  if (!is.null(tukey.result[["Coating"]])) {
    start.index.coating <- (i - 1) * nlevels.coating + 1
    end.index.coating <- i * nlevels.coating
    tukey.result.coating <- tukey.result[["Coating"]]
    tukey.coating[start.index.coating:end.index.coating, ] <- tukey.result.coating
    
    measurement.names.coating <- rownames(well.data)
    all.row.names.coating <- rownames(tukey.result.coating)
    measurement.ids.coating <- rep(measurement.names.coating[i], each = nlevels.coating)
    row.names.coating <- paste(all.row.names.coating, measurement.ids.coating)
    row_names_coating[start.index.coating:end.index.coating] <- row.names.coating
  } else {
    cat("Tukey's test for Coating is not available at iteration", i, "\n")
  }
}




# Set the row names for the matrices
rownames(filtered.parameter) <- rownames(well.data) #Set the same row names as in well.data
rownames(filtered.pvalue) <- rownames(well.data) #Set the same row names as in well.data
rownames(tukey.cells) <- row_names_cells 
rownames(tukey.coating) <- row_names_coating

View(tukey.cells)
View(tukey.coating)



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
asterisks.cells <- sapply(tukey.cells[, "Adjusted p-value"], add_asterisks)
asterisks.coating <- sapply(tukey.coating[, "Adjusted p-value"], add_asterisks)

# Add the asterisks as a new column to the tukey.cells and tukey.coating matrices
tukey.cells <- cbind(tukey.cells, Asterisks = asterisks.cells)
tukey.coating <- cbind(tukey.coating, Asterisks = asterisks.coating)




# Filter out rows with significant adjusted p-values --------------------------------

# Define a threshold for significant p-value
threshold <- 0.05

# Extract significant rows from tukey.cells
significant.cells <- tukey.cells[tukey.cells[, "Adjusted p-value"] < threshold, ] # Extract significant rows from tukey.cells
filtered.significant.cells <- significant.cells[!apply(is.na(significant.cells) | significant.cells == "", 1, all), ] # Filter out rows with NA values

# Extract significant rows from tukey.coating
significant.coating <- as.matrix(tukey.coating[tukey.coating[, "Adjusted p-value"] < threshold, ])
filtered.significant.coating <- (significant.coating[!apply(is.na(significant.coating) | significant.coating == "", 1, all), , drop = FALSE])

# Assign original row names
if (ncol(filtered.significant.coating) == 1) {
  filtered.significant.coating <- t(filtered.significant.coating)
}
rownames(filtered.significant.coating) <- rownames(tukey.coating)[tukey.coating[, "Adjusted p-value"] < threshold]

# View the significant matrices
View(filtered.significant.cells)
View(filtered.significant.coating)




# Save files to wd -------------------------------------------------

#write.table(well.data, file = "well.data.txt", sep = "\t", quote = FALSE) 
write.table(filtered.significant.cells, file = "filtered.significant.cells.txt", sep = "\t", quote = FALSE) 
write.table(filtered.significant.coating, file = "filtered.significant.coating.txt", sep = "\t", quote = FALSE) 


