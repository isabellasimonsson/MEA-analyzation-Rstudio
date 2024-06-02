library(ggplot2)
library(stringr)

#Load data
setwd("~/Documents/Chalmers/År 5/Exjobb/MEA-analys/Data/Experiment 2/Results/D87")
filtered.data.for.plotting.conditions <- read.table('filtered.data.for.plotting.conditions.txt',sep='\t',header=TRUE, check.names = FALSE)
combined.metadata <- read.table('combined.metadata.txt',sep='\t',header=TRUE, check.names = FALSE)
filtered.significant.condition <- read.table('filtered.significant.condition.txt',sep='\t',header=TRUE, check.names = FALSE)



# Number of wells per condition -------------------------------------------
#Prepare the data for plotting
number.of.wells.per.condition <- t(filtered.data.for.plotting.conditions["Number of wells per condition",])

# Convert to data frame
number.of.wells.per.condition <- as.data.frame(number.of.wells.per.condition)
# Add a column for conditions
number.of.wells.per.condition$Condition <- rownames(number.of.wells.per.condition)
# Reset row names
rownames(number.of.wells.per.condition) <- NULL
# Rename columns for clarity
colnames(number.of.wells.per.condition) <- c("Electrodes", "Condition")
# Convert Mean and SD columns to numeric
number.of.wells.per.condition$Electrodes <- as.numeric(as.character(number.of.wells.per.condition$Electrodes))



# Extract relevant p-values
filtered.wells.per.condition <- filtered.significant.condition[grepl("Number of wells per condition", rownames(filtered.significant.condition)), 
                                                              c("Adjusted p-value", "Asterisks")]


#Find the compared conditions in Tukey's test results
pattern <- "((\\b\\d+k\\b|LN521|PLO \\+ LN521|NMM|BP|Psilocin \\+ NMM)\\s?(,\\s?)?){3}"
# Extract conditions from filtered.wells.per.condition
conditions <- str_extract_all(rownames(filtered.wells.per.condition), pattern)

# Add columns for Condition 1 and Condition 2
filtered.wells.per.condition$Condition1 <- sapply(conditions, `[`, 1)
filtered.wells.per.condition$Condition2 <- sapply(conditions, `[`, 2)

# Remove leading and trailing whitespace from Condition1 and Condition2 columns
filtered.wells.per.condition$Condition1 <- trimws(filtered.wells.per.condition$Condition1)
filtered.wells.per.condition$Condition2 <- trimws(filtered.wells.per.condition$Condition2)

# Merge the mean values for Condition1 without sorting
filtered.wells.per.condition <- merge(filtered.wells.per.condition, number.of.wells.per.condition, by.x = "Condition1", by.y = "Condition", all.x = TRUE, sort = FALSE)
# Merge the mean values for Condition2 without sorting
filtered.wells.per.condition <- merge(filtered.wells.per.condition, number.of.wells.per.condition, by.x = "Condition2", by.y = "Condition", all.x = TRUE, sort = FALSE)





line_shift = 1
# Create a sequence of y values for the lines, shifting each line slightly higher
line_heights <- seq(min(number.of.wells.per.condition$Electrodes) + 5 + line_shift, length.out = nrow(filtered.wells.per.condition), by = line_shift)
# Get the unique conditions in the order they appear in number.of.wells.per.condition
condition_order <- unique(number.of.wells.per.condition$Condition)
as.numeric(factor(number.of.wells.per.condition$Condition))
# Factorize the levels based on the condition_order
number.of.wells.per.condition$Condition <- factor(number.of.wells.per.condition$Condition, levels = condition_order)
# Convert to numeric
as.numeric(number.of.wells.per.condition$Condition)


# Reorder factor levels based on the order of appearance in number.of.wells.per.condition$Condition
filtered.wells.per.condition$Condition1 <- factor(filtered.wells.per.condition$Condition1, levels = number.of.wells.per.condition$Condition)
filtered.wells.per.condition$Condition2 <- factor(filtered.wells.per.condition$Condition2, levels = number.of.wells.per.condition$Condition)
# Now check the numeric representation
as.numeric(filtered.wells.per.condition$Condition1)
as.numeric(filtered.wells.per.condition$Condition2)
# Recalculate the midpoint between Condition1 and Condition2 for x-position of asterisks
filtered.wells.per.condition$midpoint_x <- as.numeric(filtered.wells.per.condition$Condition1) + 
  0.5 * (as.numeric(filtered.wells.per.condition$Condition2) - as.numeric(filtered.wells.per.condition$Condition1))


# Plotting
number.of.wells.per.condition.plot <- ggplot() +
  geom_bar(data = number.of.wells.per.condition, aes(x = Condition, y = Electrodes, fill = Condition), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.wells.per.condition,
               aes(x = Condition1, xend = Condition2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.wells.per.condition,
               aes(x = Condition1, xend = Condition1, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the start
  geom_segment(data = filtered.wells.per.condition,
               aes(x = Condition2, xend = Condition2, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the end
  geom_text(data = filtered.wells.per.condition,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Number of wells per condition", x = NULL, y = "Number of wells") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.wells.per.condition.plot)


# Save the plot to the working directory
ggsave("number.of.wells.per.condition.plot.png", plot = number.of.wells.per.condition.plot, width = 10, height = 6)





# Number of covered electrodes --------------------------------------------
#Prepare the data for plotting
number.of.covered.electrodes <- t(filtered.data.for.plotting.conditions["Number of Covered Electrodes",])

# Convert to data frame
number.of.covered.electrodes <- as.data.frame(number.of.covered.electrodes)
# Add a column for conditions
number.of.covered.electrodes$Condition <- rownames(number.of.covered.electrodes)
# Reset row names
rownames(number.of.covered.electrodes) <- NULL
# Rename columns for clarity
colnames(number.of.covered.electrodes) <- c("Electrodes", "Condition")
# Convert Mean and SD columns to numeric
number.of.covered.electrodes$Electrodes <- as.numeric(as.character(number.of.covered.electrodes$Electrodes))



# Extract relevant p-values
filtered.covered.electrodes <- filtered.significant.condition[grepl("Number of Covered Electrodes", rownames(filtered.significant.condition)), 
                                                       c("Adjusted p-value", "Asterisks")]


#Find the compared conditions in Tukey's test results
pattern <- "((\\b\\d+k\\b|LN521|PLO \\+ LN521|NMM|BP|Psilocin \\+ NMM)\\s?(,\\s?)?){3}"
# Extract conditions from filtered.covered.electrodes
conditions <- str_extract_all(rownames(filtered.covered.electrodes), pattern)

# Add columns for Condition 1 and Condition 2
filtered.covered.electrodes$Condition1 <- sapply(conditions, `[`, 1)
filtered.covered.electrodes$Condition2 <- sapply(conditions, `[`, 2)

# Remove leading and trailing whitespace from Condition1 and Condition2 columns
filtered.covered.electrodes$Condition1 <- trimws(filtered.covered.electrodes$Condition1)
filtered.covered.electrodes$Condition2 <- trimws(filtered.covered.electrodes$Condition2)

# Merge the mean values for Condition1 without sorting
filtered.covered.electrodes <- merge(filtered.covered.electrodes, number.of.covered.electrodes, by.x = "Condition1", by.y = "Condition", all.x = TRUE, sort = FALSE)
# Merge the mean values for Condition2 without sorting
filtered.covered.electrodes <- merge(filtered.covered.electrodes, number.of.covered.electrodes, by.x = "Condition2", by.y = "Condition", all.x = TRUE, sort = FALSE)





line_shift = 15
# Create a sequence of y values for the lines, shifting each line slightly higher
line_heights <- seq(min(number.of.covered.electrodes$Electrodes) + 5 + line_shift, length.out = nrow(filtered.covered.electrodes), by = line_shift)
# Get the unique conditions in the order they appear in number.of.covered.electrodes
condition_order <- unique(number.of.covered.electrodes$Condition)
as.numeric(factor(number.of.covered.electrodes$Condition))
# Factorize the levels based on the condition_order
number.of.covered.electrodes$Condition <- factor(number.of.covered.electrodes$Condition, levels = condition_order)
# Convert to numeric
as.numeric(number.of.covered.electrodes$Condition)


# Reorder factor levels based on the order of appearance in number.of.covered.electrodes$Condition
filtered.covered.electrodes$Condition1 <- factor(filtered.covered.electrodes$Condition1, levels = number.of.covered.electrodes$Condition)
filtered.covered.electrodes$Condition2 <- factor(filtered.covered.electrodes$Condition2, levels = number.of.covered.electrodes$Condition)
# Now check the numeric representation
as.numeric(filtered.covered.electrodes$Condition1)
as.numeric(filtered.covered.electrodes$Condition2)
# Recalculate the midpoint between Condition1 and Condition2 for x-position of asterisks
filtered.covered.electrodes$midpoint_x <- as.numeric(filtered.covered.electrodes$Condition1) + 
  0.5 * (as.numeric(filtered.covered.electrodes$Condition2) - as.numeric(filtered.covered.electrodes$Condition1))


# Plotting
number.of.covered.electrodes.plot <- ggplot() +
  geom_bar(data = number.of.covered.electrodes, aes(x = Condition, y = Electrodes, fill = Condition), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.covered.electrodes,
               aes(x = Condition1, xend = Condition2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.covered.electrodes,
               aes(x = Condition1, xend = Condition1, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the start
  geom_segment(data = filtered.covered.electrodes,
               aes(x = Condition2, xend = Condition2, y = line_heights - 3, yend = line_heights + 3)) + # Add vertical line at the end
  geom_text(data = filtered.covered.electrodes,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5, size = 5, fontface = "bold") + # Increase size and make text bold
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Number of Covered Electrodes", x = NULL, y = "Number of Electrodes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.covered.electrodes.plot)


# Save the plot to the working directory
ggsave("number.of.covered.electrodes.plot.png", plot = number.of.covered.electrodes.plot, width = 10, height = 6)




# Number of Active Electrodes ---------------------------------------------

#Prepare the data for plotting
number.of.active.electrodes <- t(filtered.data.for.plotting.conditions["Number of Active Electrodes",])

# Convert to data frame
number.of.active.electrodes <- as.data.frame(number.of.active.electrodes)
# Add a column for conditions
number.of.active.electrodes$Condition <- rownames(number.of.active.electrodes)
# Reset row names
rownames(number.of.active.electrodes) <- NULL
# Rename columns for clarity
colnames(number.of.active.electrodes) <- c("Electrodes", "Condition")
# Convert Mean and SD columns to numeric
number.of.active.electrodes$Electrodes <- as.numeric(as.character(number.of.active.electrodes$Electrodes))



# Extract relevant p-values
filtered.active.electrodes <- filtered.significant.condition[grepl("Number of Active Electrodes", rownames(filtered.significant.condition)), 
                                                              c("Adjusted p-value", "Asterisks")]


#Find the compared conditions in Tukey's test results
pattern <- "((\\b\\d+k\\b|LN521|PLO \\+ LN521|NMM|BP|Psilocin \\+ NMM)\\s?(,\\s?)?){3}"
# Extract conditions from filtered.active.electrodes
conditions <- str_extract_all(rownames(filtered.active.electrodes), pattern)

# Add columns for Condition 1 and Condition 2
filtered.active.electrodes$Condition1 <- sapply(conditions, `[`, 1)
filtered.active.electrodes$Condition2 <- sapply(conditions, `[`, 2)

# Remove leading and trailing whitespace from Condition1 and Condition2 columns
filtered.active.electrodes$Condition1 <- trimws(filtered.active.electrodes$Condition1)
filtered.active.electrodes$Condition2 <- trimws(filtered.active.electrodes$Condition2)

# Merge the mean values for Condition1 without sorting
filtered.active.electrodes <- merge(filtered.active.electrodes, number.of.active.electrodes, by.x = "Condition1", by.y = "Condition", all.x = TRUE, sort = FALSE)
# Merge the mean values for Condition2 without sorting
filtered.active.electrodes <- merge(filtered.active.electrodes, number.of.active.electrodes, by.x = "Condition2", by.y = "Condition", all.x = TRUE, sort = FALSE)





line_shift = 1.5
# Create a sequence of y values for the lines, shifting each line slightly higher
line_heights <- seq(min(number.of.active.electrodes$Electrodes) + 1 + line_shift, length.out = nrow(filtered.active.electrodes), by = line_shift)
# Get the unique conditions in the order they appear in number.of.active.electrodes
condition_order <- unique(number.of.active.electrodes$Condition)
as.numeric(factor(number.of.active.electrodes$Condition))
# Factorize the levels based on the condition_order
number.of.active.electrodes$Condition <- factor(number.of.active.electrodes$Condition, levels = condition_order)
# Convert to numeric
as.numeric(number.of.active.electrodes$Condition)


# Reorder factor levels based on the order of appearance in number.of.active.electrodes$Condition
filtered.active.electrodes$Condition1 <- factor(filtered.active.electrodes$Condition1, levels = number.of.active.electrodes$Condition)
filtered.active.electrodes$Condition2 <- factor(filtered.active.electrodes$Condition2, levels = number.of.active.electrodes$Condition)
# Now check the numeric representation
as.numeric(filtered.active.electrodes$Condition1)
as.numeric(filtered.active.electrodes$Condition2)
# Recalculate the midpoint between Condition1 and Condition2 for x-position of asterisks
filtered.active.electrodes$midpoint_x <- as.numeric(filtered.active.electrodes$Condition1) + 
  0.5 * (as.numeric(filtered.active.electrodes$Condition2) - as.numeric(filtered.active.electrodes$Condition1))


# Plotting
number.of.active.electrodes.plot <- ggplot() +
  geom_bar(data = number.of.active.electrodes, aes(x = Condition, y = Electrodes, fill = Condition), stat = "identity", show.legend = FALSE) +
  geom_segment(data = filtered.active.electrodes,
               aes(x = Condition1, xend = Condition2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.active.electrodes,
               aes(x = Condition1, xend = Condition1, y = line_heights - 0.5, yend = line_heights + 0.5)) + # Add vertical line at the start
  geom_segment(data = filtered.active.electrodes,
               aes(x = Condition2, xend = Condition2, y = line_heights - 0.5, yend = line_heights + 0.5)) + # Add vertical line at the end
  geom_text(data = filtered.active.electrodes,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5) + # Move asterisks above the lines
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Number of Active Electrodes", x = NULL, y = "Number of Electrodes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.active.electrodes.plot)


# Save the plot to the working directory
ggsave("number.of.active.electrodes.plot.png", plot = number.of.active.electrodes.plot, width = 10, height = 6)



# Number of spikes --------------------------------------------------------

#Prepare the data for plotting
number.of.spikes <- t(rbind(filtered.data.for.plotting.conditions["Mean Number of Spikes per well",],
                            filtered.data.for.plotting.conditions["Standard deviation Number of Spikes per well",]))

# Convert to data frame
number.of.spikes <- as.data.frame(number.of.spikes)
# Add a column for conditions
number.of.spikes$Condition <- rownames(number.of.spikes)
# Reset row names
rownames(number.of.spikes) <- NULL
# Rename columns for clarity
colnames(number.of.spikes) <- c("Mean", "SD", "Condition")
# Convert Mean and SD columns to numeric
number.of.spikes$Mean <- as.numeric(as.character(number.of.spikes$Mean))
number.of.spikes$SD <- as.numeric(as.character(number.of.spikes$SD))


# Extract relevant p-values
filtered.mean.spikes <- filtered.significant.condition[grepl("Mean Number of Spikes", rownames(filtered.significant.condition)), 
                                                          c("Adjusted p-value", "Asterisks")]


#Find the compared conditions in Tukey's test results
pattern <- "((\\b\\d+k\\b|LN521|PLO \\+ LN521|NMM|BP|Psilocin \\+ NMM)\\s?(,\\s?)?){3}"
# Extract conditions from filtered.mean.spikes
conditions <- str_extract_all(rownames(filtered.mean.spikes), pattern)

# Add columns for Condition 1 and Condition 2
filtered.mean.spikes$Condition1 <- sapply(conditions, `[`, 1)
filtered.mean.spikes$Condition2 <- sapply(conditions, `[`, 2)

# Remove leading and trailing whitespace from Condition1 and Condition2 columns
filtered.mean.spikes$Condition1 <- trimws(filtered.mean.spikes$Condition1)
filtered.mean.spikes$Condition2 <- trimws(filtered.mean.spikes$Condition2)

# Merge the mean values for Condition1 without sorting
filtered.mean.spikes <- merge(filtered.mean.spikes, number.of.spikes, by.x = "Condition1", by.y = "Condition", all.x = TRUE, sort = FALSE)
# Merge the mean values for Condition2 without sorting
filtered.mean.spikes <- merge(filtered.mean.spikes, number.of.spikes, by.x = "Condition2", by.y = "Condition", all.x = TRUE, sort = FALSE)






line_shift = 240
# Create a sequence of y values for the lines, shifting each line slightly higher
line_heights <- seq(min(number.of.spikes$Mean) + 350 + line_shift, length.out = nrow(filtered.mean.spikes), by = line_shift)
# Get the unique conditions in the order they appear in number.of.spikes
condition_order <- unique(number.of.spikes$Condition)
as.numeric(factor(number.of.spikes$Condition))
# Factorize the levels based on the condition_order
number.of.spikes$Condition <- factor(number.of.spikes$Condition, levels = condition_order)
# Convert to numeric
as.numeric(number.of.spikes$Condition)


# Reorder factor levels based on the order of appearance in number.of.spikes$Condition
filtered.mean.spikes$Condition1 <- factor(filtered.mean.spikes$Condition1, levels = number.of.spikes$Condition)
filtered.mean.spikes$Condition2 <- factor(filtered.mean.spikes$Condition2, levels = number.of.spikes$Condition)
# Now check the numeric representation
as.numeric(filtered.mean.spikes$Condition1)
as.numeric(filtered.mean.spikes$Condition2)
# Recalculate the midpoint between Condition1 and Condition2 for x-position of asterisks
filtered.mean.spikes$midpoint_x <- as.numeric(filtered.mean.spikes$Condition1) + 
  0.5 * (as.numeric(filtered.mean.spikes$Condition2) - as.numeric(filtered.mean.spikes$Condition1))


# Plotting
number.of.spikes.plot <- ggplot() +
  geom_bar(data = number.of.spikes, aes(x = Condition, y = Mean, fill = Condition), stat = "identity", show.legend = FALSE) +
  geom_errorbar(data = number.of.spikes, aes(x = Condition, ymin = Mean - SD, ymax = Mean + SD), width = 0.4) +
  geom_segment(data = filtered.mean.spikes,
               aes(x = Condition1, xend = Condition2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.mean.spikes,
               aes(x = Condition1, xend = Condition1, y = line_heights - 25, yend = line_heights + 25)) + # Add vertical line at the start
  geom_segment(data = filtered.mean.spikes,
               aes(x = Condition2, xend = Condition2, y = line_heights - 25, yend = line_heights + 25)) + # Add vertical line at the end
  geom_text(data = filtered.mean.spikes,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5) + # Move asterisks above the lines
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Number of Spikes per Well", x = NULL, y = "Number of Spikes") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(number.of.spikes.plot)


# Save the plot to the working directory
ggsave("number.of.spikes.plot.png", plot = number.of.spikes.plot, width = 10, height = 6)




# Firing rate -------------------------------------------------------------

#Prepare the data for plotting
firing.rate <- t(rbind(filtered.data.for.plotting.conditions["Mean Firing Rate (Hz) per well",],
                            filtered.data.for.plotting.conditions["Standard deviation Firing Rate (Hz) per well",]))

# Convert to data frame
firing.rate <- as.data.frame(firing.rate)
# Add a column for conditions
firing.rate$Condition <- rownames(firing.rate)
# Reset row names
rownames(firing.rate) <- NULL
# Rename columns for clarity
colnames(firing.rate) <- c("Mean", "SD", "Condition")
# Convert Mean and SD columns to numeric
firing.rate$Mean <- as.numeric(as.character(firing.rate$Mean))
firing.rate$SD <- as.numeric(as.character(firing.rate$SD))


# Extract relevant p-values
filtered.mean.firing.rate <- filtered.significant.condition[grepl("Mean Mean Firing Rate", rownames(filtered.significant.condition)), 
                                                       c("Adjusted p-value", "Asterisks")]


#Find the compared conditions in Tukey's test results
pattern <- "((\\b\\d+k\\b|LN521|PLO \\+ LN521|NMM|BP|Psilocin \\+ NMM)\\s?(,\\s?)?){3}"
# Extract conditions from filtered.mean.firing.rate
conditions <- str_extract_all(rownames(filtered.mean.firing.rate), pattern)

# Add columns for Condition 1 and Condition 2
filtered.mean.firing.rate$Condition1 <- sapply(conditions, `[`, 1)
filtered.mean.firing.rate$Condition2 <- sapply(conditions, `[`, 2)

# Remove leading and trailing whitespace from Condition1 and Condition2 columns
filtered.mean.firing.rate$Condition1 <- trimws(filtered.mean.firing.rate$Condition1)
filtered.mean.firing.rate$Condition2 <- trimws(filtered.mean.firing.rate$Condition2)

# Merge the mean values for Condition1 without sorting
filtered.mean.firing.rate <- merge(filtered.mean.firing.rate, firing.rate, by.x = "Condition1", by.y = "Condition", all.x = TRUE, sort = FALSE)
# Merge the mean values for Condition2 without sorting
filtered.mean.firing.rate <- merge(filtered.mean.firing.rate, firing.rate, by.x = "Condition2", by.y = "Condition", all.x = TRUE, sort = FALSE)






line_shift = 0.2
# Create a sequence of y values for the lines, shifting each line slightly higher
line_heights <- seq(min(firing.rate$Mean) + 0.3 + line_shift, length.out = nrow(filtered.mean.firing.rate), by = line_shift)
# Get the unique conditions in the order they appear in firing.rate
condition_order <- unique(firing.rate$Condition)
as.numeric(factor(firing.rate$Condition))
# Factorize the levels based on the condition_order
firing.rate$Condition <- factor(firing.rate$Condition, levels = condition_order)
# Convert to numeric
as.numeric(firing.rate$Condition)


# Reorder factor levels based on the order of appearance in firing.rate$Condition
filtered.mean.firing.rate$Condition1 <- factor(filtered.mean.firing.rate$Condition1, levels = firing.rate$Condition)
filtered.mean.firing.rate$Condition2 <- factor(filtered.mean.firing.rate$Condition2, levels = firing.rate$Condition)
# Now check the numeric representation
as.numeric(filtered.mean.firing.rate$Condition1)
as.numeric(filtered.mean.firing.rate$Condition2)
# Recalculate the midpoint between Condition1 and Condition2 for x-position of asterisks
filtered.mean.firing.rate$midpoint_x <- as.numeric(filtered.mean.firing.rate$Condition1) + 
  0.5 * (as.numeric(filtered.mean.firing.rate$Condition2) - as.numeric(filtered.mean.firing.rate$Condition1))


# Plotting
firing.rate.plot <- ggplot() +
  geom_bar(data = firing.rate, aes(x = Condition, y = Mean, fill = Condition), stat = "identity", show.legend = FALSE) +
  geom_errorbar(data = firing.rate, aes(x = Condition, ymin = Mean - SD, ymax = Mean + SD), width = 0.4) +
  geom_segment(data = filtered.mean.firing.rate,
               aes(x = Condition1, xend = Condition2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.mean.firing.rate,
               aes(x = Condition1, xend = Condition1, y = line_heights - 0.05, yend = line_heights + 0.05)) + # Add vertical line at the start
  geom_segment(data = filtered.mean.firing.rate,
               aes(x = Condition2, xend = Condition2, y = line_heights - 0.05, yend = line_heights + 0.05)) + # Add vertical line at the end
  geom_text(data = filtered.mean.firing.rate,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5) + # Move asterisks above the lines
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Firing Rate per Well", x = NULL, y = "Firing Rate (Hz)") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(firing.rate.plot)


# Save the plot to the working directory
ggsave("firing.rate.plot.png", plot = firing.rate.plot, width = 10, height = 6)









# Resistance --------------------------------------------------------------

#Prepare the data for plotting
resistance <- t(rbind(filtered.data.for.plotting.conditions["Mean Resistance (kOhms) per electrode",],
                            filtered.data.for.plotting.conditions["Standard deviation Resistance (kOhms) per electrode",]))

# Convert to data frame
resistance <- as.data.frame(resistance)
# Add a column for conditions
resistance$Condition <- rownames(resistance)
# Reset row names
rownames(resistance) <- NULL
# Rename columns for clarity
colnames(resistance) <- c("Mean", "SD", "Condition")
# Convert Mean and SD columns to numeric
resistance$Mean <- as.numeric(as.character(resistance$Mean))
resistance$SD <- as.numeric(as.character(resistance$SD))


# Extract relevant p-values
filtered.mean.resistance <- filtered.significant.condition[grepl("Mean Resistance", rownames(filtered.significant.condition)), 
                                                       c("Adjusted p-value", "Asterisks")]


#Find the compared conditions in Tukey's test results
pattern <- "((\\b\\d+k\\b|LN521|PLO \\+ LN521|NMM|BP|Psilocin \\+ NMM)\\s?(,\\s?)?){3}"
# Extract conditions from filtered.mean.resistance
conditions <- str_extract_all(rownames(filtered.mean.resistance), pattern)

# Add columns for Condition 1 and Condition 2
filtered.mean.resistance$Condition1 <- sapply(conditions, `[`, 1)
filtered.mean.resistance$Condition2 <- sapply(conditions, `[`, 2)

# Remove leading and trailing whitespace from Condition1 and Condition2 columns
filtered.mean.resistance$Condition1 <- trimws(filtered.mean.resistance$Condition1)
filtered.mean.resistance$Condition2 <- trimws(filtered.mean.resistance$Condition2)

# Merge the mean values for Condition1 without sorting
filtered.mean.resistance <- merge(filtered.mean.resistance, resistance, by.x = "Condition1", by.y = "Condition", all.x = TRUE, sort = FALSE)
# Merge the mean values for Condition2 without sorting
filtered.mean.resistance <- merge(filtered.mean.resistance, resistance, by.x = "Condition2", by.y = "Condition", all.x = TRUE, sort = FALSE)






line_shift = 5
# Create a sequence of y values for the lines, shifting each line slightly higher
line_heights <- seq(max(resistance$Mean) +2 + line_shift, length.out = nrow(filtered.mean.resistance), by = line_shift)
# Get the unique conditions in the order they appear in resistance
condition_order <- unique(resistance$Condition)
as.numeric(factor(resistance$Condition))
# Factorize the levels based on the condition_order
resistance$Condition <- factor(resistance$Condition, levels = condition_order)
# Convert to numeric
as.numeric(resistance$Condition)


# Reorder factor levels based on the order of appearance in resistance$Condition
filtered.mean.resistance$Condition1 <- factor(filtered.mean.resistance$Condition1, levels = resistance$Condition)
filtered.mean.resistance$Condition2 <- factor(filtered.mean.resistance$Condition2, levels = resistance$Condition)
# Now check the numeric representation
as.numeric(filtered.mean.resistance$Condition1)
as.numeric(filtered.mean.resistance$Condition2)
# Recalculate the midpoint between Condition1 and Condition2 for x-position of asterisks
filtered.mean.resistance$midpoint_x <- as.numeric(filtered.mean.resistance$Condition1) + 
  0.5 * (as.numeric(filtered.mean.resistance$Condition2) - as.numeric(filtered.mean.resistance$Condition1))


# Plotting
resistance.plot <- ggplot() +
  geom_bar(data = resistance, aes(x = Condition, y = Mean, fill = Condition), stat = "identity", show.legend = FALSE) +
  geom_errorbar(data = resistance, aes(x = Condition, ymin = Mean - SD, ymax = Mean + SD), width = 0.4) +
  geom_segment(data = filtered.mean.resistance,
               aes(x = Condition1, xend = Condition2, y = line_heights, yend = line_heights)) +
  geom_segment(data = filtered.mean.resistance,
               aes(x = Condition1, xend = Condition1, y = line_heights - 1, yend = line_heights + 1)) + # Add vertical line at the start
  geom_segment(data = filtered.mean.resistance,
               aes(x = Condition2, xend = Condition2, y = line_heights - 1, yend = line_heights + 1)) + # Add vertical line at the end
  geom_text(data = filtered.mean.resistance,
            aes(x = midpoint_x, y = line_heights, label = Asterisks),
            vjust = -0.5) + # Move asterisks above the lines
  scale_x_discrete(labels = function(x) x) + # Keep original x-axis labels
  labs(title = "Mean Resistance per Covered Electrode", x = NULL, y = "Resistance (kOhms)") + # Update axis labels
  theme_minimal() + # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 1, size = 16, face = "bold"), # Increase size and make bold
        axis.text.y = element_text(size = 16, face = "bold"), # Increase size and make bold
        axis.title.x = element_text(size = 14, face = "bold"), # Increase size and make bold
        axis.title.y = element_text(size = 20, face = "bold"), # Increase size and make bold
        plot.title = element_text(size = 24, face = "bold")) + # Increase size and make bold
  theme(legend.position = "none") # Remove legend

# Print the plot
print(resistance.plot)


# Save the plot to the working directory
ggsave("resistance.plot.png", plot = resistance.plot, width = 10, height = 6)

