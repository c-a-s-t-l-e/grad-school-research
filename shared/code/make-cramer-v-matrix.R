# Cramer's V allows you to compare two categorical items by giving a measure between 0 and 1.
# 1 means the strongest relationship you could possibly have, 0 means there's absolutely nothing there.

##################################################################################################################
# Libraries
##################################################################################################################
library(vcd)
library(corrplot)
library(tidyverse)

##################################################################################################################
# Cramer's V Function
##################################################################################################################

# Safer version of Cramér’s V
cramerV <- function(x, y) {
  tab <- table(x, y)
  if (nrow(tab) < 2 || ncol(tab) < 2) { # this only allows for items that occur together at least 2 or more times
    return(NA)   # if there's not enough variation, return NA
  } else {
    return(suppressWarnings(assocstats(tab)$cramer)) # otherwise, get the score for the two items
  }
}

##################################################################################################################
# Make a Matrix from the Cramer's V Scores

# Select which categorical columns you would like to get from the dataset
all_cats <- crash_data_mca %>% select(-c(City, State, Model, Make, `Model Year`, Vehicle)) # all as factor

# Build Cramér’s V matrix that consists of the items being on the x and y axes
mat <- outer(names(all_cats), names(all_cats),
             Vectorize(function(a, b) cramerV(all_cats[[a]], all_cats[[b]])))

# Set all the values that run along the diagonal to 1 since those items line up perfectly with themselves
diag(mat) <- 1

# Add row/col names for a better graph
rownames(mat) <- colnames(all_cats)
colnames(mat) <- colnames(all_cats)

##################################################################################################################
# Plot the Cramer's V Scores
##################################################################################################################

# Threshold to show only the most revelevant pairs
threshold <- 0.3

# Copy matrix
mat_high <- mat

# Set weak associations to NA so that they don't show any color in the matrix plot
mat_high[mat_high < threshold] <- NA

# Heatmap with only strong associations
corrplot(mat_high, method = "color", is.corr = FALSE,
         title = paste("Cramér’s V ≥", threshold),
         mar = c(0,0,2,0),
         tl.col = "black", tl.cex = 0.6, tl.srt = 45,
         na.label = " ")   # blank out the NAs

