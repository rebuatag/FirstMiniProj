# First Mini Project (R Programming)
CMSC 197: Introduction to Data Science

### PROBLEM #1
```
Write a function named pollutantMean that calculates the mean of a pollutant (sulfate or nitrate) across 
a specified list of monitors. The function pollutantMean takes 3 argument: directory, pollutant, and id. 
Given a vector monitor ID numbers, pollutantMean reads that monitor’s particulate matter data from the 
directory specified in the directory argument and returns the means of the pollutant across all of the
monitors, ignoring any missing values coded as NA. 
```

### PROBLEM #2
```
Write a function named complete that reads a directory full of files and reports the number of completely 
observed cases in each data file. The function should return a data frame where the first column is the
name of the file, and the second column is the number of complete cases.
```

### PROBLEM #3
 ```
Write a function named corr that takes a directory of data files and a threshold for complete cases and
calculates the correlation between sulfate and nitrate for monitor locations where the number of completely 
observed cases (on all variables) is greater than the threshold. The function should return a vector of 
correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold 
requirement, then the function should return a numeric vector of length 0.
 ```

### PROBLEM #4
```
Modify the code given so that you can plot the 30-day mortality rates for heart attack.
```
```
  outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  head(outcome)
  outcome[, 11] <- as.numeric(outcome[, 11])

  ## You may get a warning about NAs being introduced
  hist(outcome[, 11])
```
