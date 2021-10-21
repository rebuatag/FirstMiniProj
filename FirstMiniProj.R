## CMSC 197 Introduction to Data Science: First Mini Project
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - -- - - 

## Before starting I set my working directory to the directory of my data using the following command
## setwd("C:/Users/HP/Desktop/4th Year 1st Sem/CMSC197 [Intro to Data Science]/First Mini Project")
## Then, I source it to my R code by entering source("FirstMiniProj.R") in the console

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - -- - - 
## Problem #1
    ## Description :
        ## pollutantmean -> calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
        ## 3 arguments : 
            ## directory -> character vector of length 1 indicating the location of the csv files (specdata)
            ## pollutant -> character vector of length 1 indicating the name of pollutant for which we will calculate the mean;
            ##           -> either sulfate or nitrate
            ## id        -> integer vector indicating the monitor ID numbers to be used  
        ## reads that monitors particulate matter data from the specified directory argument.
        ## returns the means of the pollutant across all of the monitors, ignoring any missing values coded as NA.
    ## To run:
        ## Make sure that you are in the working directory of where the data is.
        ## Enter pollutantmean("specdata", "sulfate", 1:10) to generate the mean of the pollutant
            ## pollutantmean() takes 3 argument:
                ## 1st, directory of data; 2nd, pollutant(either sulfate or nitrate); 3rd, monitor ID/s

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## List of csv files to monitor from the directory, specdata
    ## pattern attribute will only return files names with matching .csv format
    ## full.names attribute = TRUE will return the full path of the files (directory path + file name)
    files_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE) 
    
    ## Empty data frame for storing later
    datas <- data.frame()
    
    ## Loop through the list of files using the monitor ID specified 
    for (i in id) {
        
        ## read the file of corresponding ID
        read_data <- read.csv(files_list[i])
        
        ## Removing data from the file with NA values
        ## na.omit() returns the object with incomplete cases removed
        read_data <- na.omit(read_data)
        
        ## row binding of files to the datas data frame
        datas <- rbind(datas, read_data)
    }
    
    ## mean would compute the mean of the pollutant from the data frame datas
    ## [[]] would return the column with specified pollutant parameter from the data frame
    mean(datas[[pollutant]])
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - -- - - 

## Problem #2
    ## Description :
        ## complete      -> reads a directory full of files and reports the number of completely observed cases in each data file
        ## 2 arguments   :
            ## directory -> character vector of length 1 indicating the location of the csv files (specdata)
            ## id        -> integer vector indicating the monitor ID numbers to be used 
        ## return a data frame ; the first column : name of the file, and the second column : number of complete cases
    ## To run: 
        ## Make sure that you are in the working directory of where the data is.
        ## Enter complete("specdata", 1) in the console to get the no. of complete observation case/s of a specific monitor ID
            ## complete() function takes 2 argument:
                ## 1st: directory of data; 2nd: monitor id to be used

complete <- function(directory, id = 1:332) {
    
    ## List of monitor files from the directory, specdata
    files_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)     
    
    ## Empty data frame for storing the number of completely observed cases
    complete_cases <- data.frame()
    
    ## Loop through the list of files using the ID specified 
    for (i in id) {
        
        ## read the file of corresponding ID
        read_file <- read.csv(files_list[i], header = TRUE)
        
        ## Delete rows that have incomplete cases
        ## na.omit() returns the object with incomplete cases removed
        read_file <- na.omit(read_file)
        
        ## Count rows with complete cases and store it in nobs
        nobs <- nrow(read_file)
        
        ## Enumerate cases by index i, which is based on the id parameter
        ## Store in complete cases dataframe the cases that are complete
        ## data.frame creates a data frame with monitor ID and no. of complete cases
        ## rbind binds the cases row wise
        complete_cases <- rbind(complete_cases, data.frame(i, nobs))
    }
    
    ## return the data frame of complete cases
    complete_cases
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - -- - - 

## Problem #3
    ## Description :
        ## corr         -> calculates the correlation between sulfate and nitrate for monitor locations where the number of 
        ##                 completely observed cases (on all variables) is greater than the threshold
        ## 2 arguments  :
            ## directory-> character vector of length 1 indicating the location of the csv files (specdata)
            ## threshold-> numeric vector of length 1 indicating the no. of completely observed observations (on all variables)
            ##             required to compute the correlation between nitrate and sulfate; default value = 0
        ## return a vector of correlations for the monitors that meet the threshold requirement. 
        ## If no monitors meet thw threshold requirement, then the function should return a numeric vector of length 0. 
    ## To run: 
        ## Make sure that you are in the working directory of where the data is.
        ## Assign corr function to a variable : 
                ## cr <- corr("specdata", 150) ; 
                ## corr function takes 2 argument; 1st, directory of data; 2nd, threshold value
        ## Enter head(cr) to generate the first lines of the function
        ## Enter summary(cr) to get the summary statistics of the function
        ## Enter length(cr) to get the length of the vector

corr <- function(directory, threshold = 0) {

    ## List of files to monitor from the directory, specdata
    files_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)  
    
    ## Empty vector for storing values used in correlation analysis
    correlation <- vector(mode = "numeric", length = 0)
    
    ## Looping through files
    for(i in 1:length(files_list)){
        
        ## Read the file of corresponding index
        read_file <- read.csv(files_list[i], header = TRUE)
        
        ## Removing incomplete cases
        ## na.omit() returns the object with incomplete cases removed
        read_file <- na.omit(read_file)
        
        ## count the no of rows of complete observation cases
        ## if it is greater than the threshold
        if(nrow(read_file) > threshold){
            
            ## Compute the correlation of the pollutants then store in a vector
            ## Since read_file is a data frame, we use [[]] to get the pollutants
            correlation <- c(correlation,cor(read_file[["sulfate"]], read_file[["nitrate"]]))
        }
    }
    
    ## return the result vector
    correlation
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - -- - - 

## Problem #4
    ## Description :
        ## plot the 30-day mortality rates for heart attack given the dataset outcome-of-care-measures.csv
    ## To run: 
        ## use setwd() to change the working directory to where the data is.
        ## Enter hospital() in console to display the histogram.

hospital <- function() {
    
    ## Read the data from csv file as characters
    outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    
    ## Coerce the column to be numeric
    ## By coercing it, warning about NAs are introduced because of missing values
    outcome[, 11] <- as.numeric(outcome[, 11])
    
    ## Plot the histogram with values from outcome[, 11]
    ## main is the attribute of main title
    ## xlab is the attribute for the x-axis label
    ## col is the attribute for the color to be used to fill the bars
    hist(outcome[, 11],
         main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
         xlab = "Deaths",
         col = "light blue")
}
