##' ---
##' title: "Customized Conjoint Analysis"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "html_document"
##' ---

##' Housekeeping
## Install and load the 'cjoint' package
require('cjoint')

## Check and set your working directory (Uncomment for your system)
getwd()  # Get the current working directory
# setwd("C:/ENTER/YOUR/FOLDER/PATH/HERE")  # For PC (Note the forward slashes!)
# setwd("~/ENTER/YOUR/FOLDER/PATH/HERE")  # For Mac OSX 

##' CUSTOMIZE THIS PART FOR EACH PROJECT
## 1. ENTER your Qualtrics file name (exactly as it appears in the quotes)
qualtrics <- "ENTER YOUR FILE NAME HERE"  
data.name <- paste(qualtrics, "csv", sep = ".")  # Create the full data name (with .csv)

## Load the raw .csv from Qualtrics
df.raw <- read.csv(data.name, header = FALSE, check.names = TRUE)

## Fix the variable names and make sure first 2 rows match
df.raw[2, ] <- df.raw[1, ]
colnames(df.raw) = as.character(unlist(df.raw[1, ]))
df.raw = df.raw[-1, ]

## Print the variable names
names(df.raw)

## 2. Create a new unique respondent identifier
counter <- nrow(df.raw) - 1
df.raw$cj.id <- seq(from = 0, to = counter, by = 1)
df.raw$cj.id[1] <- "cj.id"

## 3. ENTER the number of different conjoint experiments (default is 1)
cj.exp <- 1  # CHANGE if needed 

## 4. ENTER respondent preferences from each conjoint experiment
cj.prefs <- vector(mode="list", cj.exp)
cj.prefs[[1]] <- c("ENTER", "ENTER", "ENTER")  # Conjoint 1, replace 'ENTER' with each variable name

## IF YOU HAVE MORE THAN 1 CONJOINT EXPERIMENT, THEN
## uncomment and run 'cj.prefs[[2]]', 'cj.prefs[[3]]', etc.
## (each conjoint experiment should have 'cj.prefs[[i]]' object)
# cj.prefs[[2]] <- c("ENTER", "ENTER", "ENTER")  

## 5. (Optional) ENTER variables for sub-analysis by groups (Segmentation)
# cj.segments <- c("ENTER", "ENTER", "ENTER")  # Uncomment and replace 'ENTER' with each variable name

## 6. SET the baseline category for each attribute
## (Must match EXACTLY as how it is written in the .php file, 
## including spaces and capitalization)
for (j in 1:cj.exp){
    ## Display attributes from each conjoint experiment
    x <- paste("^", paste(rep("F", j), collapse = ""), "-1-1", "$", sep = "")
    y <- grep(x, names(df.raw))
    cat("Conjoint", j, "\n")
    print(levels(df.raw[, y]))
}

baselines <- vector(mode="list", cj.exp)  # Create an empty list to store the names

baselines[[1]]$`ENTER ATTRIBUTE HERE` <- "ENTER BASELINE VALUE HERE"  # Uncomment and run

## IF YOU HAVE MORE THAN 1 CONJOINT EXPERIMENT, THEN
## (each conjoint experiment should have its own baselines object)
# baselines[[2]]$`ENTER ATTRIBUTE HERE` <- "ENTER BASELINE VALUE HERE"  # Uncomment and run

## 7. ENTER the conjoint attributes from the .php file (see #6 above)
## We'll use this to build the model for the analysis
cj.model <- vector(mode="list", cj.exp)
cj.model[[1]] <- selected ~ `ENTER` + `ENTER` + `ENTER`

## IF YOU HAVE MORE THAN 1 CONJOINT EXPERIMENT, THEN
## (each conjoint experiment should have its own cj.model object)
# cj.model[[2]] <- selected ~ `ENTER` + `ENTER` + `ENTER`  # Uncomment and run

##' Run the conjoint analysis
cj.attr <- cj.keep <- df.sub <- data.name.sub <- df <- results <- vector(mode="list", cj.exp)

for (i in 1:cj.exp) {
    ## Identify embedded variables (attributes respondents saw; default is 'F')
    z <- paste(paste("^", paste(rep("F", i), collapse = ""), sep = ""), "\\-", sep = "")
    cj.attr[[i]] <- grep(z, names(df.raw), value = TRUE)
    ## Variables to keep
    if (exists("cj.segments") == TRUE) {
        cj.keep[[i]] <- c("cj.id", cj.attr[[i]], cj.prefs[[i]], cj.segments)  
    } else {
        cj.keep[[i]] <- c("cj.id", cj.attr[[i]], cj.prefs[[i]])          
    }
    ## Subset data by kept variables
    df.sub[[i]] <- df.raw[cj.keep[[i]]]
    ## Remove missing values
    df.sub[[i]] <- na.omit(df.sub[[i]])
    ## Create a file name
    data.name.sub[[i]] <- paste(paste(qualtrics, i, sep = "_"), ".csv", sep = "")  
    ## Save the subsetted data so that it can be loaded
    write.csv(df.sub[[i]],   
              data.name.sub[[i]], 
              row.names = FALSE) 
    ## Load the subsetted data into a conjoint data frame
    if (exists("cj.segments") == TRUE) {
        df[[i]] <- read.qualtrics(data.name.sub[[i]],         # Subsetted data
                                  respondentID = "cj.id",     # Unique respondent ID
                                  responses = cj.prefs[[i]],  # Selected option
                                  covariates = "cj.segments") # Grouping variables 
    }
    else {
        df[[i]] <- read.qualtrics(data.name.sub[[i]],         # Subsetted data
                                  respondentID = "cj.id",     # Unique respondent ID
                                  responses = cj.prefs[[i]])  # Selected option
    } 
    cat("\n", "Processing Conjoint", i, "\n")
    ## Save the converted data for use with other statistical software
    write.csv(df[[i]], 
              paste(data.name.sub[[i]], "conjoint.csv", sep = "_"), 
              row.names = FALSE)
    ## Conjoint analysis - calculate AMCE estimator using all attributes in the design
    results[[i]] <- amce(data = df[[i]],                   # The conjoint dataset
                       cj.model[[i]],                         # The model
                       cluster = TRUE,                     # This should be set to TRUE
                       respondent.id = "respondentIndex",  # Respondent identifier
                       baselines = baselines[[i]])         # Use the specified baselines

    ## Print results from each conjoint experiment
    print(summary(results[[i]]))

    ## Create figures
    plot(results[[i]], 
         main = substitute(paste("Conjoint", i)),
         xlab = "Change in Predicted Probability")
}
