##' ---
##' title: "Automated Conjoint Analysis"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## Install and load the 'cjoint' package (versoin 2.0.4)
require(devtools)
install_version("cjoint", version = "2.0.4", repos = "http://cran.us.r-project.org")
require('cjoint')

## Check and set your working directory (Uncomment for your system)
getwd()  # Get the current working directory
# setwd("C:/ENTER/YOUR/FOLDER/PATH/HERE")  # For PC (Note the forward slashes!)
# setwd("~/ENTER/YOUR/FOLDER/PATH/HERE")  # For Mac OSX 

##' CUSTOMIZE THIS PART FOR EACH PROJECT
## 1. ENTER your Qualtrics file name (exactly as it appears without the .csv; quotes required)
qualtrics <- "ENTER YOUR FILE NAME HERE"

## 2. ENTER the number of different conjoint experiments (default = 1)
cj.exp <- 1

## 3. LIST preferences from each conjoint experiment (answer choices)
## If there is more than a single conjoint experiment, then
## uncomment and run 'cj.prefs[[2]]', 'cj.prefs[[3]]', etc.
## (each unique conjoint experiment should have its own 'cj.prefs[[i]]' object)
cj.prefs <- vector(mode = "list", cj.exp)
cj.prefs[[1]] <- c("ENTER", "ENTER", "ENTER")  # Conjoint 1, replace 'ENTER' with each variable name

## 4. LIST attributes (main categories) from each conjoint experiment
## These will serve as the predictors for our models
cj.model <- vector(mode = "list", cj.exp)
cj.model[[1]] <- selected ~ `ENTER ATTRIBUTE 1` + `ENTER ATTRIBUTE 2` + `ENTER ATTRIBUTE 3`

## 5. [OPTIONAL] LIST variables for subgroup analysis for each conjoint experiment
cj.segments <- vector(mode = "list", cj.exp)
cj.segments[[1]] <- c("ENTER", "ENTER", "ENTER")  # Uncomment and replace 'ENTER' with each variable name

## 6. SET the baseline category for each attribute
## Must match EXACTLY as how it is written in the .php file, including spaces and capitalization
## (each conjoint experiment should have its own baselines object)
baselines <- vector(mode = "list", cj.exp)  # Create an empty list to store the names
baselines[[1]]$`ENTER ATTRIBUTE 1 HERE` <- "ENTER BASELINE VALUE 1 HERE"  # Uncomment and run
baselines[[1]]$`ENTER ATTRIBUTE 1 HERE` <- "ENTER BASELINE VALUE 2 HERE"  # Uncomment and run
baselines[[1]]$`ENTER ATTRIBUTE 1 HERE` <- "ENTER BASELINE VALUE 3 HERE"  # Uncomment and run
baselines[[1]]$`ENTER ATTRIBUTE 2 HERE` <- "ENTER BASELINE VALUE 1 HERE"  # Uncomment and run
baselines[[1]]$`ENTER ATTRIBUTE 2 HERE` <- "ENTER BASELINE VALUE 2 HERE"  # Uncomment and run
baselines[[1]]$`ENTER ATTRIBUTE 2 HERE` <- "ENTER BASELINE VALUE 3 HERE"  # Uncomment and run


##' Prepare the data for analysis
data.name <- paste(qualtrics, "csv", sep = ".")  # Create the file name
df.raw <- read.csv(data.name, header = FALSE, check.names = FALSE, 
                   stringsAsFactors = FALSE, na.strings = c("", " ", "NA"))  ## Load the raw .csv

## Fix the variable names and make sure first 2 rows match
df.raw[2, ] <- df.raw[1, ]
df.raw <- df.raw[-3, ]
colnames(df.raw) = as.character(unlist(df.raw[1, ]))
df.raw = df.raw[-1, ]

## Create a new unique respondent identifier
counter <- nrow(df.raw) - 1
df.raw$cj.id <- seq(from = 0, to = counter, by = 1)
df.raw$cj.id[1] <- "cj.id"

## Create the datasets, save to unique .csv files
cj.attr <- cj.keep <- df.sub <- data.name.sub <- file.name.sub <- df <- results <- vector(mode = "list", cj.exp)

for (i in 1:cj.exp) {
    ## Identify embedded variables (attributes respondents saw; default is 'F')
    z <- paste(paste("^", paste(rep("F", i), collapse = ""), sep = ""), "\\-", sep = "")
    cj.attr[[i]] <- grep(z, names(df.raw), value = TRUE)
    ## Variables to keep
    if (length(cj.segments[[i]]) > 0) {
        cj.keep[[i]] <- c("cj.id", cj.attr[[i]], cj.prefs[[i]], cj.segments[[i]])  
    } else {
        cj.keep[[i]] <- c("cj.id", cj.attr[[i]], cj.prefs[[i]])          
    }
    ## Subset data by kept variables
    df.sub[[i]] <- df.raw[cj.keep[[i]]]
    ## Remove missing values
    df.sub[[i]] <- na.omit(df.sub[[i]])
    ## Create the file name
    data.name.sub[[i]] <- paste(qualtrics, i, sep = "_") 
    file.name.sub[[i]] <- paste(data.name.sub[[i]], ".csv", sep = "")  
    ## Save the subsetted data so that it can be loaded
    write.csv(df.sub[[i]],   
              file.name.sub[[i]], 
              row.names = FALSE) 
    ## Load the subsetted data into a conjoint data frame
    if (length(cj.segments[[i]]) > 0) {
        df[[i]] <- read.qualtrics(file.name.sub[[i]],            # Subsetted data
                                  respondentID = "cj.id",        # Unique respondent ID
                                  responses = cj.prefs[[i]],     # Selected option
                                  covariates = cj.segments[[i]]) # Grouping variables 
    }
    else {
        df[[i]] <- read.qualtrics(file.name.sub[[i]],         # Subsetted data
                                  respondentID = "cj.id",     # Unique respondent ID
                                  responses = cj.prefs[[i]])  # Selected option
    } 
    cat("\n", "Processing Conjoint", i, "\n")
    ## Save the converted long format data
    write.csv(df[[i]], 
              paste(data.name.sub[[i]], "conjoint.csv", sep = "_"), 
              row.names = FALSE)
}

##' Run the main conjoint analysis and save plots to .pdf files
for (i in 1:cj.exp) {
    ## Conjoint analysis - calculate AMCE estimator using all attributes in the design
    results[[i]] <- amce(data = df[[i]],                   # The conjoint dataset
                       cj.model[[i]],                      # The model
                       cluster = TRUE,                     # This should be set to TRUE
                       respondent.id = "respondentIndex",  # Respondent identifier
                       baselines = baselines[[i]])         # Use the specified baselines

    ## Print results from each conjoint experiment
    print(summary(results[[i]]))

    ## Create figures
    pdf(paste("figure", i, ".pdf", sep = ""), width = 21/2.54, height = 29.7/2.54)
    plot(results[[i]], 
         main = paste("Conjoint", i, sep = " ")),
         xlab = "Change in Predicted Probability")
    dev.off()
}

##' Example of how to conduct conjoint analyses on subsetted data (e.g., by demographics)
## Make a list to hold subsetted data
cj.df <- vector(mode = "list", cj.exp)

## Gender
cj.df[[1]] <- subset(df[[1]], gender == 1)  # Male
cj.df[[2]] <- subset(df[[1]], gender == 2)  # Female

head(cj.df[[1]][["gender"]])
head(cj.df[[2]][["gender"]])

## Age categories
cj.df[[3]] <- subset(df[[1]], age <= 29)  # 18-29 years old
cj.df[[4]] <- subset(df[[1]], age >= 30 & age <= 49)  # 30-49 years old
cj.df[[5]] <- subset(df[[1]], age >= 50)  # 50+ years old

## Social grade
cj.df[[6]] <- subset(df[[1]], socialgrade == 1)  # AB
cj.df[[7]] <- subset(df[[1]], socialgrade == 2)  # C1
cj.df[[8]] <- subset(df[[1]], socialgrade == 3)  # C2
cj.df[[9]] <- subset(df[[1]], socialgrade == 4)  # DE

## 2015 Vote
cj.df[[10]] <- subset(df[[1]], vote2015 == 1)  # Conservative
cj.df[[11]] <- subset(df[[1]], vote2015 == 2)  # Labour
cj.df[[12]] <- subset(df[[1]], vote2015 == 7)  # Did not vote (DK)

## Brexit vote
cj.df[[13]] <- subset(df[[1]], votebrexit == 1)  # Remain
cj.df[[14]] <- subset(df[[1]], votebrexit == 2)  # Leave

## UK Region
cj.df[[15]] <- subset(df[[1]], region2 == 7)  # London
cj.df[[16]] <- subset(df[[1]], region2 == 8 | region2 == 9)  # Rest of South
cj.df[[17]] <- subset(df[[1]], region2 >=1 & region2 <= 6)  # Rest of England
cj.df[[18]] <- subset(df[[1]], region2 == 10)  # Wales
cj.df[[19]] <- subset(df[[1]], region2 == 11)  # Scotland

## Make a list of subsetted data labels
cj.titles <- c("Males", "Females",
               "18 to 29 years old", "30 to 49 years old", "50 years and above",
               "Social grade AB", "Social grade C1", "Social grade C2", "Social grade DE",
               "Conservative", "Labour", "Did not vote",
               "Remain", "Leave",
               "London", "Rest of South", "Rest of England", "Wales", "Scotland")

subresults <- vector(mode = "list", cj.exp)

for (i in 1:19) {
    ## Conjoint analysis - calculate AMCE estimator using all attributes in the design
    subresults[[i]] <- amce(data = cj.df[[i]],               # The conjoint dataset
                         cj.model[[1]],                      # The model
                         cluster = TRUE,                     # This should be set to TRUE
                         respondent.id = "respondentIndex",  # Respondent identifier
                         baselines = baselines[[1]])         # Use the specified baselines
    
    ## Print results from each conjoint experiment
    print(summary(subresults[[i]]))
    
    ## Create and save figures
    pdf(paste("subfigure", i, ".pdf", sep = ""), width = 21/2.54, height = 29.7/2.54)
    plot(subresults[[i]], 
         main = cj.titles[[i]],
         xlab = "Change in Predicted Probability")
    dev.off()
}

