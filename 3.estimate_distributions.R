# This program takes the cleaned FMLA data and calculates a number of
# distirbutions of leave length.

library("dplyr")
library("survey")

df <- read.csv("fmla_clean_2012.csv")

# ---------------------------------------------------------------------------------------------------------
# 0. Define Functions
# ---------------------------------------------------------------------------------------------------------

# Function to calculate length distribution
getDist <- function(y){
  
  des <- svydesign(id = ~1,  weights = as.formula("~ fixed_weight"), data = df %>% filter_(y))
  tbl <- svytable(as.formula("~ length"),des)
  
  return(cumsum(tbl)/sum(tbl))
}

# Function to run Estimates
runEstimate <- function(y,lname){
  
  # Apply function to all sub-types
  dists <- mapply(getDist, y = y, SIMPLIFY = FALSE)
  
  # Save
  for (i in 1:length(dists)){
    data_frame(length=names(dists[[i]]),cdf=dists[[i]]) %>%
      write.csv(paste0("./estimates/length_",lname,"_",names(dists[i]),".csv"),row.names=F)
  }
}

# ---------------------------------------------------------------------------------------------------------
# 1. Own health
# ---------------------------------------------------------------------------------------------------------

# subsetting data
conditional <- c(noprog = "take_own==1 & recStatePay==0",
                 prog =   "take_own==1 & recStatePay==1")

# Run Estimation
runEstimate(conditional,"ownhealth")

# ---------------------------------------------------------------------------------------------------------
# 2. Ill Child
# ---------------------------------------------------------------------------------------------------------

# subsetting data
conditional <- c(male = "take_illchild==1 & female==0",
                 female = "take_illchild==1 & female==1")

# Run Estimation
runEstimate(conditional,"illchild")

# ---------------------------------------------------------------------------------------------------------
# 3. Ill Parent
# ---------------------------------------------------------------------------------------------------------

# subsetting data
conditional <- c(male = "take_illparent==1 & female==0",
                 female = "take_illparent==1 & female==1")

# Run Estimation
runEstimate(conditional,"illparent")

# ---------------------------------------------------------------------------------------------------------
# 4. Ill Spouse
# ---------------------------------------------------------------------------------------------------------

# subsetting data
conditional <- c(male = "take_illspouse==1 & female==0",
                 female = "take_illspouse==1 & female==1")

# Run Estimation
runEstimate(conditional,"illspouse")

# ---------------------------------------------------------------------------------------------------------
# 5. Maternity/Disability
# ---------------------------------------------------------------------------------------------------------

# subsetting data
conditional <- c(only = "take_matdis==1")

# Run Estimation
runEstimate(conditional,"maternity")

# ---------------------------------------------------------------------------------------------------------
# 6. New Child
# ---------------------------------------------------------------------------------------------------------

# subsetting data
conditional <- c(male = "take_bond==1 & female==0",
                 female = "take_bond==1 & female==1")

# Run Estimation
runEstimate(conditional,"newchild")
