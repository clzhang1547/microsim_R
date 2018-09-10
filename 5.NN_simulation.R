
# """
# Program to create a basic k=1 Nearest Neighbor imputation of leave-taking behavior in ACS data from FMLA observations
#  
# 9 Sept 2018
# Luke
#
# """

cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("class")
library("dplyr")
library("survey")

#options(error=recover)
options(error=NULL)

d_acs <- read.csv("ACS_clean.csv")
#d_acs_raw <- read.csv("ACS_clean.csv")
d_fmla <- read.csv("fmla_clean_2012.csv")

# cleaning
d_acs$age <- d_acs$AGEP

# ---------------------------------------------------------------------------------------------------------
# 0. Define Functions
# ---------------------------------------------------------------------------------------------------------
xvars <- c("widowed", "divorced", "separated", "nevermarried", "female", 
           "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
           "white", "asian", "hisp","nochildren")

runKNN <- function(y,z) {
  # filter fmla cases
  d_temp <- d_fmla %>% filter_(z)
  d_temp <- d_temp[c(y, xvars)]
  d_temp <- d_temp %>% filter(complete.cases(.))
  
  # create labels, training data
  label <- d_temp[c(y)]
  train <- d_temp[c(xvars)]
  
  # filter out acs vars
  temp_acs <- d_acs %>% filter_(z)
  
  # create test data set 
  test <- temp_acs[c(xvars)]
  
  # estimate KNN
  estimate <- knn(train, test, as.factor(label[,y]), k=1, use.all=FALSE)
  return(data.frame(temp_acs["id"],as.numeric(estimate)-1))
}

runEstimate <- function(y,z,lname) {
  # get KNN estimates for all leave types
  estimates <- mapply(runKNN,y=y,z=z, SIMPLIFY = FALSE)
  names(estimates) <-paste(names(estimates),lname,sep= "_")
  
  # compile estimates into a single dataframe
  est_df <- data.frame(row.names=d_acs$id)
  est_df$id <- d_acs$id
  j <- 0

  for (i in estimates) {
    j <- j+1
    colnames(i) <- c("id", names(estimates)[j] )
    est_df <- merge(est_df, i, by="id", all.x=TRUE)
  }
  return(est_df)

}



 # ---------------------------------------------------------------------------------------------------------
 # 1. Probability of needing (including taking) a leave
 # ---------------------------------------------------------------------------------------------------------
# classes of leave
classes <- c(own = "type_own", 
             illspouse = "type_illspouse",
             illchild = "type_illchild",
             illparent = "type_illparent",
             matdis = "type_matdis",
             bond = "type_bond")

conditional <- c(own = "TRUE",
                 illspouse = "nevermarried == 0 & divorced == 0",
                 illchild = "TRUE",
                 illparent = "TRUE",
                 matdis = "female == 1 & nochildren == 0",
                 bond = "nochildren == 0")


predict <- runEstimate(classes, conditional, "needtake")
d_acs <- merge(d_acs,predict, by="id")


# ---------------------------------------------------------------------------------------------------------
# 2. Days of Leave Taken
# ---------------------------------------------------------------------------------------------------------
#Days of leave taken - currently takes length from most recent leave only
classes <- c(own = "length_own",
             illspouse = "length_illspouse",
             illchild = "length_illchild",
             illparent = "length_illparent",
             matdis = "length_matdis",
             bond = "length_bond")

conditional <- c(own = "TRUE",
                 illspouse = "nevermarried == 0 & divorced == 0",
                 illchild = "TRUE",
                 illparent = "TRUE",
                 matdis = "female == 1 & nochildren == 0",
                 bond = "nochildren == 0")

predict <- runEstimate(classes, conditional, "length")

d_acs <- merge(d_acs,predict, by="id")

# ---------------------------------------------------------------------------------------------------------
# 3. Cost of Leave Taken -> placeholder right now, everyone receives half their daily wage
#
# Just a function of income: daily wage * length of leave in days * .5
#
# Need to finish CPS imputations to make this complete I think
# ---------------------------------------------------------------------------------------------------------
#Days of leave taken - currently takes length from most recent leave only
classes <- c("own", "illspouse", "illchild","illparent","matdis","bond")
for (i in classes) {
  cost = paste(i,"_cost", sep="")
  len = paste(i,"_length", sep="")
  
  # not dealing with weeks worked yet, need to impute that.
  # just assuming full time jobs for illustration purposes - 261 working days per year
  d_acs[cost] <- as.matrix(d_acs["WAGP"])/261  *as.matrix(d_acs[len])*.5
}

# shuffle around some of the columns for ease of readability
d_acs <- d_acs[,c(1:341, 342, 348, 354, 343, 349, 355, 344, 350, 356, 
                  345, 351, 357, 346, 352, 358, 347, 353, 359)]

write.csv(d_acs, file = "ACS_with_leave.csv", row.names = FALSE)


