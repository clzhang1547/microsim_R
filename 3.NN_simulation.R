
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

simulate <- function(filename, d_leave, d_test) {

  # cleaning
  d_test$age <- d_test$AGEP
  
  # ---------------------------------------------------------------------------------------------------------
  # 0. Define Functions
  # ---------------------------------------------------------------------------------------------------------
  xvars <- c("widowed", "divorced", "separated", "nevermarried", "female", 
             "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
             "white", "asian", "hisp","nochildren")
  
  runKNN <- function(y,z) {
    # filter fmla cases
    d_temp <- d_leave %>% filter_(z)
    d_temp <- d_temp[c(y, xvars)]
    d_temp <- d_temp %>% filter(complete.cases(.))
    
    # create labels, training data
    label <- d_temp[c(y)]
    train <- d_temp[c(xvars)]
    
    # filter out acs vars
    temp_acs <- d_test %>% filter_(z)
    
    # create test data set 
    test <- temp_acs[c(xvars)]
    
    # estimate KNN
    estimate <- knn(train, test, as.factor(label[,y]), k=1, use.all=FALSE)
    return(data.frame(temp_acs["id"],as.numeric(estimate)-1))
  }
  
  runEstimate <- function(y,z,lname) {
    # get KNN estimates for all leave types
    estimates <- mapply(runKNN,y=y,z=z, SIMPLIFY = FALSE)
    names(estimates) <-paste(lname,names(estimates),sep= "_")
    
    # compile estimates into a single dataframe
    est_df <- data.frame(row.names=d_test$id)
    est_df$id <- d_test$id
    j <- 0
  
    for (i in estimates) {
      j <- j+1
      colnames(i) <- c("id", names(estimates)[j] )
      est_df <- merge(est_df, i, by="id", all.x=TRUE)
    }
    return(est_df)
  
  }
  
  
  
   # ---------------------------------------------------------------------------------------------------------
   # 1. Probability of taking a leave
   # ---------------------------------------------------------------------------------------------------------
  # classes of leave
  classes <- c(own = "take_own", 
               illspouse = "take_illspouse",
               illchild = "take_illchild",
               illparent = "take_illparent",
               matdis = "take_matdis",
               bond = "take_bond")
  
  conditional <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0")
  
  
  predict <- runEstimate(classes, conditional, "take")
  d_test <- merge(d_test,predict, by="id")
  
  
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
  
  d_test <- merge(d_test,predict, by="id")
  
  # ---------------------------------------------------------------------------------------------------------
  # 3. Cost of Leave Taken (benefits recieved) 
  #
  # placeholder right now, everyone receives half their daily wage
  #
  # Just a function of income: daily wage * length of leave in days * .5
  #
  # Need to finish CPS imputations to make this complete I think
  # ---------------------------------------------------------------------------------------------------------
  #Days of leave taken - currently takes length from most recent leave only
  classes <- c("own", "illspouse", "illchild","illparent","matdis","bond")
  for (i in classes) {
    cost = paste("cost_",i, sep="")
    len = paste("length_",i, sep="")
    
    # not dealing with weeks worked yet, need to impute that.
    # just assuming full time jobs for illustration purposes - 261 working days per year
    d_test[cost] <- as.matrix(d_test["WAGP"])/261  *as.matrix(d_test[len])*.5
  }
  
  # # shuffle around some of the columns for ease of readability
  # d_test <- d_test[,c(1:341, 342, 348, 354, 343, 349, 355, 344, 350, 356, 
  #                   345, 351, 357, 346, 352, 358, 347, 353, 359)]
  
  write.csv(d_test, file = paste(filename,".csv",sep=""), row.names = FALSE)
  return(d_test)
}