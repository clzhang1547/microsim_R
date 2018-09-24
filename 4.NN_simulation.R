
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
library("varhandle")

#options(error=recover)
options(error=NULL)

# ---------------------------------------------------------------------------------------------------------
# 0. Define Functions
# ---------------------------------------------------------------------------------------------------------

runKNN <- function(d_train,d_test,y,z,test_cond) {
  
  xvars <- c("widowed", "divorced", "separated", "nevermarried", "female", 
             "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
             "white", "asian", "hisp","nochildren")
  
  # filter fmla cases
  d_temp <- d_train %>% filter_(z)
  d_temp <- d_temp[c(y, xvars)]
  d_temp <- d_temp %>% filter(complete.cases(.))
  
  # create labels, training data
  label <- d_temp[c(y)]
  train <- d_temp[c(xvars)]
  
  # filter out acs vars
  temp_test <- d_test %>% filter_(test_cond)
  temp_test <- temp_test %>% filter(complete.cases(temp_test[c(xvars)]))
  
  # create test data set 
  test <- temp_test[c(xvars)]
  
  
  
  # estimate KNN
  estimate <-knn(train, test, as.factor(label[,y]), k=1, use.all=FALSE)
  
  # NOT YET ADDRESSED
  # in case of ties, knn() function randomly assigns neighbor among tied candidates
  # fixing random seed to preserve replicability 
  
  
  return(data.frame(temp_test["empid"],unfactor(estimate)))
}

runKNNestimate <- function(d_train,d_test,y,z,test_cond,lname) {
  # get KNN estimates for all leave types
  estimates <- mapply(runKNN,y=y,z=z, test_cond=test_cond, MoreArgs=list(d_train,d_test), SIMPLIFY = FALSE)
  names(estimates) <-paste(lname,names(estimates),sep= "")
  
  # compile estimates into a single dataframe
  est_df <- data.frame(row.names=d_test$empid)
  est_df$empid <- d_test$empid
  j <- 0
  
  for (i in estimates) {
    j <- j+1
    colnames(i) <- c("empid", names(estimates)[j] )
    est_df <- merge(est_df, i, by="empid", all.x=TRUE)
  }
  return(est_df)
  
}

#  function to loop through leaves types
fmla_impute <- function(filename, d_fmla, d_acs,leaveprogram) {
     # ---------------------------------------------------------------------------------------------------------
   # 1. the probability of taking a leave
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
  
  
  predict <- runKNNestimate(d_fmla,d_acs,classes, conditional, conditional, "take_")
  d_acs <- merge(d_acs,predict, by="empid")
  
  
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
  
  #   Leave lengths are the same, except for own leaves, which are instead taken from the distribution of leave takers in FMLA survey reporting 
  #   receiving some pay from state programs
  
  if (leaveprogram==TRUE) {
    conditional <- c(own = "recStatePay == 1",
                     illspouse = "nevermarried == 0 & divorced == 0 & recStatePay == 1",
                     illchild = "recStatePay == 1",
                     illparent = "recStatePay == 1",
                     matdis = "female == 1 & nochildren == 0 & recStatePay == 1",
                     bond = "nochildren == 0 & recStatePay == 1")
  }
  
  if (leaveprogram==FALSE) {
    conditional <- c(own = "recStatePay == 0",
                     illspouse = "nevermarried == 0 & divorced == 0 & recStatePay == 0",
                     illchild = "recStatePay == 0",
                     illparent = "recStatePay == 0",
                     matdis = "female == 1 & nochildren == 0 & recStatePay == 0",
                     bond = "nochildren == 0 & recStatePay == 0")
  }
  
  test_conditional <- c(own = "take_own==1",
                        illspouse = "take_illspouse==1 & nevermarried == 0 & divorced == 0",
                        illchild = "take_illchild==1",
                        illparent = "take_illparent==1",
                        matdis = "take_matdis==1 & female == 1 & nochildren == 0",
                        bond = "take_bond==1 & nochildren == 0")
  
  predict <- runKNNestimate(d_fmla,d_acs,classes, conditional, test_conditional, "length_")
  
  d_acs <- merge(d_acs,predict, by="empid")
  
  # ---------------------------------------------------------------------------------------------------------
  # 3. Other Leave characteristics needed
  # ---------------------------------------------------------------------------------------------------------
  classes <- c("prop_pay","particip","particip_length","total_length","benefit_prop","coveligd")
  
  conditional <- rep("TRUE",length(classes))
  
  predict<- runKNNestimate(d_fmla,d_acs,classes, conditional, conditional, "")
  
  d_acs <- merge(d_acs,predict, by="empid")

  write.csv(d_acs, file = paste(filename,".csv",sep=""), row.names = FALSE)
  return(d_acs)
}



