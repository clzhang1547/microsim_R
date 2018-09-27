

# """
# Program to create a basic k=1 Nearest Neighbor imputation 
# Leave lengths for multiple reasons need to be imputed for multi-leave takers
# 
# ---Multi-leave taker leave type and length imputation
# In FMLA, leave lengths are only unbiasedly observed for most recent leave. 
# We also observe the number of reasons leave was taken
# We will impute the other leave types taken based on logit model from ACM
# We will impute leave length from other FMLA obs' "most recent leave" of the same type
#
# ---Leave needer leave length imptuation
# do not observe leave lengths for leave needers
# We will impute the other leave types taken based on logit model 
#
# For now using logit imputation
#
# 13 Sept 2018
# Luke
#
# """

cat("\014")  
#rm(list=ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("plyr")
library("dplyr")
library("survey")
library("varhandle")

options(error=recover)
#options(error=NULL)
source("4.NN_simulation.R")

# d_test - entire FMLA survey
# d_train - FMLA survey respondents with multiple leaves taken/needed

# ---------------------------------------------------------------------------------------------------------
# 0. Define Functions
# ---------------------------------------------------------------------------------------------------------
# Function to run logits
runLogit <- function(d_in,x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = d_in %>% filter_(y))
  svyglm(as.formula(x),data = d_in %>% filter_(y),family = "quasibinomial",design = des)
}

# Function to run Estimates
runLogitEstimate <- function(d_in,x,y,z){
  
  # Apply function to all leave types
  complete <- mapply(runLogit, x = x, y = y, z = z, MoreArgs=list(d_in), SIMPLIFY = FALSE)
  
  # Extract Coefficient Estimates
  # switched to lapply, sapply was giving me errors for specif that were all the same functional form
  estimates <- lapply(complete, coef)
  return(estimates)
}

# Function to apply estimates to training data set
runImpute <- function(d_in, estimates, lname,tcond) {
  
  # common var lists
  vars_name=c()
  vars_prob=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste(lname,i, sep="_"))
    vars_prob= c(vars_prob, paste(lname,i,"prob", sep="_"))
  }
  
  d <- d_in %>% filter_(tcond)
  vars = c()
  # calculate a column of probabilities of each leave type based on individual characteristics
  
  for (i in leave_types) {
    var= paste(lname,i, sep="_")
    var_prob= paste(lname,i,"prob", sep="_")
    model=estimates[[i]]
    d[var_prob]=model["(Intercept)"]
    for (dem in names(model)) {
      if (dem !='(Intercept)') { 
        d[is.na(d[,dem]),dem]=0
        d[,var_prob]= d[,var_prob] + d[,dem]*model[dem]
      }
      d[,var_prob]=exp(d[,var_prob])/(1+exp(d[,var_prob]))
    }
    
  }
  
  # randomly select leave types for those taking multiple leaves from those types not already taken
  
  d['leave_count']=d['num_leaves_taken']- rowSums(d[,vars_name], na.rm=TRUE)
  for (n in seq(1, max(d['leave_count']))) {
    d['rand']=runif(nrow(d))    
    
    # transform values to represent probabilities that sum to 1 of remaining possible leave types 
    d['sum']=0
    for (i in leave_types) {
      var_name= paste(lname,i, sep="_")
      var_prob= paste(lname,i,"prob", sep="_")
      d[d[,var_name]==1 & !is.na(d[,var_name]), var_prob]= 0
      d['sum']= d[,'sum'] + d[,var_prob]
    }
    d['cum']=0
    for (i in leave_types) {
      var_prob= paste(lname,i,"prob", sep="_")
      var_name= paste(lname,i, sep="_")
      d[d['sum']!=0,var_prob]=  d[d['sum']!=0,var_prob]/d[d['sum']!=0,'sum']
      d[d['sum']==0,var_prob]=  0
      d[d['rand']>d['cum'] & d['rand']<(d['cum']+d[,var_prob]) & d['leave_count']>=n, var_name]=1
      d['cum']= d[,'cum'] + d[,var_prob]
    }
  }
  # keep just the new take vars variables and id
  
  keep <- c(vars_name, "empid")
  d <- d[ , (names(d) %in% keep)]
  d <- merge( d, d_in, by="empid",all.y=TRUE)
  
  # update take_vars for those with multiple leaves
  for (i in vars_name) { 
    x=paste(i,".x",sep="")
    y=paste(i,".y",sep="")
    d[!is.na(d[x]),i]= d[!is.na(d[x]),x]
    d[is.na(d[i]),i]= d[is.na(d[i]),y]
  }
  for (i in vars_name) { 
    x=paste(i,".x",sep="")
    y=paste(i,".y",sep="")
    d <- d[, !(names(d) %in% c(x,y))]
  }
  return(d)
}

# functions to randomly draw from distribution of variable in data set

RandDraw <- function(d_train,d_test,y,z,test_cond) {

  # filter fmla cases
  d_temp <- d_train %>% filter_(z)
  train <- d_temp %>% filter(complete.cases(y))
  
  # filter out acs vars
  test <- d_test %>% filter_(test_cond)
  
  # random draw
  if (!y %in% colnames(test)) {
    test[y]=NA
  }
  A <- function(x) train[sample(nrow(train), 1), y]
  test[y] <- apply(test[y],1, A)

  return(data.frame(test[c("empid",y)]))
}

runRandDraw <- function(d_train,d_test,y,z,test_cond,lname) {
  estimates <- mapply(RandDraw,y=y,z=z, test_cond=test_cond, MoreArgs=list(d_train,d_test), SIMPLIFY = FALSE)
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

# function to impute leave length once leave taking behavior has been modified

impute_leave_length <- function(d_impute, d_in, conditional, test_cond,leaveprogram) { 
  
  classes <- c(own = "length_own",
               illspouse = "length_illspouse",
               illchild = "length_illchild",
               illparent = "length_illparent",
               matdis = "length_matdis",
               bond = "length_bond")

  
  # moving to draw from leave distribution rather than KNN prediction for computational issues
  #predict <- runKNNestimate(d_in,d_impute, classes, conditional, test_cond, "length_")
  predict <- runRandDraw(d_impute,d_in, classes, conditional, test_cond, "length_")
  
  d_in <- merge(predict, d_in, by="empid", all.y=TRUE)

  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste("length",i, sep="_"))
  }
  
  # update leave vars
  for (i in vars_name) { 
    x=paste(i,".x",sep="")
    y=paste(i,".y",sep="")
    if (x %in% colnames(d_in) & y %in% colnames(d_in)) {
      d_in[!is.na(d_in[x]) & !is.na(d_in[y]) & d_in[,y]==0 ,i]= 
        d_in[!is.na(d_in[x]) & !is.na(d_in[y]) & d_in[,y]==0,x]
      d_in[is.na(d_in[i]),i]= d_in[is.na(d_in[i]),y]  
    }
  }
  
  # fix logical inconsistency from previous loop in cases returning predicted NA value 
  # .y column was passing its value if x was na, and that needs to be addressed
  
  for (i in leave_types) {
    var_take=paste("take_",i,sep="")
    var_length=paste("length_",i,sep="")
    d_in[d_in[var_take]==0 & is.na(d_in[var_length])==FALSE & is.na(d_in[var_take])==FALSE,var_length]=0
  }
  
  # drop extraneous merge vars
  for (i in vars_name) { 
    x=paste(i,".x",sep="")
    y=paste(i,".y",sep="")
    if (x %in% colnames(d_in) & y %in% colnames(d_in)) {
      d_in <- d_in[, !(names(d_in) %in% c(x,y))]
    }
  }
  return(d_in)
}

intra_impute <- function(d_fmla) {
  
  # ---------------------------------------------------------------------------------------------------------
  # 1. Types of Leave taken for multiple leave takers
  # ---------------------------------------------------------------------------------------------------------

  # specifications
  # using ACM specifications
  specif <- c(own = "take_own ~ age + male + lnfaminc + black + hisp + coveligd",
              illspouse = "take_illspouse ~ 1",
              illchild = "take_illchild ~ 1",
              illparent = "take_illparent ~ 1",
              matdis = "take_matdis ~ 1",
              bond = "take_bond ~ 1")
  
  leave_types=c("own","illspouse","illchild","illparent","matdis","bond")
  
  # subsetting data
  conditional <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0")
  
  test_conditional <- rep("num_leaves_taken>1",6)
  
  # weights
  weight <- c(own = "~ fixed_weight",
              illspouse = "~ fixed_weight",
              illchild = "~ fixed_weight",
              illparent = "~ weight",
              matdis = "~ fixed_weight",
              bond = "~ fixed_weight")
  
    # Run Estimation
  estimates <- runLogitEstimate(d_fmla, specif,conditional, weight)
  
    # Run leave type imputation on multi-leave takers based on logit estimates
  d_fmla <- runImpute(d_fmla, estimates, "take", test_conditional)  
  
  # ---------------------------------------------------------------------------------------------------------
  # 2. Types of Leave Needed for multiple leave needers
  # ---------------------------------------------------------------------------------------------------------
  
  # specifications
  # using ACM specifications
  specif <- c(own = "need_own ~ age + male + lnfaminc + black + hisp + coveligd",
              illspouse = "need_illspouse ~ 1",
              illchild = "need_illchild ~ 1",
              illparent = "need_illparent ~ 1",
              matdis = "need_matdis ~ 1",
              bond = "need_bond ~ 1")
  
  leave_types=c("own","illspouse","illchild","illparent","matdis","bond")
  
  # subsetting data
  conditional <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0")
  
  test_conditional <- rep("num_leaves_need>1",6)
  
  # weights
  weight <- c(own = "~ fixed_weight",
              illspouse = "~ fixed_weight",
              illchild = "~ fixed_weight",
              illparent = "~ weight",
              matdis = "~ fixed_weight",
              bond = "~ fixed_weight")

  # Run Estimation
  estimates <- runLogitEstimate(d_fmla, specif,conditional, weight)

  # Run leave type imputation on multi-leave takers based on logit estimates
  d_fmla <- runImpute(d_fmla, estimates, "need", test_conditional)

  return(d_fmla)
}
