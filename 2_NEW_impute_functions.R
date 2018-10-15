
# """
# 2_imputation_functions
#
# These functions prepare the FMLA data set for ACS to impute leave taking behavior from it.
# and then execute the imputation from CPS, then FMLA into ACS.
#
# 9 Sept 2018
# Luke
# 
# TESTING TODO: what happens when filtered test data sets of 0 obs are fed into imputation functions
#               currently is handled properly by runOrdinalImpute. Should make sure others are as well
#
# """



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. impute_intra_fmla
    # Subfunctions:
    # 1a. runLogit
    # 1b. runLogitImpute
    # 1c. add_leave_types
# 2. LEAVEPROGRAM
# 3. impute_leave_length
    # Subfunctions:
    # 3a. runRandDraw
# 4. impute_cps_to_acs
    # Subfunctions:    
    # 4a. runLogit
    # 4b. runLogitImpute
    # 4c. runOrdinal
    # 4d. runOrdinalImpute
# 5. impute_fmla_to_acs
    # 5a. KNN1_scratch
    # 5b. impute_leave_length

# ============================ #
# 1. impute_intra_fmla
# ============================ #

# Program to create a basic k=1 Nearest Neighbor imputation 
# Leave lengths for multiple reasons need to be imputed for multi-leave takers
# 
# We observe the number of reasons leave was taken, but observe only 1 or 2 of the actual reasons
# We will impute the other leave types taken based on logit model from ACM


impute_intra_fmla <- function(d_fmla) {
  # intra-fmla imputation for additional leave taking and lengths
  # This is modifying take_ and need_ vars for those with additional leaves
  
  # ---------------------------------------------------------------------------------------------------------
  # A. For those multi-leave takers who responded to longest leave for a different reason, use that reason/length 
  #    rather than imputing
  # ---------------------------------------------------------------------------------------------------------
  # count number of leaves needed
  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste("take_",i, sep=""))
  }
  
  # leave_count is tracking the number of variables to be imputed. Sorry, probably should have used a better var name
  # Num_leaves_take and the rowsums term are not consistent, hence this check beyond just the observation of
  # only longest and most recent leaves.
  d_fmla['leave_count']=d_fmla['num_leaves_take']- rowSums(d_fmla[,vars_name], na.rm=TRUE)
  d_fmla['long_flag']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="") # An alternative way for this is to use paste0
    len_var=paste("length_",i,sep="")
    long_var=paste("long_",i,sep="")
    longlen_var=paste("longlength_",i,sep="")
    
    # flag those whose longest leave is used
    d_fmla['long_flag'] <- with(d_fmla, ifelse(get(long_var)==1 & num_leaves_take>1 & leave_count>0 & get(take_var)==0,1,long_flag))  
    
    # alter length of 2nd leave type to match longest leave
    d_fmla[len_var] <- with(d_fmla, ifelse(get(long_var)==1 & num_leaves_take>1 & leave_count>0 & get(take_var)==0,get(longlen_var),get(len_var))) 
    
    # alter take_var of 2nd leave type of longest leave
    d_fmla[take_var] <- with(d_fmla, ifelse(get(long_var)==1 & num_leaves_take>1 & leave_count>0 & get(take_var)==0,1,get(take_var)))
  }
  
  # ---------------------------------------------------------------------------------------------------------
  # B. Types of Leave taken for multiple leave takers
  # ---------------------------------------------------------------------------------------------------------
  # creating throwaway function to easily repeat this for leave needers
  temp_func <- function(lname){
    
    # specifications
    # using ACM specifications
    specif <- c(own = paste0(lname, "_own ~ age + male + lnfaminc + black + hisp + coveligd"),
                illspouse = paste0(lname, "_illspouse ~ 1"),
                illchild = paste0(lname, "_illchild ~ 1"),
                illparent = paste0(lname, "_illparent ~ 1"),
                matdis = paste0(lname, "_matdis ~ 1"),
                bond = paste0(lname, "_bond ~ 1"))
    
    # subsetting data
    conditional <- c(own = "TRUE",
                     illspouse = "nevermarried == 0 & divorced == 0",
                     illchild = "TRUE",
                     illparent = "TRUE",
                     matdis = "female == 1 & nochildren == 0",
                     bond = "nochildren == 0")
    
    
    # weights
    weight <- c(own = "~ fixed_weight",
                illspouse = "~ fixed_weight",
                illchild = "~ fixed_weight",
                illparent = "~ weight",
                matdis = "~ fixed_weight",
                bond = "~ fixed_weight")
    
    
    # Run Estimation
    # INPUTS: FMLA data set, Lists of regression specifications, conditional filters for training 
    #         data, and weight selection 
    estimates <- mapply(runLogitEstimate, x = specif, y = conditional, z = weight, 
                        MoreArgs=list(d_in=d_fmla), 
                        SIMPLIFY = FALSE)
    
    # OUTPUT: Lists of regression estimates for each dependent var in specif
    
    # Generate probabilities of each leave type based on logit estimates
    vars_name=c()
    for (i in leave_types) {
      vars_name= c(vars_name, paste(lname,i, sep="_"))
    }
    test_conditional <- rep(paste0("num_leaves_", lname,">1 & long_flag==0"),6)
    
    # nest estimates for mapply to work properly
    nest_estimate <- lapply(seq(1,length(estimates)), function(y) {estimates[y]} )

    #INPUTS: FMLA data set, lists of regression coefficients, conditional filters for training data,
    #        variables to impute 
    
    impute <- mapply(runLogitImpute, estimate=nest_estimate, tcond= test_conditional, var_name=vars_name, 
                     MoreArgs=list(d_in=d_fmla,multi_var=TRUE), SIMPLIFY = FALSE)
    #OUTPUT: probabilities of each FMLA individual taking/needing a type of leave

    # Run leave type imputation on multi-leave takers based on probability
    # INPUT: FMLA data, probabilities of taking/needing leave type
    d_fmla <- add_leave_types(d_fmla,lname, impute) 
    # OUTPUT:  FMLA data with imputed leave types for those taking/needing multiple leaves, 
    #        of which one or more are not observed in their responses to the FMLA survey
  }
  
  # first run imputes take_* variables 
  temp_func("take")
  
  # ---------------------------------------------------------------------------------------------------------
  # C. Types of Leave Needed for multiple leave needers
  # ---------------------------------------------------------------------------------------------------------
  # second run: perform same operations on need_* variables
  temp_func("need")

  return(d_fmla)
}
# ============================ #
# 1A. runLogitEstimate
# ============================ #
# function to construct logit estimation model from training data set
runLogitEstimate <- function(d_in,x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = d_in %>% filter_(y))
  complete <- svyglm(as.formula(x),data = d_in %>% filter_(y),family = "quasibinomial",design = des)
  estimate <- complete$coefficients 
  return(estimate)
}

# ============================ #
# 1B. runLogitImpute
# ============================ #
# function to take logit estimation model, and impute values to test data set

runLogitImpute <- function(d_in, estimate, var_name, tcond, multi_var=FALSE) {

  # calculate a column of probabilities of variable based on individual characteristics
  var_prob= paste0(var_name,"_prob")
  d <- d_in %>% filter_(tcond)
  # unnest estimate if nested. needed to be nested in first place because of mapply
  if (multi_var==TRUE) {
    estimate= estimate[[1]]
  }
  model=estimate
  d[var_prob]=estimate['(Intercept)']
  for (dem in names(model)) {
    if (dem !='(Intercept)') { 
      d[is.na(d[,dem]),dem]=0
      d[var_prob]= d[,var_prob] + d[,dem]*model[dem]
    }
  }
  d[var_prob] <- with(d, exp(get(var_prob))/(1+exp(get(var_prob))))
  
  # for some variables, we just need a random generation for single column
  # However, for leave taking/needing imputation, we need to select leave types conditional
  # on the number of leaves to be imputed and the leave types already taken/needed.
  # so for those instances, return_var will be FALSE, and we'll then run the 
  # "add_leave_types" function below.
  if (multi_var==FALSE) {
    d['rand']=runif(nrow(d))
    d[var_name] <- with(d, ifelse(rand>get(var_prob),0,1))
    keep <- c(var_name, "id")
    d <- d[ , (names(d) %in% keep)]
    d <- merge( d, d_in, by="id",all.y=TRUE)
    return(d)
  }
  else {
    keep <- c(var_prob, "id")
    d <- d[ , (names(d) %in% keep)]
    return(d)
  }
}

# ============================ #
# 1C. add_leave_types
# ============================ #
add_leave_types <- function(d, lname, impute) {
  
  # merge imputed values with fmla data
  for (i in impute) {
    d <- merge(i, d, by="id",all.y=TRUE)
    # set missing probability = 0
    d[is.na(d[colnames(i[2])]), colnames(i[2])] <- 0
  }  
  d_orig <- d 
  # randomly select leave types for those taking multiple leaves from those types not already taken
  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste(lname,i, sep="_"))
  }
  
  d['leave_count']=d[paste0('num_leaves_',lname)]- rowSums(d[,vars_name], na.rm=TRUE)
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
  return(d)
}

# ============================ #
# 2. LEAVEPROGRAM
# ============================ #
# Baseline changes for addition of a leave program
# follows baseline changes of ACM model (see p.11 of ACM model description paper). Main change needed to base cleaning:
#   Leave needers who did not take a leave in the absence of a program, and
#   who said the reason that they did not take a leave was because they could not afford to
#   take one, take a leave in the presence of a program.

LEAVEPROGRAM <- function(d) {
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    need_var=paste("need_",i,sep="")
    # I wonder if we should create another dummy variable of whether this person didn't take a leave pre-program
    d[,take_var] <- ifelse(d[,"unaffordable"]==1 & d[,need_var]==1,1,d[,take_var])
  }
  
  return(d)
}


# ============================ #
# 3. impute_leave_length
# ============================ #
# function to impute leave length once leave taking behavior has been modified
impute_leave_length <- function(d_impute, d_in, conditional, test_cond, fmla) { 
  
  classes <- c(own = "length_own",
               illspouse = "length_illspouse",
               illchild = "length_illchild",
               illparent = "length_illparent",
               matdis = "length_matdis",
               bond = "length_bond")
  
  
  # moving to draw from leave distribution rather than KNN prediction for computational issues
  #predict <- runKNNestimate(d_in,d_impute, classes, conditional, test_cond, "length_")
  #INPUTS: variable requiring imputation, conditionals to filter test and training data on,
  #        ACS or FMLA observations requiring imputed leave length (test data), FMLA observations constituting the
  #        sample from which to impute length from (training data), and specification whether or not
  #        test data is FMLA data or not (operations differ for ACS length imputation)
  predict <- mapply(runRandDraw, y=classes, z=conditional,test_cond=test_cond,
                    MoreArgs = list(d_train=d_impute, d_test=d_in, lname="length_", fmla=fmla),
                    SIMPLIFY = FALSE)
  # Outputs: data sets of imputed leave length values for ACS or FMLA observations requiring them
  
  # merge imputed values with fmla data
  for (i in predict) {
    d_in <- merge(i, d_in, by="id",all.y=TRUE)
  }  

  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste("length",i, sep="_"))
  }
  
  # update leave vars
  # H: hmmm I'm a bit lost here. Let's talk through it. I think a lot of this could be solved with left_join
  # L: I agree this is probably unnecessarily messy. I have a vague memory of trying left_join and something not happening the way I wanted.
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
# ============================ #
# 3a. runRandDraw
# ============================ #
runRandDraw <- function(d_train,d_test,y,z,test_cond,lname, fmla=TRUE) {
  
  # filter training cases
  d_temp <- d_train %>% filter_(z)
  train <- d_temp %>% filter(complete.cases(y))
  
  # filter out test cases
  test <- d_test %>% filter_(test_cond)
  
  # random draw
  if (!y %in% colnames(test)) {
    test[y]=NA
  }
  
  A <- function(x) train[sample(nrow(train), 1), y]
  test[y] <- apply(test[y],1, A)
  
  # For the FMLA intra imputation runs of this, we also know long_length.
  # replace with minimum of (median length of training set & long_length) if draw is greater the length of longest leave
  # should think some more about what to do here, I think there's a better way.
  # Also, raw leave length distribution looks weird on closer inspection in FMLA data
  
  if (fmla==TRUE) {
    test$median <- median(train[,y])
    test[y] <- with(test, ifelse(!is.na(long_length) & get(y)>long_length,pmin(median, long_length),get(y)))
  }
  
  
  est_df <- test[c("id",y)]
  return(est_df) 

}

# ============================ #
# 4. impute_cps_to_acs
# ============================ #

# This program cleans CPS data and runs a number of logit
# regressions to produce coefficient estimates (which are stored as csv files)
# to be used in the simulation model.

impute_cps_to_acs <- function(d_acs, d_cps){
  
  # ---------------------------------------------------------------------------------------------------------
  # Run models
  # ---------------------------------------------------------------------------------------------------------
  
  # logit for hourly paid regression
  
  specif = c(paid_hrly =  paste("paid_hrly ~ female + black + a_age + agesq + BA",
                                "+ GradSch + occ_1 + occ_3 + occ_5 + occ_7 + occ_8",
                                "+ occ_9 + occ_10 + ind_5 + ind_8 + ind_11 + ind_12"))
  conditional = c(paid_hrly= "TRUE")
  weight = c(paid_hrly = "~ marsupwt")
  
  # INPUTS: CPS (training) data set, logit regression model specification, training filter condition, weight to use
  estimate <- runLogitEstimate(d_cps,specif,conditional,weight)
  # OUTPUT: Logit model estimates for getting paid hourly dummy
  
  # INPUTS: ACS (test) data set, Logit model estimates for getting paid hourly dummy, variable to impute
  #         test filtering condition
  d_acs <-runLogitImpute(d_acs, estimate,"paid_hrly","TRUE") 
  # OUTPUT: ACS data with imputed paid hourly variable
  
  # INPUTS/OUTPUTS are similar for remaining logits
  
  # ordered logit for number of employers
  # biggest problem with ordered logit currently is it is unweighted; can't use CPS weight without getting a non-convergence error
  specif = c(num_employers= paste("factor(phmemprs) ~  age + agesq + asian + hisp",
                                  "+ ltHS + someCol + BA + GradSch + lnearn",
                                  "+ hiemp + ind_4 + ind_5 + ind_6 + ind_8",
                                  "+ ind_13 + occ_1 + occ_6 + occ_7 + occ_9 + occ_10"))
  conditional = c(num_employers= "TRUE")
  weight = c(num_employers = "marsupwt")
  
  # INPUTS: CPS (training) data set, ordinal regression model specification, training filter condition, weight to use
  estimate <- runOrdinalEstimate(d_cps,specif,conditional,weight)
  # OUTPUTS: ordinal model estimates for number of employers dummy
  
  # INPUTS: ACS (test) data set, ordinal model estimates for getting number of employers dummy, variable to impute
  #         test filtering condition
  d_acs <- runOrdinalImpute(d_acs, estimate,"num_emp","TRUE")
  # OUTPUTS: ACS data with imputed number of employers variable
  
  # INPUTS/OUTPUTS are similar for remaining ordinals
  
  # ordered logit for weeks worked - 50-52 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + agesq +  black + hisp + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==1")
  weight = c(iweeks_worked = "marsupwt")
  estimate <- runOrdinalEstimate(d_cps,specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"wks_50_52","WKW==1")
  
  # logit for weeks worked - 48-49 weeks
  specif = c(iweeks_worked= paste("wks_48_49 ~ age + agesq + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==2")
  weight = c(iweeks_worked = "~ marsupwt")
  estimate <- runLogitEstimate(d_cps,specif,conditional,weight)
  d_acs <- runLogitImpute(d_acs, estimate,"wks_48_49","WKW==2")
  
  # ordered logit for weeks worked - 40-47 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==3")
  weight = c(iweeks_worked = "marsupwt")
  estimate <- runOrdinalEstimate(d_cps,specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"wks_40_47","WKW==3")
  
  # ordered logit for weeks worked - 27-39 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + fem_cu6 + fem_c617 + fem_cu6and617 + female + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==4")
  weight = c(iweeks_worked = "marsupwt")
  estimate <- runOrdinalEstimate(d_cps, specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"wks_27_39","WKW==4")
  
  # ordered logit for weeks worked - 14-26 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + hisp + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==5")
  weight = c(iweeks_worked = "marsupwt")
  estimate <- runOrdinalEstimate(d_cps, specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"wks_14_26","WKW==5")
  
  # ordered logit for weeks worked - <13 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + agesq + female + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==6")
  weight = c(iweeks_worked = "marsupwt")
  estimate <- runOrdinalEstimate(d_cps, specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"wks_0_13","WKW==6")
  
  # create single weeks worked var
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_50_52),wks_50_52+49,0))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_48_49),wks_48_49+48,iweeks_worked)) # "+48" is intentaional, this is a 0/1 var
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_40_47),wks_40_47+39,iweeks_worked))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_27_39),wks_27_39+26,iweeks_worked))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_14_26),wks_14_26+13,iweeks_worked))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_0_13),wks_0_13,iweeks_worked))
  
  # Ordered logit employer size categories
  specif = c(emp_size =  paste("factor(emp_size) ~ a_age + black + ltHS + someCol + BA + GradSch + lnearn",
                               "  + hiemp + ind_1 + ind_3 + ind_5 + ind_6 + ind_8 + ind_9",
                               "+ ind_11 + ind_12 + ind_13 + occ_1 + occ_4 + occ_5 + occ_6 + occ_7 + occ_9"))
  conditional = c(emp_size= "TRUE")
  weight = c(emp_size = "marsupwt")
  estimate <- runOrdinalEstimate(d_cps, specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"emp_size","TRUE")
  
  
  # then do random draw within assigned size range
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==1,sample(1:9, nrow(d_acs), replace=T),0))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==2,sample(10:49, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==3,sample(50:99, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==4,sample(100:499, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==5,sample(500:999, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==6,sample(1000:9999, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(emp_size=temp_size)
  d_acs <- d_acs %>% mutate(weeks_worked_cat=weeks_worked)
  d_acs <- d_acs %>% mutate(weeks_worked=iweeks_worked)
  
  
  # clean up vars
  d_acs <- d_acs[, !(names(d_acs) %in% c('rand','temp_size','iweeks_worked',
                                         "wks_0_13", "wks_14_26", "wks_27_39", "wks_40_47", 
                                         "wks_48_49", "wks_50_52", "num_emp" ))]
  return(d_acs)
}

# ============================ #
# 4A. runLogitEstimate
# ============================ #
# see function 1A

# ============================ #
# 4B. runLogitImputation
# ============================ #
# see function 1B

# ============================ #
# 4C. runOrdinalEstimate
# ============================ #
# MASS implementation, polr function
runOrdinalEstimate <- function(d_cps,x,y,z){
  estimate <- polr(as.formula(x), data = d_cps %>% filter_(y))
  return(estimate)
  
  # 
  #   # OGLMX implementation - gives pretty non sensical results from my efforts
  #   runOrdinal <- function(x,y,z){
  #      results.ologit <- oglmx(as.formula(x), data = d_cps %>% filter_(y), weights=marsupwt)
  #      pause()
  #      return(estimate)
  #   }

}

# ============================ #
# 4D. runOrdinalImputation
# ============================ #
# Function to apply estimate to training data set, ordinal
runOrdinalImpute <- function(d_in, estimate, varname, tcond) {
  d <- d_in %>% filter_(tcond)
  
  # ensure there is at least one row in test data set that needs imputing
  if (!is.null(rownames(d))) {
    
    # calculate score from ordinal model
    model=estimate$coefficients
    d['var_score']=0
    for (dem in names(model)) {
      if (dem !='(Intercept)') { 
        d[is.na(d[,dem]),dem]=0
        d[,'var_score']= d[,'var_score'] + d[,dem]*model[dem]
      }
    }
    
    # assign categorical variable based on ordinal cuts
    cuts= estimate$zeta
    cat_num= length(cuts)+1
    d[varname] <- 0
    d['rand']=runif(nrow(d))
    for (i in seq(cat_num)) {
      if (i!=cat_num) {
        d <- d %>% mutate(cumprob= var_score-cuts[i])
        d <- d %>% mutate(cumprob2= exp(cumprob)/(1+exp(cumprob)))
        d[varname] <- with (d, ifelse(get(varname)==0 & rand>=cumprob2,i,get(varname)))
      }
      if (i==cat_num) {
        d[varname] <- with (d, ifelse(get(varname)==0,i,get(varname)))
      }
    }
    # keep just the new variable and id
    d <- d[c(varname, "id")]
    d_in <- merge( d, d_in, by="id",all.y=TRUE)
  }
  return(d_in)
}  

# ============================ #
# 5. impute_fmla_to_acs
# ============================ #
# Based on user-specified method, impute leave taking behavior in fmla to ACS
# default is KNN1
impute_fmla_to_acs <- function(d_fmla, d_fmla_orig, d_acs,leaveprogram, impute_method,xvars) {
  # d_fmla - modified fmla data set
  # d_fmla_orig - unmodified fmla data set
  # d_acs - ACS data set
  # leaveprogram - TRUE or FALSE. Presence or absence of a leave program. 
  # impute_method - method to use for imputation
  # xvars - dependent variables used by imputation method. Must be present and have same name in 
  #         both fmla and acs data sets.
  
  # ---------------------------------------------------------------------------------------------------------
  # A. Leave characteristics needed: leave taking behavior, proportion of income paid by employer,
  #     whether leave was unaffordable or not
  # ---------------------------------------------------------------------------------------------------------

  # classes of leave
  classes <- c(own = "take_own", 
               illspouse = "take_illspouse",
               illchild = "take_illchild",
               illparent = "take_illparent",
               matdis = "take_matdis",
               bond = "take_bond",
               prop_pay = "prop_pay",
               unaffordable= "unaffordable")
  # H: I forget the logic for these conditionals. We should discuss.
  # L: Sure
  conditional <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0",
                   prop_pay="TRUE",
                   unaffordable="TRUE")

  # Save ACS and FMLA Dataframes at this point to document format that alternative imputation methods 
  # will need to expect
  saveRDS(d_fmla, file="d_fmla_impute_input.rds")
  saveRDS(d_acs, file="d_acs_impute_input.rds")
  # KNN1 imputation method
  if (impute_method=="KNN1") {
    # separate KNN1 calls for each unique conditional doesn't work because of differing missing values
    
    # INPUTS: variable to be imputed, conditionals to filter training and test data on, FMLA data (training), and
    #         ACS data (test), id variable, and dependent variables to use in imputation
    impute <- mapply(KNN1_scratch, imp_var=classes,train_cond=conditional, test_cond=conditional,
                        MoreArgs=list(d_train=d_fmla,d_test=d_acs,xvars=xvars), SIMPLIFY = FALSE)
    # OUTPUTS: A data set for each leave taking/other variables requiring imputation. Each data set contains id for ACS
    #          observations in the leave variable universe, and a dummy indicating whether that ACS individual
    #          takes that kind of leave of not, as well as a dummy for whether leave needing is due to 
    #          unaffordability, and the proportion of pay received from employer while on leave.
    
   # merge imputed values with acs data
    for (i in impute) {
      d_acs <- merge(i, d_acs, by="id",all.y=TRUE)
    }  
  }
  browser()
  saveRDS(d_acs, file="d_acs_impute_output.rds")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # alternate imputation methods will go here
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # for example:
  
  if (impute_method=="Hocus Pocus") {
    # Hocus pocus function calls here
  }
  

  # ---------------------------------------------------------------------------------------------------------
  # B. Impute Days of Leave Taken
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
  # H:  Why are these imputed separately? Couldn't this be done in the above? In other words, why doesn't the above also
  #     grab the length from the nearest neighbor instead of just the take/don't take indicator?
  # L: Would save us a KNN call. Only issue might be there are take_==1 in FMLA with missing lengths
  #    So they are drawing from different conditionals (and this is a random draw as opposed to a KNN to match ACM's model).
  #    This is one more place where KNN could be called.
  
  if (leaveprogram==TRUE) {
    conditional <- c(own = "recStatePay == 1 &length_own>0 & is.na(length_own)==FALSE",
                     illspouse = "recStatePay == 1 &length_illspouse>0 & is.na(length_illspouse)==FALSE & nevermarried == 0 & divorced == 0",
                     illchild = "recStatePay == 1 &length_illchild>0 & is.na(length_illchild)==FALSE",
                     illparent = "recStatePay == 1 &length_illparent>0 & is.na(length_illparent)==FALSE",
                     matdis = "recStatePay == 1 &length_matdis>0 & is.na(length_matdis)==FALSE & female == 1 & nochildren == 0",
                     bond = "recStatePay == 1 &length_bond>0 & is.na(length_bond)==FALSE & nochildren == 0")
  }
  if (leaveprogram==FALSE) {
    conditional <- c(own = "recStatePay == 0 & length_own>0 & is.na(length_own)==FALSE",
                     illspouse = "recStatePay == 0 & length_illspouse>0 & is.na(length_illspouse)==FALSE & nevermarried == 0 & divorced == 0",
                     illchild = "recStatePay == 0 & length_illchild>0 & is.na(length_illchild)==FALSE",
                     illparent = "recStatePay == 0 & length_illparent>0 & is.na(length_illparent)==FALSE",
                     matdis = "recStatePay == 0 & length_matdis>0 & is.na(length_matdis)==FALSE & female == 1 & nochildren == 0",
                     bond = "recStatePay == 0 & length_bond>0 & is.na(length_bond)==FALSE & nochildren == 0")
  } 
  
  test_conditional <- c(own = "take_own==1",
                        illspouse = "take_illspouse==1 & nevermarried == 0 & divorced == 0",
                        illchild = "take_illchild==1",
                        illparent = "take_illparent==1",
                        matdis = "take_matdis==1 & female == 1 & nochildren == 0",
                        bond = "take_bond==1 & nochildren == 0")
  
  # INPUTS: unmodified FMLA survey as the training data, ACS as test data,
  #         and conditionals for filter the two sets. Using unmodified survey for leave lengths,
  #         since modified FMLA survey contains no new information about leave lengths, and 
  #         the intra-FMLA imputed leave lengths a random draw from that imputed data
  #         would produced a biased 
  #         estimate of leave length
  d_acs <- impute_leave_length(d_fmla_orig, d_acs, conditional, test_conditional, fmla=FALSE)
  # OUTPUT: ACS data with lengths for leaves imputed
  
  # replace leave taking and length NA's with zeros now
  # wanted to distinguish between NAs and zeros in FMLA survey, 
  # but no need for that in ACS now that we're "certain" of ACS leave taking behavior 
  # We are "certain" because we only imputed leave takers/non-takers, discarding those with 
  # uncertain/ineligible status (take_[type]=NA).
  
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    take_var=paste("take_",i,sep="")
    d_acs[len_var] <- with(d_acs, ifelse(is.na(get(len_var)),0,get(len_var)))
    d_acs[take_var] <- with(d_acs, ifelse(is.na(get(take_var)),0,get(take_var)))
  }
  
  return(d_acs)
}

# ============================ #
# 5A. KNN1_scratch
# ============================ #
# Define KNN1 matching method

KNN1_scratch <- function(d_train, d_test, imp_var, train_cond, test_cond, xvars) { 
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.
  
  # Data manipulation

  # filter fmla m_test
  # filter on conditions, select just the variables we want to impute and the variables used to calculate distance
  # H: Use dplyr select here. ALl 3 lines could just be one line
  # L: Great, I will do that. Update: what's the best way to do one line?
  
  # create training data
  
  # filter by training condition
  train <- d_train %>% filter_(train_cond)
  # ensure that only nonmissing data are used in training
  train <- train %>% filter(complete.cases(select(train, 'id', imp_var,xvars)))
  train <- select(train, imp_var, xvars)
  train ['nbor_id'] <- as.numeric(rownames(train))
  train ['id'] <- NULL
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  test <- d_test %>% filter_(test_cond)
  test <- select(test, 'id', xvars)
  test <- test %>% filter(complete.cases(.))
  
  # Initial checks
  
  # check for data frames
  if ((!is.data.frame(train)) | (!is.data.frame(test))) {
    stop("train_set and test_set must be data frames")
  }  
  
  # check for missing data
  if (anyNA(train) | anyNA(test)) {
    stop("missing values not allowed in train_test or test_set")
  }
  
  # normalize training data to equally weight differences between variables
  # H: I'm not sure whether we normalize before or after computing the distance. We should check this.
  # L: This is normalizing before checking the distance. I'm pretty sure that's where it should happen.
  for (i in colnames(train)) {
    if (i != 'nbor_id' & i != imp_var & sum(train[i])!=0 ){
      train[i] <- scale(train[i],center=0,scale=max(train[,i]))
    }
  } 
  
  for (i in colnames(test)) {
    if (i != 'id' & sum(test[i])!=0 ){
      test[i] <- scale(test[i],center=0,scale=max(test[,i]))
    }
  } 
  
  # id var must be first variable of data
  
  # find distance
  
  m_test <- as.matrix(test)
  m_train <-as.matrix(train)
  
  nest_test <- list()
  nest_train <- list()
  # nested lists of vectors for apply functions
  
  nest_test <- lapply(seq(1,nrow(m_test)) , function(y){ 
    m_test[y,colnames(test)!='id']
  })
  nest_train <- lapply(seq(1,nrow(m_train)) , function(y){ 
    m_train[y,colnames(train)!='nbor_id' & colnames(train)!=imp_var]
  })
  
  # mark neighbor as minimium distance
  start_time <-Sys.time()
  min_start <- ncol(train)-2
  
  f1 <- function(j) {
    min_dist <- min_start
    d <- mapply(find_dist, x=nest_train, MoreArgs=list(y=j))
    return(which.min(d))
  }
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
  temp <- lapply(nest_test, f1)
  temp <- unlist(temp)
  temp <- cbind(test["id"],as.data.frame(unlist(temp)))
  colnames(temp)[colnames(temp)=="unlist(temp)"] <- "nbor_id"
  temp <- join(temp[c("id","nbor_id")], train[c("nbor_id",imp_var)], by=c("nbor_id"), type="left")
  temp <- temp[c("id",imp_var)]
  return(temp)
}

# ============================ #
# 5B. impute_leave_length
# ============================ #
# see function 3
