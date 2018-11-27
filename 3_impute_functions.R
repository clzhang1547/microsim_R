
# """
# 3_impute_functions
#
# These functions impute the FMLA data set into the ACS.
#
# 9 Sept 2018
# Luke
# 
# TESTING TODO: what happens when filtered test data sets of 0 obs are fed into imputation functions
#               currently is handled properly by runOrdinalImpute and runRandDraw. 
#               Should make sure others are as well.
#
# """



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. impute_fmla_to_acs
# Modular imputation methods - can be swaped out for one another for FMLA to ACS imputation of:
# take_* vars, unaffordable, prop_pay variables
  # 1A. KNN1_scratch
  # 1B. logit_leave_method
      # 1Ba. runLogitImpute - used in hardcoded methods found elsewhere as well
  # 1C. KNN_multi
  # 1D. Naive_Bayes

# ============================ #
# 1. impute_fmla_to_acs
# ============================ #
# master fmla to acs imputation function
# Based on user-specified method, impute leave taking behavior in fmla to ACS
# default is KNN1

impute_fmla_to_acs <- function(d_fmla, d_fmla_orig, d_acs,leaveprogram, impute_method,xvars,kval) {
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
  
  # -----------------Hard coded objects all methods must use-----------------------------------------------
  # yvars: the dependent vars that must be imputed by the selected method
  yvars <- c(own = "take_own", 
              illspouse = "take_illspouse",
              illchild = "take_illchild",
              illparent = "take_illparent",
              matdis = "take_matdis",
              bond = "take_bond",
              prop_pay = "prop_pay",
              unaffordable= "unaffordable")
  
  # filters: logical conditionals always applied to filter vraiable imputation 
  filts <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0",
                   prop_pay="TRUE",
                   unaffordable="TRUE")
  
  # weight: if method uses FMLA weights, the weight variable to use
  weights <- c(own = "~ fixed_weight",
              illspouse = "~ fixed_weight",
              illchild = "~ fixed_weight",
              illparent = "~ weight",
              matdis = "~ fixed_weight",
              bond = "~ fixed_weight",
              prop_pay = '~ fixed_weight',
              unaffordable = "~ fixed_weight")
  
  # Save ACS and FMLA Dataframes at this point to document format that 
  # alternative imputation methods will need to expect
  saveRDS(d_fmla, file="./R_dataframes/d_fmla_impute_input.rds") # TODO: Remove from final version
  saveRDS(d_acs, file="./R_dataframes/d_acs_impute_input.rds") # TODO: Remove from final version
  
  # KNN1 imputation method
  if (impute_method=="KNN1") {
    # separate KNN1 calls for each unique conditional doesn't work because of differing missing values
    # INPUTS: variable to be imputed, conditionals to filter training and test data on, FMLA data (training), and
    #         ACS data (test), id variable, and dependent variables to use in imputation
    impute <- mapply(KNN1_scratch, imp_var=yvars,train_filt=filts, test_filt=filts,
                        MoreArgs=list(d_train=d_fmla,d_test=d_acs,xvars=xvars), SIMPLIFY = FALSE)
    # OUTPUTS: list of data sets for each leave taking/other variables requiring imputation. 
   # merge imputed values with acs data
    for (i in impute) {
      d_acs <- merge(i, d_acs, by="id",all.y=TRUE)
    }  
    
    # save output for reference when making other methods
    saveRDS(d_acs, file="./R_dataframes/d_acs_impute_output.rds") # TODO: Remove from final version
  }
  
  # Logit estimation of leave taking to compare with Chris' results in Python
  if (impute_method=="logit") {
    d_acs <- logit_leave_method(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                                yvars=yvars, test_filts=filts, train_filts=filts, 
                                weights=weights, create_dummies=TRUE)
  }
  
  if (impute_method=="KNN_multi") {
    # INPUTS: variable to be imputed, conditionals to filter training and test data on, FMLA data (training), and
    #         ACS data (test), id variable, and dependent variables to use in imputation, number of nbors
    impute <- mapply(KNN_multi, imp_var=yvars,train_filt=filts, test_filt=filts,
                     MoreArgs=list(d_train=d_fmla,d_test=d_acs,xvars=xvars, kval=kval), SIMPLIFY = FALSE)
    
    # OUTPUTS: list of data sets for each leave taking/other variables requiring imputation. 
    # merge imputed values with acs data
    for (i in impute) {
      d_acs <- merge(i, d_acs, by="id",all.y=TRUE)
    }  
    
  }
  if (impute_method=="Naive_Bayes") {
    d_acs <- Naive_Bayes(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                         yvars=yvars, test_filts=filts, train_filts=filts, 
                         weights=weights)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # alternate imputation methods will go here
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # for example:
  
  if (impute_method=="Hocus Pocus") {
    # Hocus pocus function calls here
  }
  
  return(d_acs)
}

# ============================ #
# 1A. KNN1_scratch
# ============================ #
# Define KNN1 matching method

KNN1_scratch <- function(d_train, d_test, imp_var, train_filt, test_filt, xvars) { 
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.

  # create training data
  
  # filter dataset and keep just the variables of interest
  train <-  d_train %>% filter(complete.cases(select(d_train, 'id', imp_var,xvars))) %>% 
    filter_(train_filt) %>%
    select(imp_var, xvars) %>%
    mutate(id = NULL)
  train ['nbor_id'] <- as.numeric(rownames(train))
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  
  test <- d_test %>% filter_(test_filt) %>%
    select(id, xvars) %>%
    filter(complete.cases(.))
  
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
  
  # mark minimium distance
  min_start <- ncol(train)-2
  
  where_min <- function(j) {
    min_dist <- min_start
    d <- mapply(find_dist, x=nest_train, MoreArgs=list(y=j))
    return(which.min(d))
  }
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
  temp <- lapply(nest_test, where_min)
  temp <- unlist(temp)
  temp <- cbind(test["id"],as.data.frame(unlist(temp)))
  colnames(temp)[colnames(temp)=="unlist(temp)"] <- "nbor_id"
  temp <- join(temp[c("id","nbor_id")], train[c("nbor_id",imp_var)], by=c("nbor_id"), type="left")
  temp <- temp[c("id",imp_var)]
  return(temp)
}
# ============================ #
# 1B. logit_leave_method
# ============================ #
# logit imputation of leave characteristics

logit_leave_method <- function(d_test, d_train, xvars, yvars, test_filts, train_filts, 
                               weights, create_dummies) {
  
  # placeholder modification of xvars to follow Chris' specification in python
  # should be removed in final version
  xvars <- c('age', 'agesq', 'male', 'wkhours', 'ltHS', 'BA', 'GradSch', 
             'empgov_fed', 'empgov_st', 'empgov_loc',
             'lnfaminc', 'black', 'asian', 'hisp', 'other',
             'ndep_kid', 'ndep_old', 'nevermarried', 'partner',
             'widowed', 'divorced', 'separated')
  
  # population mean imputation for missing xvars in logit regression
  for (i in xvars) {
    d_train[is.na(d_train[,i]), i] <- 0
    d_test[is.na(d_test[,i]), i] <- mean(d_test[,i], na.rm = TRUE)
  }
  
  # remove prop_pay from lists as we need to use ordinal regression for it
  train_filts <- list.remove(train_filts, 'prop_pay')
  test_filts <- list.remove(test_filts, 'prop_pay')
  yvars <- list.remove(yvars, 'prop_pay')
  weights <- list.remove(weights, 'prop_pay')
  
  # generate formulas for logistic regression
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  
  formulas=c()
  for (i in yvars) { 
    formulas= c(formulas, 
                paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  
  # create columns based on logit estimates  
  sets <-  mapply(runLogitEstimate, formula = formulas, train_filt = train_filts,
                       test_filt=test_filts, weight = weights, varname=yvars,
                       MoreArgs=list(d_train=d_train, d_test=d_test, create_dummies=TRUE), 
                       SIMPLIFY = FALSE)


  # merge imputed values into single data set
  for (i in sets) {
    d_test <- merge(i, d_test, by="id",all.y=TRUE)
    # set missing probability = 0
    d_test[is.na(d_test[colnames(i[2])]), colnames(i[2])] <- 0
  } 
  
  # Do an ordinal logit imputation for prop_pay
  d_filt <- runOrdinalEstimate(d_train=d_train,d_test=d_test, formula=paste("factor(prop_pay) ~",  
                               paste(xvars[1],'+', paste(xvars[2:length(xvars)], collapse=" + "))),
                               test_filt="TRUE", train_filt="TRUE", varname='prop_pay')
  d_test <- merge(d_filt, d_test, by='id', all.y=TRUE)
  
  # replace factor levels with prop_pay proportions
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 1, 0, prop_pay))
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 2, .125, prop_pay))
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 3, .375, prop_pay))
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 4, .5, prop_pay))
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 5, .625, prop_pay))
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 6, .875, prop_pay))
  d_test <- d_test %>% mutate(prop_pay = ifelse(prop_pay == 7, 1, prop_pay))
  
  return(d_test)
}

# ============================ #
# 1Ba. runLogitEstimate
# ============================ #
# function to construct logit estimation model from training data set, 
# then create imputed columns for valid observations in test data set
# returns a separate copy of only those observations with valid imputed values

runLogitEstimate <- function(d_train,d_test, formula, test_filt,train_filt, weight, 
                             varname, create_dummies){
  des <- svydesign(id = ~1,  weights = as.formula(weight), data = d_train %>% filter_(train_filt))
  complete <- svyglm(as.formula(formula),data = d_train %>% filter_(train_filt),
                     family = "quasibinomial",design = des)
  estimate <- complete$coefficients 
  var_prob= paste0(varname,"_prob")
  d_filt <- d_test %>% filter_(test_filt)
  d_filt[var_prob]=estimate['(Intercept)']
  for (dem in names(estimate)) {
    if (dem !='(Intercept)') { 
      d_filt[is.na(d_filt[,dem]),dem]=0
      d_filt[var_prob]= d_filt[,var_prob] + d_filt[,dem]*estimate[dem]
    }
  }
  
  d_filt[var_prob] <- with(d_filt, exp(get(var_prob))/(1+exp(get(var_prob))))
  d_filt <- d_filt[,c(var_prob, 'id')]
  
  # option to create dummy variables in addition to probabilities
  if (create_dummies==TRUE) {
    d_filt [is.na(d_filt[var_prob]), var_prob] <- 0
    d_filt['rand']=runif(nrow(d_filt))
    d_filt[varname] <- with(d_filt, ifelse(rand>get(var_prob),0,1))    
    d_filt <- d_filt[,c(varname, 'id')]
  }
  
  return(d_filt)
}

# ============================ #
# 1C. KNN_multi
# ============================ #
# Define KNN matching method, but allowing multiple (k) neighbors 
KNN_multi <- function(d_train, d_test, imp_var, train_filt, test_filt, xvars, kval) { 
  
  # starts the same way as KNN1_scratch
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.
  
  # create training data
  
  # filter dataset and keep just the variables of interest
  train <-  d_train %>% filter(complete.cases(select(d_train, 'id', imp_var,xvars))) %>% 
    filter_(train_filt) %>%
    select(imp_var, xvars) %>%
    mutate(id = NULL)
  train ['nbor_id'] <- as.numeric(rownames(train))
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  
  test <- d_test %>% filter_(test_filt) %>%
    select(id, xvars) %>%
    filter(complete.cases(.))
  
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
  
  # mark minimium distance
  min_start <- ncol(train)-2
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
  # same as KNN1 up until here
  where_min <- function(j) {
    min_dist <- min_start
    d <- mapply(find_dist, x=nest_train, MoreArgs=list(y=j))
    nbors <- order(d)[1:kval]
    return(nbors)
  }
  
  # create nbors: data set for each test obs' nearest neighbors in train dataset with their index
  nbors <- lapply(nest_test, where_min)
  nbors <- as.data.frame(t(as.data.frame(nbors)))
  # store test id in nbors set before continuing
  rownames(nbors) <- NULL
  colnames(nbors) <- paste0(rep('nbor_id',kval), seq(1,kval))
  nbors['id'] <- test$id
  # add impute variable's values for each index from training data set
  for (i in seq(kval)) {
    nbors <- merge(nbors, train[c('nbor_id',imp_var)], by.x=paste0('nbor_id',i), by.y='nbor_id', how='left')
    colnames(nbors)[kval+i+1] <- c(paste0('nbor_val', i))
  }

  # To decide on a single value to impute:
  # we will pick the mode value among nbors, weighted by distance
  # There are other ways of picking a single value, I'm sure
  
  # throwaway function to get mode of rows
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  nbors[imp_var] <- apply(nbors[(kval+2):(kval*2+1)], 1, Mode)
  temp <- merge(nbors[c('id',imp_var)], test['id'], by='id')
  return(temp)
}

# ============================ #
# 1D. Naive_Bayes
# ============================ #
# Naive Bayes imputation function

Naive_Bayes <- function(d_train, d_test, yvars, train_filts, test_filts, weights, xvars) {
  
  # generate formulas for Naive Bayes model
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  formulas <- c()
  for (i in yvars) { 
    formulas <- c(formulas, 
                paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  names(formulas) <- names(yvars)

  # predict each yvar 
  for (i in names(yvars)) {
    
    # generate NB model from training data
    d_train_filt <- d_train %>% filter(complete.cases(select(d_train, yvars[i], xvars))) %>% filter_(train_filts[i])  
    model <- naiveBayes(x = d_train_filt[xvars], y = d_train_filt[yvars[i]])
    
    # apply model to test data 
    d_test_filt <- d_test %>% filter(complete.cases(select(d_test, xvars))) %>% filter_(test_filts[i]) 
    predict <- predict(object=model, newdata = d_test_filt)
    browser()
  }

  return(d_test)
}