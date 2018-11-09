
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
# 5. impute_fmla_to_acs
    # 5a. KNN1_scratch
    # 5b. impute_leave_length
    # 5c. logit_leave_method

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

  vars_name <- paste0("take_",leave_types)
  
  # leave_count is tracking the number of variables to be imputed. Sorry, probably should have used a better var name
  # Num_leaves_take and the rowsums term are not consistent, hence this check beyond just the observation of
  # only longest and most recent leaves.

  d_fmla['leave_count']=d_fmla['num_leaves_take']- rowSums(d_fmla[,vars_name], na.rm=TRUE)
  d_fmla['long_flag']=0
  for (i in leave_types) {
    take_var <- paste0("take_",i)
    len_var  <- paste0("length_",i)
    long_var <- paste0("long_",i)
    longlen_var <- paste0("longlength_",i)
    
    # flag those whose longest leave is used
    d_fmla['long_flag'] <- with(d_fmla, ifelse(get(long_var)==1 & leave_count>0 & get(take_var)==0,1,long_flag))  
    
    # alter length of 2nd leave type to match longest leave
    d_fmla[len_var] <- with(d_fmla, ifelse(get(long_var)==1 & leave_count>0 & get(take_var)==0,get(longlen_var),get(len_var))) 
    
    # alter take_var of 2nd leave type of longest leave
    d_fmla[take_var] <- with(d_fmla, ifelse(get(long_var)==1 & leave_count>0 & get(take_var)==0,1,get(take_var)))
  }
  
  # ---------------------------------------------------------------------------------------------------------
  # B. Types of Leave taken for multiple leave takers
  # ---------------------------------------------------------------------------------------------------------
  # creating throwaway function to easily repeat this for leave needers
  temp_func <- function(lname){
    # HK: Question - Where in here are the lengths allocated for the imputed leave types?
    # LP: 
    
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
    # HK: Why don't we combine the runLogitEstimate and runLogit Impute Functions?
    # LP:
    
    estimates <- mapply(runLogitEstimate, x = specif, y = conditional, z = weight, 
                        MoreArgs=list(d_in=d_fmla), 
                        SIMPLIFY = FALSE)
    
    # OUTPUT: Lists of regression estimates for each dependent var in specif
    
    # Generate probabilities of each leave type based on logit estimates
    vars_name <- paste(lname,leave_types,sep="_")
    test_conditional <- rep(paste0("num_leaves_", lname,">1 & long_flag==0"),length(vars_name))
    
    # nest estimates for mapply to work properly
    nest_estimate <- lapply(seq(1,length(estimates)), function(y) {estimates[y]} )

    #INPUTS: FMLA data set, lists of regression coefficients, conditional filters for training data,
    #        variables to impute 
    # HK: Luke does this produce 6 columns?
    # LP: Yep
    
    impute <- mapply(runLogitImpute, estimate=nest_estimate, tcond= test_conditional, var_name=vars_name, 
                     MoreArgs=list(d_in=d_fmla,multi_var=TRUE, intra_leave=TRUE), SIMPLIFY = FALSE)
    #OUTPUT: probabilities of each FMLA individual taking/needing a type of leave

    # Run leave type imputation on multi-leave takers based on probability
    # INPUT: FMLA data, probabilities of taking/needing leave type
    d_fmla <- add_leave_types(d_fmla,lname, impute) 
    # OUTPUT:  FMLA data with imputed leave types for those taking/needing multiple leaves, 
    #        of which one or more are not observed in their responses to the FMLA survey
  }
  
  # first run imputes take_* variables 
  # updates d_fmla in place?
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

runLogitImpute <- function(d_in, estimate, var_name, tcond, multi_var=FALSE,intra_leave=FALSE) {

  # calculate a column of probabilities of variable based on individual characteristics
  var_prob= paste0(var_name,"_prob")
  d <- d_in %>% filter_(tcond)
  # unnest estimate if nested. needed to be nested in first place because of mapply
  if (multi_var==TRUE) {
    estimate= estimate[[1]]
  }
  d[var_prob]=estimate['(Intercept)']
  
  for (dem in names(estimate)) {
    if (dem !='(Intercept)') { 
      d[is.na(d[,dem]),dem]=0
      d[var_prob]= d[,var_prob] + d[,dem]*estimate[dem]
    }
  }
  d[var_prob] <- with(d, exp(get(var_prob))/(1+exp(get(var_prob))))
  
  # for some variables, we just need a random generation for single column
  # However, for leave taking/needing imputation, we need to select leave types conditional
  # on the number of leaves to be imputed and the leave types already taken/needed.
  # so for those instances, return_var will be FALSE, and we'll then run the 
  # "add_leave_types" function below.
  if (intra_leave==FALSE) {
    d['rand']=runif(nrow(d))
    d[var_name] <- with(d, ifelse(rand>get(var_prob),0,1))
    keep <- c(var_name, "id")
    d <- d[ , (names(d) %in% keep)]
    if (multi_var==FALSE) {
      d <- merge( d, d_in, by="id",all.y=TRUE)  
    }
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
  vars_name <- paste(lname,leave_types,sep="_")
  
  d['leave_count']=d[paste0('num_leaves_',lname)]- rowSums(d[,vars_name], na.rm=TRUE)
  
  # HK: I see what you're doing here. But I think this would be much easier by just using a multinomial
  # function and the input probabilities.
  # Question: What does this produce? An indicator for each varname of whether that leave was taken? But
  # it seems like all individuals are taking the same number of leaves?
  
  # LP: Let's discuss this, not clear in my head how to change this. 
  # Regarding question, this doesn't produce any individual variables. It is adding some 1's to "take_" vars
  # so that ppl are taking as many different types of leaves as they said in the survey.
  
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
    take_var=paste0("take_",i)
    need_var=paste0("need_",i)
    # I wonder if we should create another dummy variable of whether this person didn't take a leave pre-program
    # HK: Do you mean so we can identify the "extensive margin" of people moving into "take leavers"? Sure!
    # LP: I discuss how to handle later
    
    d[,take_var] <- ifelse(d[,"unaffordable"]==1 & d[,need_var]==1 & !is.na(d[,'unaffordable']) & !is.na(d[,need_var]),1,d[,take_var])
  }
  
  return(d)
}


# ============================ #
# 3. impute_leave_length
# ============================ #
# function to impute leave length once leave taking behavior has been modified
impute_leave_length <- function(d_impute, d_in, conditional, test_cond, fmla) { 
  
  specif <- c(own = "length_own",
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
  predict <- mapply(runRandDraw, y=specif, z=conditional,test_cond=test_cond,
                    MoreArgs = list(d_train=d_impute, d_test=d_in, lname="length_", fmla=fmla),
                    SIMPLIFY = FALSE)
  # Outputs: data sets of imputed leave length values for ACS or FMLA observations requiring them
  
  # merge imputed values with fmla data
  count=0
  for (i in predict) {
    count=count+1
    if (!is.null(i)) {
      d_in <- merge(i, d_in, by="id",all.y=TRUE)  
    }
    else {
      d_in[paste0('length_',leave_types[count])] <- NA
    }
  }  
  
  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste("length",i, sep="_"))
  }
  
  # update leave vars
  # H: hmmm I'm a bit lost here. Let's talk through it. I think a lot of this could be solved with left_join
  # L: I agree this is probably unnecessarily messy. I have a vague memory of trying left_join and something not happening the way I wanted.
  # HK: OK let's meet to figure this out
  # LP: Noting for meeting
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
  # HK: lname doesn't seem to be used?
  # filter training cases
  d_temp <- d_train %>% filter_(z)
  train <- d_temp %>% filter(complete.cases(y))
  
  # filter out test cases
  test <- d_test %>% filter_(test_cond)
  
  if (nrow(test)!=0) {
    # random draw
    if (!y %in% colnames(test)) {
      test[y]=NA
    }
    
    A <- function(x) train[sample(nrow(train), 1), y]
    test[y] <- apply(test[y],1, A)
    
    # For the FMLA intra imputation runs of this, we also know long_length.
    # HK: Is this part of the function doable without the intra length stuff we've discussed?
    # LP:
    
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
}


# ============================ #
# 4. impute_fmla_to_acs
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

  # KNN1 imputation method
  if (impute_method=="KNN1") {
    
    # Save ACS and FMLA Dataframes at this point to document format that alternative imputation methods 
    # will need to expect
    saveRDS(d_fmla, file="./R_dataframes/d_fmla_impute_input.rds") # Remove from final version
    saveRDS(d_acs, file="./R_dataframes/d_acs_impute_input.rds") # Remove from final version
    
    # classes of leave
    # HK: Let's throw the need_leave variable in here to run LEAVEPROGRAM after the FMLA->ACS imputation
    # LP: Rather than imputing need_ vars, can I reverse engineer the extensive margin var from 'unaffordable'
    specif <- c(own = "take_own", 
                illspouse = "take_illspouse",
                illchild = "take_illchild",
                illparent = "take_illparent",
                matdis = "take_matdis",
                bond = "take_bond",
                prop_pay = "prop_pay",
                unaffordable= "unaffordable")
    # H: I forget the logic for these conditionals. We should discuss.
    # L: Sure
    # LP: Note for discussion
    conditional <- c(own = "TRUE",
                     illspouse = "nevermarried == 0 & divorced == 0",
                     illchild = "TRUE",
                     illparent = "TRUE",
                     matdis = "female == 1 & nochildren == 0",
                     bond = "nochildren == 0",
                     prop_pay="TRUE",
                     unaffordable="TRUE")

    
    # separate KNN1 calls for each unique conditional doesn't work because of differing missing values
    
    # INPUTS: variable to be imputed, conditionals to filter training and test data on, FMLA data (training), and
    #         ACS data (test), id variable, and dependent variables to use in imputation
    impute <- mapply(KNN1_scratch, imp_var=specif,train_cond=conditional, test_cond=conditional,
                        MoreArgs=list(d_train=d_fmla,d_test=d_acs,xvars=xvars), SIMPLIFY = FALSE)
    # OUTPUTS: A data set for each leave taking/other variables requiring imputation. Each data set contains id for ACS
    #          observations in the leave variable universe, and a dummy indicating whether that ACS individual
    #          takes that kind of leave of not, as well as a dummy for whether leave needing is due to 
    #          unaffordability, and the proportion of pay received from employer while on leave.
    
   # merge imputed values with acs data
    for (i in impute) {
      d_acs <- merge(i, d_acs, by="id",all.y=TRUE)
    }  
    saveRDS(d_acs, file="./R_dataframes/d_acs_impute_output.rds") # Remove from final version
  }
  
  # Logit estimation of leave taking to compare with Chris' results in Python
  if (impute_method=="logit") {
    # HK: We use logit a lot. Can we just have one logit function, like KNN1_scratch that is continually used instead
    # of redefining many of the functions?
    # LP: We should have a broader discussion about how to handle the secondary imputations needed.
    # As is, logit estimates are used in different ways in different places, requiring different manipulations
    d_acs <- logit_leave_method(d_acs, d_fmla)
  }
  
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
  specif <- c(own = "length_own",
               illspouse = "length_illspouse",
               illchild = "length_illchild",
               illparent = "length_illparent",
               matdis = "length_matdis",
               bond = "length_bond")
  
  #   Leave lengths are the same, except for own leaves, which are instead taken from the distribution of leave takers in FMLA survey reporting 
  #   receiving some pay from state programs

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
# 4A. KNN1_scratch
# ============================ #
# Define KNN1 matching method

KNN1_scratch <- function(d_train, d_test, imp_var, train_cond, test_cond, xvars) { 
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.
  # HK: This actually just adds one column right?
  # LP: yeah, each call only adds one column
  
  # Data manipulation

  # filter fmla m_test
  # filter on conditions, select just the variables we want to impute and the variables used to calculate distance

  # create training data
  
  # filter dataset and keep just the variables of interest
  train <-  d_train %>% filter(complete.cases(select(d_train, 'id', imp_var,xvars))) %>% 
    filter_(train_cond) %>%
    select(imp_var, xvars) %>%
    mutate(id = NULL)
  train ['nbor_id'] <- as.numeric(rownames(train))
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  test <- d_test %>% filter_(test_cond) %>%
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
# 4B. impute_leave_length
# ============================ #
# see function 3


# ============================ #
# 4C. logit_leave_method
# ============================ #
  # logit imputation of leave characteristics
  # following Chris' specification in python

logit_leave_method <- function(d_acs, d_fmla) {
  # population mean imputation for missing xvars in logit regression
  imp_mean= c('age', 'agesq', 'male', 'wkhours', 'ltHS', 'BA', 'GradSch', 
              'empgov_fed', 'empgov_st', 'empgov_loc',
              'lnfaminc', 'black', 'asian', 'hisp', 'other',
              'ndep_kid', 'ndep_old', 'nevermarried', 'partner',
              'widowed', 'divorced', 'separated')
  
  for (i in imp_mean) {
    d_fmla[is.na(d_fmla[,i]), i] <- 0
    d_acs[is.na(d_acs[,i]), i] <- mean(d_acs[,i], na.rm = TRUE)
  }
  
  # classes of leave
  specif <- c(own = "take_own ~ age + agesq + male +  wkhours + ltHS +
              BA + GradSch + empgov_fed + empgov_st + empgov_loc +
              lnfaminc + black + asian + hisp + other +
              ndep_kid + ndep_old + nevermarried + partner +
              widowed + divorced + separated", 
              illspouse = "take_illspouse ~ age + agesq + male +  wkhours + ltHS +
              BA + GradSch + empgov_fed + empgov_st + empgov_loc + 
              lnfaminc + black + asian + hisp + other + 
              ndep_kid + ndep_old + nevermarried + partner +
              widowed + divorced + separated",
              illchild = "take_illchild ~ age + agesq + male +  wkhours + ltHS +
              BA + GradSch + empgov_fed + empgov_st + empgov_loc +
              lnfaminc + black + asian + hisp + other +
              ndep_kid + ndep_old + nevermarried + partner +
              widowed + divorced + separated",
              illparent = "take_illparent ~ age + agesq + male +  wkhours + ltHS +
              BA + GradSch + empgov_fed + empgov_st + empgov_loc +
              lnfaminc + black + asian + hisp + other +
              ndep_kid + ndep_old + nevermarried + partner +
              widowed + divorced + separated",
              matdis = "take_matdis ~ age + agesq + male +  wkhours + ltHS +
              BA + GradSch + empgov_fed + empgov_st + empgov_loc +
              lnfaminc + black + asian + hisp + other +
              ndep_kid + ndep_old + nevermarried + partner +
              widowed + divorced + separated",
              bond = "take_bond ~ age + agesq + male +  wkhours + ltHS + 
              BA + GradSch + empgov_fed + empgov_st + empgov_loc +
              lnfaminc + black + asian + hisp + other +
              ndep_kid + ndep_old + nevermarried + partner +
              widowed + divorced + separated",
              unaffordable= "unaffordable ~ age + agesq + male +  wkhours + ltHS +
                                    BA + GradSch + empgov_fed + empgov_st + empgov_loc +
                                    lnfaminc + black + asian + hisp + other +
                                    ndep_kid + ndep_old + nevermarried + partner +
                                    widowed + divorced + separated")
  
  conditional <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0",
                   unaffordable="TRUE")
  
  # weights
  weight <- c(own = "~ fixed_weight",
              illspouse = "~ fixed_weight",
              illchild = "~ fixed_weight",
              illparent = "~ weight",
              matdis = "~ fixed_weight",
              bond = "~ fixed_weight",
              unaffordable = "~ fixed_weight")
  
  
  
  estimates <- mapply(runLogitEstimate, x = specif, y = conditional, z = weight, 
                      MoreArgs=list(d_in=d_fmla), 
                      SIMPLIFY = FALSE)
  # nest estimates for mapply to work properly
  nest_estimate <- lapply(seq(1,length(estimates)), function(y) {estimates[y]} )
  
  # Generate probabilities of each leave type based on logit estimates
  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste("take",i, sep="_"))
  }
  vars_name= c(vars_name, "unaffordable")
  impute <- mapply(runLogitImpute, estimate=nest_estimate, tcond= conditional, var_name=vars_name, 
                   MoreArgs=list(d_in=d_acs,multi_var=TRUE), SIMPLIFY = FALSE)
  
  # merge imputed values into single data set
  for (i in impute) {
    d_acs <- merge(i, d_acs, by="id",all.y=TRUE)
    # set missing probability = 0
    d_acs[is.na(d_acs[colnames(i[2])]), colnames(i[2])] <- 0
  } 
  
  # Do an ordinal logit imputation for prop_pay
  specif = "factor(prop_pay) ~ age + agesq + male +  wkhours + ltHS +
                    BA + GradSch + empgov_fed + empgov_st + empgov_loc +
                    lnfaminc + black + asian + hisp + other +
                    ndep_kid + ndep_old + nevermarried + partner +
                    widowed + divorced + separated"
  conditional = "TRUE"
  weight= "~ fixed_weight"
  estimate <- runOrdinalEstimate(d_fmla, specif,conditional,weight)
  d_acs <- runOrdinalImpute(d_acs, estimate,"prop_pay","TRUE")
  
  # replace factor levels with prop_pay proportions
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 1, 0, prop_pay))
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 2, .125, prop_pay))
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 3, .375, prop_pay))
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 4, .5, prop_pay))
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 5, .625, prop_pay))
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 6, .875, prop_pay))
  d_acs <- d_acs %>% mutate(prop_pay = ifelse(prop_pay == 7, 1, prop_pay))
  
  return(d_acs)
}

