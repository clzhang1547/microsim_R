

# """
# Building modifications for policy simulations prior to ACS NN imputation 
#  
# 9 Sept 2018
# Luke
#
# ACM parameter functions not implemented:
# PLACEOFWORK: Better implemented in the GUI filtering of ACS data 
# SELFEMPLOYED: Better implemented in the GUI filtering of ACS data 
# GOVERNMENT: Better implemented in the GUI filtering of ACS data 
# WEIGHTFACTOR: Unsure of the value of this function
# CALIBRATE: Unsure of the value of this function
# COMMENT, FILE: Made obsolete by GUI
# MISSINGVALUE: Output command better handled by GUI
# DETAIL: Output command better handled by GUI
# FORMULA/FORMULA2: Seem like very narrow functions, not sure this is the best implementation of this functionality
#
# """

cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


library("dplyr")
library("survey")

#options(error=recover)
options(error=NULL)

#d_acs <- read.csv("ACS_clean.csv")
#d_fmla <- read.csv("fmla_clean_2012.csv")


#------------------------------------
# 0. Procedural Functions 
#------------------------------------

# ----------master function that sets policy scenario, and calls functions to alter FMLA data to match policy scenario---------------------
# default values are all false. In this case, simulation will result be prediction of leave taking in as-is scenario of "absence of program"
# Parameters:
# leaveprogram - FALSE for absence of any leave program, TRUE for leave program with default assumptions
#                assumptions are modified by below parameters
# uptake - uptake calculation approach
# bene_level - proportion of pay received as part of program participation
# bene_effect - 1 to affix behavioral cost to applying to program
#               0 to disable
# topoff_rate, min_length - percent of employer engagement in top-off substitution of paid leave with program benefits
# dependent_allow - weekly dependent allowance for those with children
# full_particip_needer - whether or not leave needers always take up benefits. default is yes (1)
# needer_uptake - whether (0) leave needers follow specified uptake parameters of the general population, or (1) if 
#                 leave needers always take up benefits in the presence of the program.
# [type]_uptake - user-supplied benefit uptake rate for a given type of leave if . 1 is full uptake
# waiting_period - how long in working days must leave takers wait to claim leave benefits
# clone_factor - number of clones to create. Number is proportion of original ACS sample to clone. i.e. .1 clones 10% of the original sample.
# ext_base_effect - standard leave extension effect from ACM model
# extend_prob, extend_days, extend_prop - additional leave extension effect parameters: probability of extension, fixed days of extension,
# proportionate extension respectively
# maxlen_type - max number of days benefits can be claimed in a year
# week_bene_cap - max weekly benefits that can be collected
# fmla_protect - Indicates whether or not leaves that are extended in the presence of a program that
#     originally were less than 12 weeks in length are constrained to be no longer than
#     12 weeks in the presence of the program
# random_seed - set random seed if user wishes analyses to be replicable 


policy_simulation <- function(filename, leaveprogram=FALSE, bene_level=1,
                              topoff_rate=0, topoff_minlength=0, bene_effect=0, dependent_allow=0, full_particip_needer=1,
                              d_fmla, d_acs, own_uptake=1, matdis_uptake=1, bond_uptake=1, illparent_uptake=1, 
                              illspouse_uptake=1, illchild_uptake=1, extend_leaves=0,wait_period=0,
                              clone_factor=0, ext_base_effect=TRUE, extend_prob=0, extend_days=0, extend_prop=1,
                              maxlen_own =60, maxlen_matdis =60, maxlen_bond =60, maxlen_illparent =60, maxlen_illspouse =60, maxlen_illchild =60,
                              week_bene_cap=1000000, fmla_protect=TRUE, random_seed=NULL) {
  
  # General philosophy behind split between pre-ACS imputation and post-ACS imputation modifications:
  # Do as much as possible post-imputation, and impute as few FMLA variables as possible.
  
  # Pre-imputation: Alterations that are specific to FMLA variables. Mostly leave taking behavior.
  # Post-imputation: Alterations specific to ACS variables, or require both FMLA and ACS variables. 
  #                  Uptake behavior and benefit calculations.
  
  # set random seed option
  
  if (!missing(random_seed)) {
    set.seed(random_seed)
  }
  
  
  # ----------------pre-imputation functions---------------
  # preserve original copy of FMLA survey
  d_fmla_orig <- d_fmla 

  # intra-fmla imputation for additional leave taking and lengths
  # see 4a. file for below function
  d_fmla <- intra_impute(d_fmla_orig)

  # In presence of program, apply leave-taking behavioral updates
  if (leaveprogram==TRUE) {
    d_fmla <-LEAVEPROGRAM(d_fmla)
  }
  
  # after all parameters have been accounted for, add/remove leave lengths for leaves taken/removed

  test_conditional <- c(own = "(take_own==1 & length_own==0)|(take_own==0 & length_own!=0 & is.na(length_own)==FALSE)", 
                        illspouse = "(take_illspouse==1 & length_illspouse==0)|(take_illspouse==0 & length_illspouse!=0 & is.na(length_illspouse)==FALSE)",
                        illchild = "(take_illchild==1 & length_illchild==0)|(take_illchild==0 & length_illchild!=0 & is.na(length_illchild)==FALSE)",
                        illparent = "(take_illparent==1 & length_illparent==0)|(take_illparent==0 & length_illparent!=0 & is.na(length_illparent)==FALSE)",
                        matdis = "(take_matdis==1 & length_matdis==0)|(take_matdis==0 & length_matdis!=0 & is.na(length_matdis)==FALSE)",
                        bond = "(take_bond==1 & length_bond==0)|(take_bond==0 & length_bond!=0 & is.na(length_bond)==FALSE)")
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

  # see 4a. file for below function
  d_fmla <- impute_leave_length(d_fmla_orig, d_fmla, conditional, test_conditional)


  #-------------FMLA imputation------------------------------
  
  # currently just simple nearest neighbor, K=1 
  # see 4. file for below function
  d_acs_imp <- fmla_impute(filename,d_fmla,d_acs,leaveprogram)
  
  # -------------Post-imputation functions-----------------
  
  # function interactions description (may not be complete, just writing as they come to me):
  # UPTAKE: participation initially set, but can be overriden by later functions
  # BENEFITS: base values for benefits to be modified by other functions
  # BENEFITEFFECT: Overrides UPTAKE participation, but there are some classes of participants not affected by this
  # TOPOFF: TOPOFF overrides participation behavior of BENEFITEFFECT
  # DEPENDENTALLOWANCE: independent of other functions
  
  
  # Allow for users to clone ACS individuals 
  d_acs_imp <- CLONEFACTOR(d_acs_imp, clone_factor)
  
  # creating benefit vars in ACS based on imputations/parameters
  d_acs_imp <- PROGRAM_VARS(d_acs_imp, classes=c("own", "illspouse", "illchild","illparent","matdis","bond"), leaveprogram)
  
  # After other benefits/participation decisions made, flag those who 
  # will have exhausted employer benefits with leave remaining, and will apply to program for remainder
  d_acs_imp <- PAY_SCHEDULE(d_acs_imp)
  
  # Option to extend leaves under leave program 
  if (leaveprogram==TRUE & extend_leaves==1) {
    d_acs_imp <- EXTENDLEAVES(d_fmla, d_acs_imp, wait_period, ext_base_effect, 
                              extend_prob, extend_days, extend_prop, fmla_protect)  
  } 
  
  # Program uptake functions
  if (leaveprogram==TRUE) {
    d_acs_imp <-UPTAKE(d_acs_imp, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                    illspouse_uptake, illchild_uptake, full_particip_needer, wait_period, bene_level, 
                    maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild)
  }
  
  if (leaveprogram==FALSE) {
    d_acs_imp["benefit_prop"] <- 0
    d_acs_imp["particip"] <- 0
    d_acs_imp["particip_length"] <- 0
  }
  
  # benefit parameter functions
  
  d_acs_imp <- BENEFITS(d_acs_imp, leaveprogram)
  
  if (leaveprogram==TRUE & bene_effect==1) {
    d_acs_imp <- BENEFITEFFECT(d_acs_imp)
  }
  
   if (leaveprogram==TRUE & topoff_rate>0) {
     d_acs_imp <- TOPOFF(d_acs_imp,topoff_rate, topoff_minlength)
   }
  
  if (leaveprogram==TRUE & dependent_allow>0) {
    d_acs_imp <- DEPENDENTALLOWANCE(d_acs_imp,dependent_allow)
  }
  
  # final clean up 
  d_acs_imp <- CLEANUP(d_acs_imp, week_bene_cap)
  
  return(d_acs_imp)
}


#------------------------------------
# 1. Pre-imputation Leave Parameter Functions [on FMLA dataset]
#------------------------------------

#-----------global values across functions------------
leave_types=c("own","illspouse","illchild","illparent","matdis","bond")


# -----------LEAVEPROGRAM-----------------------------------------------------------------------------------------------------------------
# Baseline changes for addition of a leave program
# follows baseline changes of ACM model (see p.11 of ACM model description paper). Main change needed to base cleaning:
#   Leave needers who did not take a leave in the absence of a program, and
#   who said the reason that they did not take a leave was because they could not afford to
#   take one, take a leave in the presence of a program.

LEAVEPROGRAM <- function(d) {
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    need_var=paste("need_",i,sep="")
    d[,take_var] <- ifelse(d[,"unaffordable"]==1 & d[,need_var]==1,1,d[,take_var])
  }

  return(d)
}


#------------------------------------
# 2. Post-imputation Leave Parameter Functions [on post-imputation ACS data set]
#------------------------------------

#-------------------------CLONEFACTOR------------------
# allow users to clone ACS individuals

CLONEFACTOR <- function(d, clone_factor) {
  if (clone_factor > 0) {
    d$clone_flag=0
    num_clone <- round(clone_factor*nrow(d), digits=0)
    d_clones <- data.frame(sample(d$empid,num_clone,replace=TRUE))
    colnames(d_clones)[1] <- "empid"
    d_clones <- join(d_clones,d,by='empid', type='left')
    d_clones$clone_flag=1
    d <- rbind(d,d_clones)
  }
  return(d)
}


# -------------------PROGRAM_VARS------------------
# Adding base values for new ACS variables involving imputed FMLA values

PROGRAM_VARS <- function(d, classes, leaveprogram) {
  
  # calculate eligibility for program
  # for now, just using full time (30+ hours/wk) = eligible, ineligible otherwise
  d <- d %>% mutate(eligworker= ifelse(WKHP>=30, 1,0))
  d <- d %>% mutate(eligworker= ifelse(is.na(eligworker), 0,eligworker))

  # replace leave taking and length NA's with zeros now
  # wanted to distinguish between NAs and zeros in FMLA survey, 
  # but no need for that in ACS now that we're "certain" of ACS leave taking behavior 
  # We are "certain" because we only imputed leave takers/non-takers, discarding those with 
  # uncertain/ineligible status (take_[type]=NA).
  
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    take_var=paste("take_",i,sep="")
    d[len_var] <- with(d, ifelse(is.na(get(len_var)),0,get(len_var)))
    d[take_var] <- with(d, ifelse(is.na(get(take_var)),0,get(take_var)))
  }
  
  return(d)
} 

# -------------------PAY_SCHEDULE------------------
# Calculate pay schedule for employer paid leave

PAY_SCHEDULE <- function(d) {
  
  # two possible pay schedules: paid the same amount each week, or paid in full until exhausted
  # Here we randomly assign one of these three pay schedules 
  # based on conditional probabilities of total pay received and pay schedules 
  # probabilities are obtained from 2001 Westat survey which ACM used for this purpose
  # dist <- read.csv("pay_dist_prob.csv")
  
  # columns from this csv written manually to avoid dependency on csv file
  # proportion of pay received (prop_pay in FMLA data)
  # Westat 2001 survey: About how much of your usual pay did you receive in total?
  Total_paid=c("Less than half","Half","More than half")
  
  # Prob of 1st pay schedule - some pay, all weeks
  # Westat 2001 survey: Receive receive some  pay for each pay period  that you were on leave?
  Always_paid=c(0.6329781, 0.8209731, 0.9358463)
  
  # Prob of 2nd pay schedule - full pay, all weeks
  # Westat 2001 survey: If not, when you did receive pay, was it for your full salary?
  Fully_paid=c(0.3273122,0.3963387,0.3633615)
  
  # Prob of 3rd pay schedule - some pay, some weeks
  # Neither paid each pay period, nor receive full pay when they did receive pay.
  Neither_paid=1-Fully_paid
  
  d_prob=data.frame(Total_paid,Always_paid,Fully_paid,Neither_paid)
  
  # denote bucket of proportion of pay
  d <- d %>% mutate(Total_paid= ifelse(prop_pay>0 & prop_pay<.5,"Less than half",NA))
  d <- d %>% mutate(Total_paid= ifelse(prop_pay==.5, "Half" ,Total_paid))
  d <- d %>% mutate(Total_paid= ifelse(prop_pay>.5 & prop_pay<1, "More than half",Total_paid))
  
  # merge probabilities in
  d <- join(d,d_prob, type="left",match="all",by=c("Total_paid"))
  
  # assign pay schedules
  d['rand']=runif(nrow(d))
  d['rand2']=runif(nrow(d))
  
  d <- d %>% mutate(pay_schedule= ifelse(rand<Always_paid,"some pay, all weeks",NA))
  d <- d %>% mutate(pay_schedule= ifelse(rand>=Always_paid & rand2<Fully_paid,"all pay, some weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(rand>=Always_paid & rand2>=Fully_paid,"some pay, some weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(prop_pay==1,"all pay, all weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(prop_pay==0,"no pay",pay_schedule))
  
  # total_length - number of days leave taken of all types
  d['total_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d <- d %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  # count up number of types of leaves
  d['total_leaves']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d <- d %>% mutate(total_leaves = ifelse(get(paste(take_var))==1, total_leaves+1,total_leaves))
  }
  
  # Keep track of what day employer benefits will be exhausted for those receiving pay in some but not all of their leave
  # all pay, some weeks
  d <-  d %>% mutate(exhausted_by=ifelse(pay_schedule=="all pay, some weeks",round(total_length*prop_pay, digits=0), NA))
  
  # some pay, some weeks - like ACM, assumes equal distribution of partiality among pay proportion and weeks taken
  d <-  d %>% mutate(exhausted_by=ifelse(pay_schedule=="some pay, some weeks",round(total_length*sqrt(prop_pay), digits=0), exhausted_by))
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand','rand2','Always_paid','Total_paid','Fully_paid', 'Neither_paid'))]
  
  return(d)
}


# -------------------EXTENDLEAVES------------------
# Option to simulate extension of leaves in the presence of an FMLA program

EXTENDLEAVES <-function(d_train, d_test,wait_period, ext_base_effect, 
                        extend_prob, extend_days, extend_prop, fmla_protect) {
  
  # copy original leave lengths
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    orig_var=paste("orig_len_",i,sep="")
    d_test[orig_var] <- with(d_test, get(len_var))
  }

  
  # Base extension effect from ACM model (referred to as the "old" extension simulation there)
  if (ext_base_effect==TRUE) { 
  

    
    # specifications
    # using ACM specifications
    specif <- c("longerLeave ~ age + agesq + female")
    
    # subsetting data
    conditional <- c("TRUE")
    
    test_conditional <- c("TRUE")
    
    # weights
    weight <- c("~ fixed_weight")
    
    # Run Estimation
    # see file 4a. for below functions
    estimates <- runLogitEstimate(d_train, specif,conditional, weight)
    
    
    # calculate a column of probabilities of each leave type based on individual characteristics
    var_prob= "longerLeave_prob"
    model=estimates
    d_test[var_prob]=model[[1]][["(Intercept)"]]
    for (dem in names(model[[1]])) {
      if (dem !='(Intercept)') { 
        d_test[is.na(d_test[,dem]),dem]=0
        d_test[,var_prob]= d_test[,var_prob] + d_test[,dem]*model[[1]][[dem]]
      }
      d_test[,var_prob]=exp(d_test[,var_prob])/(1+exp(d_test[,var_prob]))
    }
    
    # randomly determine if longer leave is taken
    
    d_test['rand']=runif(nrow(d_test))
    d_test <- d_test %>% mutate(longer_leave=ifelse(longerLeave_prob>rand,1,0))
  
    
    # Following ACM implementation:
    # i. For workers who have leave lengths in the absence of a program that are
    # less than the waiting period for the program: the leave is extended for 1 week into the program.
    
    d_test["extend_flag"]=0
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      d_test["extend_flag"] <- with(d_test, ifelse(get(len_var)<wait_period & eligworker==1 &
                                                   longer_leave == 1 & get(take_var)==1
                                                   ,1,extend_flag))
      d_test[len_var] <- with(d_test, ifelse(get(len_var)<wait_period & eligworker== 1 &
                                               longer_leave == 1 & get(take_var)==1
                                             ,get(len_var)+wait_period+5,get(len_var)))
      d_test["total_length"] <-  with(d_test, ifelse(get(len_var)<wait_period & eligworker== 1 &
                                                       longer_leave == 1 & get(take_var)==1
                                                     ,total_length+wait_period+5, total_length))
    }
    
    # ii. For workers who do not receive any employer pay or who exhaust their
    # employer pay and then go on the program: The probability of extending a leave using
    # program benefits is set to 25 percent; and for those who do extend their leave, the
    # extension is equal to 25 percent of their length in the absences of a program.
    d_test['rand']=runif(nrow(d_test))
    d_test <- d_test %>% mutate(longer_leave=ifelse(.25>rand,1,0))
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      d_test["extend_flag"] <- with(d_test, ifelse((prop_pay==0 | !is.na(exhausted_by)) & eligworker==1 &
                                                     longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                                   ,1,extend_flag))
      d_test[len_var] <- with(d_test, ifelse((prop_pay==0 | !is.na(exhausted_by)) & eligworker==1 &
                                               longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                             ,get(len_var)*1.25,get(len_var)))
      d_test["total_length"] <-  with(d_test, ifelse((prop_pay==0 | !is.na(exhausted_by)) & eligworker==1 &
                                                       longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                                     ,total_length+get(len_var)*.25, total_length))
    }
  
    # iii. For workers who exhaust program benefits and then receive employer pay:
    #   In this case the simulator assigns a 50 percent probability of taking an extended leave
    # until their employer pay is exhausted.
    
    # Not implemented, don't really get why this would be allowed or with what probability if it was
    
    # clean up vars
    d_test <- d_test[, !(names(d_test) %in% c("longerLeave_prob"))]
  } 
  
  # Additional option to extend leave a+bx additional days with c probability if the user wishes. 
    # a = extend_days
    # b = extend_prop
    # c = extend_prob
  # simplified from the ACM model; there they allowed it to be customized by leave type, just allowing for overall adjustments for now.
  

  if (extend_prob > 0) {
    d_test['rand']=runif(nrow(d_test))
    d_test["extend_flag"] <- with(d_test, ifelse(rand<extend_prob & eligworker==1 & total_length!=0,1,extend_flag))
    
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      d_test[len_var] <- with(d_test, ifelse(rand<extend_prob & eligworker==1 & get(paste(len_var))!=0,
                                             round(get(paste(len_var))*extend_prop),get(paste(len_var))))
      d_test[len_var] <- with(d_test, ifelse(rand<extend_prob & eligworker==1 & get(paste(len_var))!=0,
                                             round(get(paste(len_var))+(extend_days/total_leaves)),get(paste(len_var))))
    }

    # clean up vars
    d_test <- d_test[, !(names(d_test) %in% c("rand","extend_amt"))]
  }
  
  # FMLA Protection Constraint option
  # If enabled, leaves that are extended in the presence of a program that
  # originally were less than 12 weeks in length are constrained to be no longer than
  # 12 weeks in the presence of the program.
  
  if (fmla_protect==TRUE) {
    d_test["fmla_constrain_flag"] <- 0
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      orig_var=paste("orig_len_",i,sep="")
      d_test["fmla_constrain_flag"] <- with(d_test, ifelse(extend_flag==1 & get(len_var)>60 & get(orig_var)<=60
                                                           ,1,fmla_constrain_flag))
      d_test[len_var] <- with(d_test, ifelse(extend_flag==1 & get(len_var)>60 & get(orig_var)<=60
                                             ,60,get(len_var)))
    }
  }
  
  # adjust total_length to match extensions of individual leaves
  d_test['total_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d_test <- d_test %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  return(d_test)
}


#---------UPTAKE------------------------------------------------------------------------------------------------------------------------
# specifies uptake rate of those that are eligible for the paid leave program
# default is "full" - all who are eligible and would receive more money than employer would pay
# would pay choose to participate 


UPTAKE <- function(d, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                   illspouse_uptake, illchild_uptake, full_particip_needer, wait_period, bene_level, 
                   maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild) {
  
  # calculate general participation decision based on employer pay vs state program pay
  
  d["benefit_prop"] <- bene_level
  d["particip"] <- 0
  
  # those who will receive more under the program will participate
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,"prop_pay"]<d[,"benefit_prop"],1,0)  
  
  # those who exhaust employer benefits before leave ends will participate
  d["particip"] <- ifelse(d[,"eligworker"]==1 & !is.na(d[,'exhausted_by']),1,d[,"particip"])  
  
  # those who choose to extend leaves in the presence of the program will participate
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,'extend_flag']==1,1,d[,"particip"])  
  
  # calculate uptake -> days of leave that program benefits are collected
  d['particip_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    uptake_val=paste(i,"_uptake",sep="")
    plen_var= paste("plen_",i, sep="")
    d['rand']=runif(nrow(d))
    d <- d %>% mutate(particip_length=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                             rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1, 
                                             particip_length+get(paste('length_',i,sep=""))-wait_period, particip_length))
    d[plen_var] <- with(d, ifelse(wait_period<get(paste('length_',i,sep="")) &
                                               rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1, 
                                             get(paste('length_',i,sep=""))-wait_period, 0))
    d <- d %>% mutate(change_flag=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                               rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1,1,0))
    
    # Option for if leave needers always take up benefits when they receive more than their employer pays in leave
    if (full_particip_needer==1) {
      d <- d %>% mutate(particip_length=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                               rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1, 
                                               particip_length+get(paste('length_',i,sep=""))-wait_period, particip_length))
      d[plen_var] <- with(d, ifelse(wait_period<get(paste('length_',i,sep="")) &
                                      rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1, 
                                    get(paste('length_',i,sep=""))-wait_period, get(plen_var)))
      d <- d %>% mutate(change_flag=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                                 rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1,1, change_flag))  
    }
    
    # subtract days spent on employer benefits from those that exhausting employer benefits (received pay for some days of leave)
    # Also accounting for wait period here, as that can tick down as a person is still collecting employer benefits
    d <- d %>% mutate(particip_length= ifelse(change_flag==1 & !is.na(exhausted_by),
                                              ifelse(get(paste('length_',i,sep="")) > exhausted_by & exhausted_by>wait_period, 
                                                     particip_length - exhausted_by + wait_period, particip_length), particip_length))
    d[plen_var] <- with(d, ifelse(change_flag==1 & !is.na(exhausted_by),
                                  ifelse(get(paste('length_',i,sep="")) > exhausted_by & exhausted_by>wait_period, 
                                         get(plen_var) - exhausted_by + wait_period, get(plen_var)), get(plen_var)))
  }
  
  # make sure those with particip_length 0 are also particip 0
  d <- d %>% mutate(particip= ifelse(particip_length==0,0, particip))

  # cap particip_length at max program days
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    max_val=paste("maxlen_",i,sep="")
    d[plen_var] <- with(d, ifelse(get(plen_var)>get(max_val),get(max_val), get(plen_var)))
  }
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand', 'change_flag'))]
  return(d)
}
# -------------------BENEFITS------------------
# Adding base values for new ACS variables involving imputed FMLA values

BENEFITS <- function(d, leaveprogram) {

  if (leaveprogram==TRUE) {
    # base benefits received from program
    d <- d %>% mutate(base_benefits=WAGP/(round(weeks_worked*5))*particip_length* benefit_prop,0)
  }
  
  # base pay received from employer based on schedule
  # pay received is same across all pay schedules
  d <- d %>% mutate(base_leave_pay=WAGP/(round(weeks_worked*5))*total_length* prop_pay,0)
  
  # actual pay and benefits - to be modified by remaining parameter functions
  d <- d %>% mutate(actual_leave_pay=base_leave_pay)
  if (leaveprogram==TRUE) {
    d <- d %>% mutate(actual_benefits=base_benefits)
  }
  
  
  
  return(d)
}
# -------------------BENEFITEFFECT------------------
# Accounting for some "cost" of applying for the program when deciding between employer paid leave and program
# probabilities are obtained from 2001 Westat survey which ACM used for this purpose

# NEEDS BUG TESTING/EVALUATION

BENEFITEFFECT <- function(d) {
  d_prob <- read.csv("bene_effect_prob.csv")
  
  # define benefit difference to match 2001 Westat survey categories
  d <- d %>% mutate(bene_diff=(actual_benefits-actual_leave_pay)/particip_length*5)
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=25, 0, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=50 & bene_diff>25, 25, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=125 & bene_diff>50, 50, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff>125, 125, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(is.na(bene_diff), 0, bene_diff))
  d['bene_diff']=as.integer(d[,'bene_diff'])
  
  # PLACEHOLDER dealing with missing faminc values for the test ACS data set.
  # need to come up with systematic way of addressing missing values in ACS eventually
  d <- d %>% mutate(faminc=ifelse(is.na(faminc),WAGP,faminc))
  
  # define family income to match 2001 Westat survey categories
  d <- d %>% mutate(finc_cat=ifelse(faminc<=10000,10000,NA))
  inc_cut <- seq(10000, 90000, by=10000)
  for (i in inc_cut) {
    d <- d %>% mutate(finc_cat=ifelse(faminc>i & faminc<=i+10000,i,finc_cat))
  }
  d <- d %>% mutate(finc_cat=ifelse(faminc>100000,100000,finc_cat))
  
  d['finc_cat']=as.numeric(d[,'finc_cat'])
  
  # recalculate uptake based on bene_diff
  d <- join(d,d_prob, type="left",match="all",by=c("bene_diff", "finc_cat"))
  d['rand']=runif(nrow(d))
  
  # exclude those participants that will not be affected by benefit effect
  # those who exhaust employer benefits before leave ends will always participate
  d["universe"] <- ifelse(d[,"eligworker"]==1 & !is.na(d[,'exhausted_by']),0,1)  
  
  # those who choose to extend leaves in the presence of the program will always participate
  d["universe"] <- ifelse(d[,"eligworker"]==1 & d[,'extend_flag']==1,0,d[,'universe']) 
  
  # flag those who are not claiming benefits due to benefit effect
  d <- d %>% mutate(bene_effect_flg=ifelse(rand>uptake_prob & particip==1 & universe==1,1,0))
  
  # update leave vars
  d <- d %>% mutate(actual_benefits=ifelse(rand>uptake_prob & particip==1 & universe==1,0,actual_benefits))
  d <- d %>% mutate(particip=ifelse(rand>uptake_prob & particip==1 & universe==1,0,particip))
  d <- d %>% mutate(particip_length=ifelse(rand>uptake_prob & particip==1 & universe==1,0,particip_length))
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    d[plen_var] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,plen_var))
  }
  d <- d[, !(names(d) %in% c('rand','bene_diff','finc_cat','uptake_prob','universe'))]
  
  return(d)
}

# -------------------TOPOFF------------------
# employers who would pay their employees
# 100 percent of wages while on leave would instead require their employees to participate
# in the program and would "top-off" the program benefits by paying the difference
# between program benefits and full pay. 
# User can specify percent of employers that engage in this, and minimum length of leave this is required for
 
TOPOFF <- function(d, topoff_rate, topoff_minlength) {
  len_vars <- c("length_own", "length_illspouse", "length_illchild","length_illparent","length_matdis","length_bond")
  
  d['topoff_rate'] <- topoff_rate
  d['topoff_min'] <- topoff_minlength
  d['rand'] <- runif(nrow(d))
  d <- d %>% mutate(topoff= ifelse(rand<topoff_rate & prop_pay==1,1,0))
  d <- d %>% mutate(topoff_count=0)
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    take_var=paste("take_",i,sep="")
    d['topoff_temp'] <- with(d,ifelse(topoff==1 & topoff_min<=get(paste(len_var)) & get(paste(take_var))==1,1,0))
    d <- d %>% mutate(topoff_count= ifelse(topoff_temp==1 ,topoff_count+1,topoff_count))
  }

  # note: topoff will override benefiteffect changes
  d <- d %>% mutate(actual_benefits=ifelse(topoff_count>0,base_benefits,actual_benefits))
  d <- d %>% mutate(actual_leave_pay=ifelse(topoff_count>0,base_leave_pay-actual_benefits,actual_leave_pay))
  
  d <- d %>% mutate(topoff_flg= ifelse(topoff_count>0,1,0))
  
  # adjust participation flag. leave taken assumed to not be affected by top off behavior
  d <- d %>% mutate(particip=ifelse(topoff_count>0,1,particip))
  
  d <- d[, !(names(d) %in% c('rand','topoff_rate','topoff_temp','topoff_min','topoff', 'topoff_count'))]
  
  return(d)
}

# -------------------------DEPENDENTALLOWANCE------------------
# include a flat weekly dependent allowance for families with children

DEPENDENTALLOWANCE <- function(d,dependent_allow) {
  d <- d %>% mutate(actual_benefits=ifelse(particip==1 & nochildren==0, actual_benefits+(dependent_allow)*particip_length/5,actual_benefits))
  return(d)
}

# -------------------------CLEANUP------------------
# Final variable alterations and consistency checks

CLEANUP <- function(d, week_bene_cap) {
  # make sure those with particip_length 0 are also particip 0
  d <- d %>% mutate(particip= ifelse(particip_length==0,0, particip))
  
  # cap benefit payments at program's weekly benefit cap
  d <- d %>% mutate(actual_benefits= ifelse(actual_benefits>week_bene_cap*particip_length/5,
                                           week_bene_cap*particip_length/5, actual_benefits))
  
  # calculate leave specific benefits
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    ben_var=paste("bene_",i,sep="")  
    d[ben_var] <- with(d, actual_benefits*(get(len_var)/total_length))
    d[ben_var] <- with(d, ifelse(is.na(get(ben_var)),0,get(ben_var)))
  }
  
  return(d)
}
