

# """
# Building modifications for policy simulations prior to ACS NN imputation 
#  
# 9 Sept 2018
# Luke
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

policy_simulation <- function(filename, leaveprogram=FALSE, bene_level=1,
                              topoff_rate=0, topoff_minlength=0, bene_effect=0, dependent_allow=0, full_particip_needer=1,
                              d_fmla, d_acs, own_uptake=1, matdis_uptake=1, bond_uptake=1, illparent_uptake=1, 
                              illspouse_uptake=1, illchild_uptake=1) {
  
  # General philosophy behind split between pre-ACS imputation and post-ACS imputation modifications:
  # Do as much as possible post-imputation, and impute as few FMLA variables as possible.
  
  # Pre-imputation: Alterations that are specific to FMLA variables. Mostly leave taking behavior.
  # Post-imputation: Alterations specific to ACS variables, or require both FMLA and ACS variables. 
  #                  Uptake behavior and benefit calculations.
  
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
  
  # creating benefit vars in ACS based on imputations/parameters
  d_acs_imp <- PROGRAM_VARS(d_acs_imp, bene_level, classes=c("own", "illspouse", "illchild","illparent","matdis","bond"))
  
  # Program uptake functions
  if (leaveprogram==TRUE) {
    d_acs_imp <-UPTAKE(d_acs_imp, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                    illspouse_uptake, illchild_uptake, full_particip_needer)
    #d_fmla <- Extend
  }
  
  
  # benefit parameter functions
  # functions are ordered in reverse presedence, highest presedence functions are last;
  # interactions description:
  # BENEFITS: base values for benefits to be modified by other functions
  # BENEFITEFFECT, TOPOFF: TOPOFF overrides participation behavior of BENEFITEFFECT
  # DEPENDENTALLOWANCE: independent of other functions
  
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

# -------------------PROGRAM_VARS------------------
# Adding base values for new ACS variables involving imputed FMLA values

PROGRAM_VARS <- function(d, bene_level, classes) {
  
  # calculate eligibility for program
  # for now, just using full time (30+ hours/wk) = eligible, ineligible otherwise
  d <- d %>% mutate(eligworker= ifelse(WKHP>=30, 1,0))
  d <- d %>% mutate(eligworker= ifelse(is.na(eligworker), 0,eligworker))
  
  # add proportion of pay participation pays out
  d["benefit_prop"] <- bene_level
  
  # calculate general participation decision based on employer pay vs state program pay
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,"prop_pay"]<d[,"benefit_prop"],1,0)
  
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

#---------UPTAKE------------------------------------------------------------------------------------------------------------------------
# specifies uptake rate of those that are eligible for the paid leave program
# default is "full" - all who are eligible and would receive more money than employer would pay
# would pay choose to participate 


UPTAKE <- function(d, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                   illspouse_uptake, illchild_uptake, full_particip_needer) {
  
  # calculate uptake -> days of leave that program benefits are collected
  
  d['particip_length']=0
  d['total_length']=0
  
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    uptake_val=paste(i,"_uptake",sep="")
    d['rand']=runif(nrow(d))
    d <- d %>% mutate(particip_length=ifelse(rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1, 
                                             (particip_length+get(paste('length_',i,sep=""))), particip_length))
    
    # Account for if leave needers always take up benefits when they receive more than their employer pays in leave
    if (full_particip_needer==1) {
      d <- d %>% mutate(particip_length=ifelse(rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1, 
                                               (particip_length+get(paste('length_',i,sep=""))), particip_length))  
    }
    d <- d %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand'))]
  return(d)
}
# -------------------BENEFITS------------------
# Adding base values for new ACS variables involving imputed FMLA values
BENEFITS <- function(d, leaveprogram) {
  # base pay received from employer
  # not dealing with weeks worked yet, need to impute that from CPS.
  # just assuming full time jobs for illustration purposes - 261 working days per year
  # also, only including in base leave pay leaves that participant would use program benefits for.
  
  d["base_leave_pay"] <- (as.matrix(d["WAGP"])/261*as.matrix(d[,'particip_length'])* as.matrix(d[,"prop_pay"]))
  
  if (leaveprogram==TRUE) {
    # base benefits received from program
    d["base_benefits"] <- (as.matrix(d["WAGP"])/261*as.matrix(d[,'particip_length']) * as.matrix(d[,"benefit_prop"]))
  }
  
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
  
  # update leave vars
  d <- d %>% mutate(bene_effect_flg=ifelse(rand>uptake_prob,0,1))
  d <- d %>% mutate(actual_benefits=ifelse(rand>uptake_prob,0,actual_benefits))
  d <- d %>% mutate(particip=ifelse(rand>uptake_prob,0,particip))
  d <- d %>% mutate(particip_length=ifelse(rand>uptake_prob,0,particip_length))
  d <- d[, !(names(d) %in% c('rand','bene_diff','finc_cat','uptake_prob'))]
  
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
  
  d['topoff_rate']=topoff_rate
  d['topoff_min']=topoff_minlength
  d['rand']=runif(nrow(d))
  d <- d %>% mutate(topoff= ifelse(rand<topoff_rate & prop_pay==1,1,0))
  d <- d %>% mutate(topoff_count=0)
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    take_var=paste("take_",i,sep="")
    d['topoff_temp']= ifelse(d['topoff']==1 & d['topoff_min']<=d[len_var] & d[take_var]==1,1,0)
    d <- d %>% mutate(topoff_count= ifelse(topoff_temp==1 ,topoff_count+1,topoff_count))
  }

  
  # overrides benefiteffect changes, since topoff behavior is seen
  d <- d %>% mutate(actual_benefits=ifelse(topoff_count>0,base_benefits,actual_benefits))
  d <- d %>% mutate(actual_leave_pay=ifelse(topoff_count>0,base_leave_pay-base_benefits,actual_leave_pay))
  
  d <- d %>% mutate(topoff_flg= ifelse(topoff_count>0,1,0))
  
  # adjust participation flag. leave taken assumed to not be affected by top off behavior
  d <- d %>% mutate(particip=ifelse(topoff_count>0,1,particip))
  
  d <- d[, !(names(d) %in% c('rand','topoff_rate','topoff_temp','topoff_min','topoff', 'topoff_count'))]
  
  return(d)
}

# -------------------------DEPENDENTALLOWANCE------------------
# include a flat dependent allowance for families with children

DEPENDENTALLOWANCE <- function(d,dependent_allow) {
  d <- d %>% mutate(actual_benefits=ifelse(particip==1 & nochildren==0, actual_benefits+dependent_allow,actual_benefits))
  return(d)
}

