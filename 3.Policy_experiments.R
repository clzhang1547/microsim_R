
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
#                 assumptions are modified by below parameters"
# uptake - uptake calculation approach
# bene_level - proportion of pay received as part of program participation
# bene_effect - TRUE to affix behavioral cost to applying to program
#                FALSE to disable
# topoff_rate, min_length - percent of employer engagement in top-off substitution of paid leave with program benefits
# dependent_allow - weekly dependent allowance for those with children
# needer_uptake - whether (0) leave needers follow specified uptake parameters of the general population, or (1) if 
# leave needers always take up benefits in the presence of the program.

policy_simulation <- function(filename, leaveprogram=FALSE, uptake="full", bene_level=1,
                              topoff_rate=0, topoff_minlength=0, bene_effect=1, dependent_allow=0, full_particip_needer=1,
                              d_fmla, d_acs, own_uptake=1, matdis_uptake=1, bond_uptake=1, illparent_uptake=1, 
                              illspouse_uptake=1, illchild_uptake=1, own_prob=1,matdis_prob=1,bond_prob=1,
                              illparent_prob=1,illspouse_prob=1, illchild_prob=1) {
  # ----------------pre-imputation functions---------------
  # preserve original copy of FMLA survey
  d_fmla_orig <- d_fmla 
  
  # intra-fmla imputation for additional leave taking and lengths
  d_fmla <- intra_impute(d_fmla_orig)

  # Leave program behavioral updates
  if (leaveprogram==TRUE) {
    d_fmla <-LEAVEPROGRAM(d_fmla)
    d_fmla <-BENEFITS(d_fmla, bene_level)
    d_fmla <-UPTAKE(d_fmla, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                    illspouse_uptake, illchild_uptake, full_particip_needer)
    #d_fmla <- Extend
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
  d_fmla <- impute_leave_length(d_fmla_orig, d_fmla, conditional, test_conditional)

  #-------------FMLA imputation------------------------------
  # currently just simple nearest neighbor, K=1 
  filename <- fmla_impute(filename,d_fmla,d_acs,leaveprogram)
  
  # -------------Post-imputation functions-----------------
  
  # adjust leave taking behavior per user specified values
  
  #d_fmla <- LEAVEPROBABILITYFACTORS(illchild_prob,own_prob, matdis_prob, bond_prob, illparent_prob, 
  #                                  illspouse_prob, illchild_prob)
  
  # creating benefit vars in ACS based on imputations/parameters
  filename <- update_benefits(filename,classes=c("own", "illspouse", "illchild","illparent","matdis","bond"))
  
  # benefit parameter functions
  # functions are ordered in reverse presedence, highest presedence functions are last;
  # interactions description:
  # BENEFITEFFECT, TOPOFF: TOPOFF overrides participation behavior of BENEFITEFFECT
  # DEPENDENTALLOWANCE: independent of other functions
  
  if (leaveprogram==TRUE & bene_effect==1) {
    filename <- BENEFITEFFECT(filename)
  }
  
   if (leaveprogram==TRUE & topoff_rate>0) {
     filename <- TOPOFF(filename,topoff_rate, topoff_minlength)
   }
  
  if (leaveprogram==TRUE & dependent_allow>0) {
    filename <- DEPENDENTALLOWANCE(filename,dependent_allow)
  }
  return(filename)
  
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
    d[,take_var] <- ifelse(d[,"A62a"]==1 & d[,need_var]==1,1,d[,take_var])
  }

  return(d)
}

#---------BENEFITS------------------------------------------------------------------------------------------------------------------------
# Specifies the benefit schedule.
# default is full pay
BENEFITS <- function(d,bene_level) {
  
  # base benefits of all participating individuals to proportion of pay
  d[,"benefit_prop"] <- bene_level
  
  return(d)
}


#---------UPTAKE------------------------------------------------------------------------------------------------------------------------
# specifies uptake rate of those that are eligible for the paid leave program
# default is "full" - all who are eligible and would receive more money than employer would pay
# would pay choose to participate 

UPTAKE <- function(d, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                   illspouse_uptake, illchild_uptake, full_particip_needer) {
  
  # calculate general participation decision based on employer pay vs state program pay
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,"prop_pay"]<d[,"benefit_prop"],1,d[,"particip"])

  # calculate uptake -> days of leave that program benefits are collected

  for (i in leave_types) {
    uptake_val=paste(i,"_uptake",sep="")
    rand=runif(1)
    if (rand<get(uptake_val)) {
      d <- d %>% mutate(particip_length=ifelse(particip==1, (particip_length+get(paste('length_',i,sep=""))), particip_length))
    }
    d <- d %>% mutate(total_length=total_length+get(paste('length_',i,sep="")))
  }
  
  # NEEDS RUNTIME TESTING
  # specify if leave needers always take up benefits
  if (full_particip_needer==1) {
    for (i in leave_types) {
      take_var=paste("take_",i,sep="")
      need_var=paste("need_",i,sep="")
      d <- d %>% mutate(particip_length=ifelse(particip==1 & d[,"A62a"]==1 & d[,need_var]==1 & d[,take_var] == 0, 
                                               (particip_length+get(paste('length_',i,sep=""))), particip_length))
      d[,take_var] <- ifelse(d[,"A62a"]==1 & d[,need_var]==1,1,d[,take_var])
    }
  }
  
  return(d)
}


#------------------------------------
# 2. Post-imputation Leave Parameter Functions [on post-imputation ACS data set]
#------------------------------------


# -------------------acs_leave_vars------------------
# Adding new variables with imputed variables

update_benefits <- function(d, classes) {
  
  # base pay received from employer
  # not dealing with weeks worked yet, need to impute that from CPS.
  # just assuming full time jobs for illustration purposes - 261 working days per year
  # also, only including in base leave pay leaves that participant would use program benefits for.
  
  d["base_leave_pay"] <- (as.matrix(d["WAGP"])/261*as.matrix(ifelse(is.na(d[,'particip_length']), 0,d[,'particip_length']))* as.matrix(d[,"prop_pay"]))
  
  # base benefits received from program
  d["base_benefits"] <- (as.matrix(d["WAGP"])/261*as.matrix(ifelse(is.na(d[,'particip_length']), 0,d[,'particip_length'])) * as.matrix(d[,"benefit_prop"]))

  # actual pay and benefits - to be modified by remaining parameter functions
  d <- d %>% mutate(actual_leave_pay=base_leave_pay)
  d <- d %>% mutate(actual_benefits=base_benefits)

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
  
  # define family income to match 2001 Westat survey categories
  d <- d %>% mutate(finc_cat=ifelse(faminc<=10000,10000,NA))
  inc_cut <- seq(20000, 90000, by=10000)
  for (i in inc_cut) {
    d <- d %>% mutate(finc_cat=ifelse(faminc>i & faminc<=i+10000,i,finc_cat))
  }
  d <- d %>% mutate(finc_cat=ifelse(faminc>100000,100000,finc_cat))
  d['finc_cat']=as.numeric(d[,'finc_cat'])
  
  # recalculate uptake based on bene_diff
  d <- join(d,d_prob, type="left",match="all")
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
  for (i in len_vars) {
    d['topoff_temp']= ifelse(d['topoff']==1 & d['topoff_min']<=d[i] & is.na(d[i])==FALSE,1,0)
    d <- d %>% mutate(topoff_count= ifelse(topoff_temp==1 ,topoff_count+1,topoff_count))
  }
  
  # overrides benefiteffect changes, since topoff behavior is seen
  d <- d %>% mutate(actual_benefits=ifelse(topoff_count>0,base_benefits,actual_benefits))
  d <- d %>% mutate(actual_leave_pay=ifelse(topoff_count>0,base_leave_pay-base_benefits,actual_leave_pay))
  
  d <- d %>% mutate(topoff_flg= ifelse(topoff_count>0,1,0))
  
  # adjust participation flag. participation leave taken assumed to not be affected by top off behavior
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