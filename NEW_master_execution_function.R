
# """
# File: Master_execution_function
#
# Master execution function that calls all other functions in order to execute the simulation
# 
# 9 Sept 2018
# Luke
# 
# """

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


# ----------master function that sets policy scenario, and calls functions to alter FMLA data to match policy scenario---------------------
# default values are all false. In this case, simulation will result be prediction of leave taking in as-is scenario of "absence of program"
# Parameters:
# impute_method - method for imputation. Default is Nearest Neighbor, K=1
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
# maxlen_DI - setting to cap total combined disability insurance leaves (matdis and own leave). default is off
# maxlen_PFL - setting to cap total combined paid family leaves (all other types). default is off
# maxlen_total - setting to cap all leaves. default is off
# week_bene_cap - max weekly benefits that can be collected
# fmla_protect - Indicates whether or not leaves that are extended in the presence of a program that
#     originally were less than 12 weeks in length are constrained to be no longer than
#     12 weeks in the presence of the program
# program eligibility parameters - user specified requirements for eligibility into program. 
# These are optional, if set to null, no restriction is applied for that parameter
# earnings - earnings in past 12 months
# weeks - weeks worked in past 12 months
# ann_hours - total number of hours worked in past 12 months
# minsize - Number of employees working at their employer
# weightfactor - Multiply ACS weights by a certain number
# random_seed - set random seed if user wishes analyses to be replicable 

# useCSV, saveDF are programmer convenience functions. should be removed from final product ( just a few adjustments in cleaning step below)


policy_simulation <- function(fmla_csv, acs_house_csv, acs_person_csv, cps_csv, useCSV=TRUE, saveDF=FALSE,
                              leaveprogram=FALSE, GOVERNMENT=FALSE, SELFEMP=FALSE, 
                              impute_method="KNN1", bene_level=1, topoff_rate=0, topoff_minlength=0, 
                              bene_effect=0, dependent_allow=0, full_particip_needer=1, own_uptake=1, matdis_uptake=1, bond_uptake=1, illparent_uptake=1, 
                              illspouse_uptake=1, illchild_uptake=1, extend_leaves=0,wait_period=0,
                              clone_factor=0, ext_base_effect=TRUE, extend_prob=0, extend_days=0, extend_prop=1,
                              maxlen_own =60, maxlen_matdis =60, maxlen_bond =60, maxlen_illparent =60, maxlen_illspouse =60, maxlen_illchild =60,
                              maxlen_PFL=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond, maxlen_DI=maxlen_bond+maxlen_matdis,
                              maxlen_total=maxlen_DI+maxlen_PFL, week_bene_cap=1000000, fmla_protect=TRUE, earnings=NULL, weeks= NULL, annhours=NULL,
                              minsize= NULL, weightfactor=1, random_seed=NULL) {
  
  # load required libraries
  library('MASS')
  library("plyr")
  library("dplyr")
  library("survey")
  library("class")
  library("dummies")
  library("varhandle")
  library('oglmx')
  library('foreign')
  library('ggplot2')
  library('reshape2')
  
  # run files that define functions
  source("1_NEW_cleaning_functions.R")
  source("2_NEW_impute_functions.R")
  source("3_NEW_post_impute_functions.R")
  
  #========================================
  # 1. Cleaning 
  #========================================
  
  if (useCSV==TRUE) {
    # Load and clean csv's for FMLA, ACS, and CPS surveys
    
    # save_csv is a programmer's convenience argument, should be removed from final product
    
    d_fmla <- read.csv(fmla_csv)
    #INPUT: Raw file for FMLA survey
    d_fmla <- clean_fmla(d_fmla, save_csv=FALSE)
    #OUTPUT: clean FMLA dataframe 

    d_acs_person <- read.csv(acs_person_csv)
    d_acs_house <-  read.csv(acs_house_csv)
    #INPUT: Raw files for ACS person, household levels  
    d_acs <- clean_acs(d_acs_person, d_acs_house, save_csv=FALSE, weightfactor,GOVERNMENT,SELFEMP)
    #OUTPUT: clean ACS dataframe of employed individuals with user options to: 
      # a) multiple weights by a specified amount
      # b) include government workers
      # c) include self employed workers
    
    d_cps <- read.csv(cps_csv)
    #INPUT: Raw file for CPS 
    d_cps <- clean_cps(d_cps)
    #OUTPUT: Cleaned CPS dataframe
    
    
    #-----CPS to ACS Imputation-----
    # Impute hourly worker, weeks worked, and firm size variables from CPS into ACS. 
    # These are needed for leave program eligibilitly determination
    d_acs <- impute_cps_to_acs(d_acs, d_cps)
  }
  
  else { 
    # Programmer convenience: load files from a dataframe
    # should be removed from final product
     d_fmla <- readRDS("d_fmla.rds")
     d_acs <- readRDS("d_acs.rds")
     d_cps <- readRDS("d_cps.rds")
  }
  
  if (saveDF==TRUE) {
    # Programmer convenience: save cleaned sets to dataframe
    # should be removed from final product
    saveRDS(d_fmla,file="d_fmla.rds")
    saveRDS(d_acs,file="d_acs.rds")
    saveRDS(d_cps,file="d_cps.rds")
    return()
  }
  
  # set random seed option
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  #========================================
  # 2. Pre-imputation 
  #========================================
  
  # General philosophy behind split between pre-ACS imputation and post-ACS imputation modifications:
  # Do as much as possible post-imputation, and impute as few FMLA variables as possible.
  
  # Pre-imputation: Alterations that are specific to FMLA variables. Mostly leave taking behavior.
  # Post-imputation: Alterations specific to ACS variables, or require both FMLA and ACS variables. 
  #                  Uptake behavior and benefit calculations.
  
  
  # define global values for use across functions
  leave_types <<- c("own","illspouse","illchild","illparent","matdis","bond")
  
  
  # preserve original copy of FMLA survey
  d_fmla_orig <- d_fmla 
  
  # INPUT: FMLA data
  # intra-fmla imputation for additional leave taking and needings
  d_fmla <- impute_intra_fmla(d_fmla)
  # OUTPUT: FMLA data with modified take_ and need_ vars for those with additional leaves
  
  # INPUT: FMLA data
  # In presence of program, apply leave-taking behavioral updates
  if (leaveprogram==TRUE) {
    d_fmla <-LEAVEPROGRAM(d_fmla)
  }
  # OUTPUT: FMLA data with adjusted take_leave columns to include 1s
  #         for those that would have taken leave if they could afford it
  
  # after all parameters have been accounted for, add leave lengths for take leaves added
  # define test and train conditionals for leave length imputation
  test_conditional <- c(own = "(take_own==1 & is.na(length_own)==TRUE)", 
                        illspouse = "(take_illspouse==1 & is.na(length_illspouse)==TRUE)",
                        illchild = "(take_illchild==1 & is.na(length_illchild)==TRUE)",
                        illparent = "(take_illparent==1 & is.na(length_illparent)==TRUE)",
                        matdis = "(take_matdis==1 & is.na(length_matdis)==TRUE)",
                        bond = "(take_bond==1 & is.na(length_bond)==TRUE)")
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
  
  # INPUTS: unmodified FMLA survey as the training data, modified FMLA as test data,
  #         and conditionals for filter the two sets
  d_fmla <- impute_leave_length(d_fmla_orig, d_fmla, conditional, test_conditional, fmla=TRUE)
  # OUTPUT: modified FMLA data with lengths for leaves added as part of
  #         program participation or intra FMLA imputation

  #-----FMLA to ACS Imputation-----
  
  # dependent vars to match on
  xvars <- c("empid", "widowed", "divorced", "separated", "nevermarried", "female", 
             "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
             "white", "asian", "hisp","nochildren")
  
  # default is just simple nearest neighbor, K=1 
  # This is the big beast of getting leave behavior into the ACS.
    d_acs_imp <- impute_fmla_to_acs(d_fmla,d_acs,leaveprogram, impute_method, xvars)  

  # TODO: define expected input and outputs of alt matching method functions
  
  
  # -------------Post-imputation functions-----------------
  
  # function interactions description (may not be complete, just writing as they come to me):
  # ELIGIBILITYRULES: eligibility rules defined, and participation initially set 
  # UPTAKE: Uptake probability values applied
  # BENEFITS: base values for benefits to be modified by other functions
  # BENEFITEFFECT: Overrides UPTAKE participation, but there are some classes of participants not affected by this
  # TOPOFF: TOPOFF overrides participation behavior of BENEFITEFFECT
  # DEPENDENTALLOWANCE: independent of other functions
  
  # OK I'm stopping here. Will review below after first revision!
  # Allow for users to clone ACS individuals 
  d_acs_imp <- CLONEFACTOR(d_acs_imp, clone_factor)
  
  # creating benefit vars in ACS based on imputations/parameters
  d_acs_imp <- PROGRAM_VARS(d_acs_imp, classes=c("own", "illspouse", "illchild","illparent","matdis","bond"), leaveprogram)
  
  # After other benefits/participation decisions made, flag those who 
  # will have exhausted employer benefits with leave remaining, and will apply to program for remainder
  d_acs_imp <- PAY_SCHEDULE(d_acs_imp)
  
  # Program eligibility and uptake functions
  if (leaveprogram==TRUE) {
    d_acs_imp <-ELIGIBILITYRULES(d_acs_imp, earnings, weeks, annhours, minsize, bene_level) 
    
    # Option to extend leaves under leave program 
    if (extend_leaves==1) {
      d_acs_imp <- EXTENDLEAVES(d_fmla, d_acs_imp, wait_period, ext_base_effect, 
                                extend_prob, extend_days, extend_prop, fmla_protect)  
    }
    
    d_acs_imp <-UPTAKE(d_acs_imp, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                       illspouse_uptake, illchild_uptake, full_particip_needer, wait_period,
                       maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                       maxlen_total,maxlen_DI,maxlen_PFL)
  }
  
  if (leaveprogram==FALSE) {
    d_acs_imp["benefit_prop"] <- 0
    d_acs_imp["particip"] <- 0
    d_acs_imp["particip_length"] <- 0
    d_acs_imp["actual_benefits"] <- 0
    d_acs_imp["base_benefits"] <- 0
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
  d_acs_imp <- CLEANUP(d_acs_imp, week_bene_cap,maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                       maxlen_total,maxlen_DI,maxlen_PFL)
  
  return(d_acs_imp)
}

