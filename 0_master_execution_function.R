
# """
# File: 0_NEW_Master_execution_function
#
# Master execution function that calls all other functions in order to execute the simulation
# 
# 9 Sept 2018
# Luke
# 
# """
# ----------master function that sets policy scenario, and calls functions to alter FMLA data to match policy scenario---------------------
# default values are all false. In this case, simulation will result be prediction of leave taking in as-is scenario of "absence of program"
# Parameters:
# impute_method - method for imputation. Default is Nearest Neighbor, K=1
# xvars - xvars for imputation method to use. Default for Nearest Neighbor, K=1
# leaveprogram - FALSE for absence of any leave program, TRUE for leave program with default assumptions
#                assumptions are modified by below parameters
# base_bene_level - proportion of pay received as part of program participation
# bene_effect - 1 to affix behavioral cost to applying to program
#               0 to disable
# topoff_rate, min_length - percent of employer engagement in top-off substitution of paid leave with program benefits
# dependent_allow - weekly dependent allowance for those with children
# full_particip_needer - whether or not leave needers always take up benefits. default is yes (1)
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
# week_bene_cap_prop - option to cap max weekly benefits that can be collected at a proportion of the mean weekly wage
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
# sample_prop - sample proportion from ACS, and adjust weightfactor accordingly
# output - filename to save final data set as csv in output folder. If not specified, no csv is saved.
# output_stats - summary statistics output specification. if not specified, no CSV saved

# useCSV, saveDF are programmer convenience functions. should be removed from final product ( just a few adjustments in cleaning step below)
# useCSV - TRUE -> load raw CSv files for ACS/CPS/FMLA and run cleaning functions
#          FALSE -> load clean R files for ACS/CPS/FMLA and skip cleaning functions 
# SaveDF - TRUE -> Save cleaned files for ACS/CPS/FMLA as R data frames
#          FALSE -> Don't save cleaned files

policy_simulation <- function(fmla_csv, acs_house_csv, acs_person_csv, cps_csv, useCSV=TRUE, saveDF=FALSE,
                              leaveprogram=TRUE, 
                              FEDGOV=FALSE, 
                              STATEGOV=FALSE,
                              LOCALGOV=FALSE,
                              SELFEMP=FALSE, 
                              impute_method="KNN1",
                              xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
                                         "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
                                         "white", "asian", "hisp","nochildren"),
                              base_bene_level=1, topoff_rate=0, topoff_minlength=0, sample_prop=NULL, sample_num=NULL,
                              bene_effect=FALSE, dependent_allow=0, full_particip_needer=FALSE, 
                              own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, 
                              illparent_uptake=.25, illspouse_uptake=.25, illchild_uptake=.25, wait_period=0,
                              own_elig_adj=1, illspouse_elig_adj=1, illchild_elig_adj=1, 
                              illparent_elig_adj=1, matdis_elig_adj=1, bond_elig_adj=1,
                              clone_factor=0, ext_base_effect=TRUE, extend_prob=0, extend_days=0, extend_prop=1,
                              maxlen_own =60, maxlen_matdis =60, maxlen_bond =60, maxlen_illparent =60, maxlen_illspouse =60, maxlen_illchild =60,
                              maxlen_PFL=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond, maxlen_DI=maxlen_bond+maxlen_matdis,
                              maxlen_total=maxlen_DI+maxlen_PFL, week_bene_cap=1000000, week_bene_min=0, week_bene_cap_prop=NULL,
                              fmla_protect=TRUE, earnings=NULL, weeks= NULL, ann_hours=NULL, minsize= NULL, 
                              elig_rule_logic= '(earnings & weeks & ann_hours & minsize)',
                              formula_prop_cuts=NULL, formula_value_cuts=NULL, formula_bene_levels=NULL,
                              weightfactor=1, output=NULL, output_stats=NULL, random_seed=123) {
  
  ####################################
  # global libraries used everywhere #
  ####################################
  pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
    return("OK")
  }
  
  global.libraries <- c('extraDistr', 'stats', 'rlist', 'MASS', 'plyr', 'dplyr', 
                        'survey', 'class', 'dummies', 'varhandle', 'oglmx', 
                        'foreign', 'ggplot2', 'reshape2')
  
  results <- sapply(as.list(global.libraries), pkgTest)
  
  # clarify select function that's in both dplyr and MASS
  # might not be necessary since MASS is loaded first?
  select <- dplyr::select
  
  # run files that define functions
  source("1_cleaning_functions.R"); source("2_pre_impute_functions.R"); source("3_impute_functions.R"); 
  source("4_post_impute_functions.R"); source("5_output_analysis_functions.R")
  
  # set random seed option
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  #========================================
  # 1. Cleaning 
  #========================================
  
  if (useCSV==TRUE) {
    # Load and clean csv's for FMLA, ACS, and CPS surveys
    
    d_fmla <- read.csv(paste0("./csv_inputs/", fmla_csv))
    #INPUT: Raw file for FMLA survey
    d_fmla <- clean_fmla(d_fmla, save_csv=FALSE)
    #OUTPUT: clean FMLA dataframe 

    d_acs_person <- read.csv(paste0("./csv_inputs/",acs_person_csv))
    d_acs_house <-  read.csv(paste0("./csv_inputs/",acs_house_csv))
    #INPUT: Raw files for ACS person, household levels  
    d_acs <- clean_acs(d_acs_person, d_acs_house, save_csv=FALSE)
    #OUTPUT: clean ACS dataframe of employed individuals 18 and over

    d_cps <- read.csv(paste0("./csv_inputs/",cps_csv))
    #INPUT: Raw file for CPS 
    d_cps <- clean_cps(d_cps)
    #OUTPUT: Cleaned CPS dataframe
    
    
    #-----CPS to ACS Imputation-----
    # Impute hourly worker, weeks worked, and firm size variables from CPS into ACS. 
    # These are needed for leave program eligibilitly determination
    
    # INPUT: cleaned acs, cps files
    d_acs <- impute_cps_to_acs(d_acs, d_cps)
    # OUTPUT: cleaned acs with imputed weeks worked, employer size, and hourly worker status
  }
  
  else { 
    # load files from a dataframe
     d_fmla <- readRDS(paste0("./R_dataframes/","d_fmla.rds"))
     d_acs <- readRDS(paste0("./R_dataframes/","d_acs.rds"))
     d_cps <- readRDS(paste0("./R_dataframes/","d_cps.rds"))
  }
  
  if (saveDF==TRUE) {
    #save cleaned sets to dataframe
    saveRDS(d_fmla,file=paste0("./R_dataframes/","d_fmla.rds"))
    saveRDS(d_acs,file=paste0("./R_dataframes/","d_acs.rds"))
    saveRDS(d_cps,file=paste0("./R_dataframes/","d_cps.rds"))
    return()
  }
  
  # sample ACS
  # user option to sample ACS data
  if (!is.null(sample_prop)|!is.null(sample_num)) {
    d_acs <- sample_acs(d_acs, sample_prop=NULL, sample_num=NULL)  
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
  
  
  #-----FMLA to ACS Imputation-----
  # filter/modify ACS data based on user specifications
  # INPUT: ACS Data
  d_acs <- acs_filtering(d_acs, weightfactor, FEDGOV, STATEGOV, LOCALGOV, SELFEMP)
  # OUTPUT: Filtered ACS data
  
  # default is just simple nearest neighbor, K=1 
  # This is the big beast of getting leave behavior into the ACS.
  # INPUT: cleaned acs/fmla data, leaveprogram=TRUE/FALSE, method for imputation, dependent variables 
  #         used for imputation
  d_acs_imp <- impute_fmla_to_acs(d_fmla,d_fmla_orig,d_acs,leaveprogram, impute_method, xvars)  
  # OUTPUT: ACS data with imputed values for a) leave taking and needing, b) proportion of pay received from
  #         employer while on leave, and c) whether leave needed was not taken due to unaffordability 
    
  # -------------Post-imputation functions-----------------
  # ---------------------------------------------------------------------------------------------------------
  # Impute Days of Leave Taken
  # ---------------------------------------------------------------------------------------------------------
  # INPUTS: unmodified FMLA survey as the training data, ACS as test data,
  #         and conditionals for filter the two sets. Using unmodified survey for leave lengths,
  #         since modified FMLA survey contains no new information about leave lengths, and 
  #         the intra-FMLA imputed leave lengths a random draw from that imputed data
  #         would produced a biased 
  #         estimate of leave length
  d_acs_imp <- impute_leave_length(d_fmla_orig, d_acs_imp, leaveprogram, conditional, 
                                   test_conditional)
  # OUTPUT: ACS data with lengths for leaves imputed
  
  # function interactions description (may not be complete, just writing as they come to me):
  # ELIGIBILITYRULES: eligibility rules defined, and participation initially set 
  # UPTAKE: Uptake probability values applied
  # BENEFITS: base values for benefits to be modified by other functions
  # BENEFITEFFECT: Overrides UPTAKE participation, but there are some classes of participants not affected by this
  # TOPOFF: TOPOFF overrides participation behavior of BENEFITEFFECT
  # DEPENDENTALLOWANCE: independent of other functions
  
  # Allow for users to clone ACS individuals 
  # INPUT: ACS file
  d_acs_imp <- CLONEFACTOR(d_acs_imp, clone_factor)
  # OUTPUT: ACS file with user-specifed number of cloned records
     
  # Assign employer pay schedule for duration of leaves via imputation from Westat 2001 survey probabilities
  # Then, flag those who will have exhausted employer benefits with leave remaining, and will apply 
  # to leave program for remainder of their leave
  # INPUT: ACS file
  d_acs_imp <- PAY_SCHEDULE(d_acs_imp)
  # OUTPUT: ACS file with imputed pay schedule, and date of benefit exhaustion for those with partial pay
  
  # Program eligibility and uptake functions
  if (leaveprogram==TRUE) {
    # INPUT: ACS file
    d_acs_imp <-ELIGIBILITYRULES(d_acs_imp, earnings, weeks, ann_hours, minsize, base_bene_level, week_bene_min,
                                 formula_prop_cuts, formula_value_cuts, formula_bene_levels, elig_rule_logic) 
    # OUTPUT: ACS file with program eligibility and base program take-up indicators
    
    # Option to extend leaves under leave program 
      # INPUT: ACS file
      d_acs_imp <- EXTENDLEAVES(d_fmla, d_acs_imp, wait_period, ext_base_effect, 
                                extend_prob, extend_days, extend_prop, fmla_protect)  
      # OUTPUT: ACS file with leaves extended based on user specifications

    # INPUT: ACS file
    d_acs_imp <-UPTAKE(d_acs_imp, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                       illspouse_uptake, illchild_uptake, full_particip_needer, wait_period,
                       maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                       maxlen_total,maxlen_DI,maxlen_PFL)
    # OUTPUT: ACS file with modified leave program variables based on user-specified program restrictions
    #         on maximum participation length and user-specified take-up rate assumptions
  }
  
  if (leaveprogram==FALSE) {
    d_acs_imp["eligworker"] <- 0
    d_acs_imp["benefit_prop"] <- 0
    d_acs_imp["particip"] <- 0
    d_acs_imp["particip_length"] <- 0
    d_acs_imp["actual_benefits"] <- 0
    d_acs_imp["base_benefits"] <- 0
  }
  
  # benefit parameter functions
  # INPUT: ACS file
  d_acs_imp <- BENEFITS(d_acs_imp, leaveprogram)
  # OUTPUT: ACS file with base employer pay and program benefit calculation variables
  
  if (leaveprogram==TRUE & bene_effect==TRUE) {
    # INPUT: ACS file
    d_acs_imp <- BENEFITEFFECT(d_acs_imp)
    # OUTPUT: ACS file with leave taking variables modified to account for behavioral cost of applying to program
  }
  
  if (leaveprogram==TRUE & topoff_rate>0) {
    # INPUT: ACS file
    d_acs_imp <- TOPOFF(d_acs_imp,topoff_rate, topoff_minlength)
    # OUTPUT: ACS file with leave taking variables modified to account for employer top-off effects
  }
  
  if (leaveprogram==TRUE & dependent_allow>0) {
    # INPUT: ACS file
    d_acs_imp <- DEPENDENTALLOWANCE(d_acs_imp,dependent_allow)
    # OUTPUT: ACS file with program benefit amounts including a user-specified weekly dependent allowance
  }
  
  # Apply type-specific elig adjustments 
  if (leaveprogram==TRUE) {
    d_acs_imp <- DIFF_ELIG(d_acs_imp, own_elig_adj, illspouse_elig_adj, illchild_elig_adj,
                           illparent_elig_adj, matdis_elig_adj, bond_elig_adj)
  }
  
  # final clean up 
  # INPUT: ACS file
  d_acs_imp <- CLEANUP(d_acs_imp, week_bene_cap,week_bene_cap_prop,week_bene_min, maxlen_own, maxlen_matdis, maxlen_bond, 
                       maxlen_illparent, maxlen_illspouse, maxlen_illchild, maxlen_total,maxlen_DI,maxlen_PFL)
  # OUTPUT: ACS file with finalized leave taking, program uptake, and benefits received variables
  
  
  # Options to output final data and summary statistics
  
  if (!is.null(output)) {
    write.csv(d_acs_imp, file=paste0('./output/',output,'.csv'))
  }
  
  if (output_stats=='standard') {
    standard_summary_stats(d_acs_imp,output) 
  }
  
  if (output_stats=='state_compar') {
    state_compar_stats(d_acs_imp, output)
  }
  
  return(d_acs_imp)
}

