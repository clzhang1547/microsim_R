# Source code for R version of Paid Leave Model 

This repository contains the code for the Microsim Paid Leave project

## Code
Purpose of each file of R code:

"0_NEW_master_execution_function.R" : Master policy simulation function calling all other functions based on user parameters

"1_NEW_cleaning_functions.R" : standardize/clean FMLA, ACS, and CPS survey variables

"2_NEW_impute_functions.R"  : define:
    KNN1 function written from scratch
    functions to execute FMLA -> ACS imputation based on nearest neighbor's value
    functions to impute additional leave taking behavior within the FMLA survey. Without such imputation, we only observe leave taking behavior for the most recent leave taken. Imputation done in the same manner as original Albelda/Clayton-Matthews (ACM) model; via logit regressions for leave types, and by random draw for leave lengths.

"3_NEW_post_impute_functions.R" : define policy simulation functions

"TEST_execution.R" : sample execution of Master policy simulation function

## Inputs
ss15hri_short.csv: sample ACS household-level file

ss16pri_short.csv: sample ACS person-level file

fmla_2012_employee_restrict_puf.csv: FMLA 2012 raw file

CPS2014extract.csv: March CPS 2014 extract file
  TODO: Add cleaning code to allow this file to be replaced with other CPS files more easily


## Other files
KNN1_testing.R: code verifying KNN1_scratch function matches results of canned 'neighbr' package

d_acs.rds, d_cps.rds, d_fmla.rds: cleaned ACS, CPS, FMLA dataframes to quicken runtimes for programmer convenience while testing
