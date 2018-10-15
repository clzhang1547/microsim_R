# Source code for R version of Paid Leave Model 

This repository contains the code for the Microsim Paid Leave project

## Code
Purpose of each file of R code:

"0_NEW_master_execution_function.R" : Master policy simulation function calling all other functions based on user parameters

"1_NEW_cleaning_functions.R" : standardize/clean FMLA, ACS, and CPS survey variables

"2_NEW_impute_functions.R"  : Several imputation functions and subfunctions to clean and impute leave taking behavior and length within the FMLA survey itself, and apply a basic counterfactual leave taking effect in the presence of a leave program. FMLA leave taking behavior is then imputed into the ACS. To execute FMLA -> ACS imputation, the default method is a nearest neighbor function. TODO: build alternative imputation methods to swap nearest neighbor with.

"3_NEW_post_impute_functions.R" : define policy simulation functions

"TEST_execution.R" : sample execution of Master policy simulation function

## Inputs
ss16hri_short.csv: sample ACS household-level file

ss16pri_short.csv: sample ACS person-level file

fmla_2012_employee_restrict_puf.csv: FMLA 2012 raw file

CPS2014extract.csv: March CPS 2014 extract file
  TODO: Add cleaning code to allow this file to be replaced with other CPS files more easily


## Other files
KNN1_testing.R: code verifying KNN1_scratch function matches results of canned 'neighbr' package

d_acs.rds, d_cps.rds, d_fmla.rds: cleaned ACS, CPS, FMLA dataframes to quicken runtimes for programmer convenience while testing
