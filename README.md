# Source code for R version of Paid Leave Model 

This repository contains the code for the Microsim Paid Leave project

## Code
Purpose of each file of R code:

"1. clean_FMLA.R" : standardize/clean FMLA survey variables

"2. clean_ACS.R"  : standardize/clean ACS survey variables

"2a. KNN1_match.R" : define KNN1 function written from scratch

"3. Policy_experiements.R" : define policy simulation functions (should perhaps come after 4, 4a as it references functions in those two)

"4. NN_simulation.R" : define functions to execute FMLA -> ACS imputation based on nearest neighbor's value

"4a. intra_fmla_NN_simulation.R" : define functions to impute additional leave taking behavior within the FMLA survey. Without such imputation, we only observe leave taking behavior for the most recent leave taken. Imputation done in the same manner as original Albelda/Clayton-Matthews (ACM) model; via logit regressions for leave types, and by random draw for leave lengths.

"5. execution.R" : execute sample policy simulations 

"old code.R": misc old code not currently in use

## Inputs
ss15hma_short.csv: sample ACS household-level file

ss15pma_short.csv: sample ACS person-level file

fmla_2012_employee_restrict_puf.csv: FMLA 2012 raw file

bene_effect_prob.csv: Behavioral benefit uptake probabilities by payout level calculated via ACM with 2001 Westat survey on leave taking

## Intermediate files
ACS_clean.csv: cleaned ACS file from "2. clean_ACS.R"

fmla_clean_2012.csv: cleaned FMLA file from "1. clean_FMLA.R"

## outputs
"ACS_with_program.csv": ACS file with imputed leave, based on presence of program and sample parameters. Identical to "d1" in "5. execution.R"
"ACS_no_program.csv": ACS file with imputed leave, based on absence of program. Identical to "d2" in "5. execution.R"
