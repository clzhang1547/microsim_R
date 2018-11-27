#rm(list=ls())
cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")

#First time, preload cleaned data sets to speed up testing
# policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
#                    acs_person_csv="ss16pri_short.csv",
#                    acs_house_csv="ss16hri_short.csv",
#                    cps_csv="CPS2014extract.csv",
#                    useCSV=TRUE,
#                    saveDF=TRUE,
#                    GOVERNMENT=TRUE,
#                    SELFEMP=TRUE)


start_time <- Sys.time()
# subsequent times, can run from saved r data frames to save time.
d <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                  acs_person_csv="ss16pri_short.csv",
                  acs_house_csv="ss16hri_short.csv",
                  cps_csv="CPS2014extract.csv",
                  useCSV=FALSE,
                  saveDF=FALSE,
                  leaveprogram=TRUE,
                  base_bene_level=.55,
                  impute_method="Naive_Bayes",
                  #sample_prop=.95,
                  ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                  bene_effect=TRUE, full_particip_needer=FALSE, wait_period=5, clone_factor=0, week_bene_cap=1216,
                  own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                  illspouse_uptake=.25, illchild_uptake=.25,
                  maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                  maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                  maxlen_illspouse =30, maxlen_illchild =30,earnings=30000, own_elig_adj= .75,
                  formula_value_cuts=c(20000, 50000, 100000), formula_bene_levels=c(.4,.5,.6,.7),
                  output='states', output_stats='state_compar', random_seed=NULL)
end_time <- Sys.time()
print(end_time - start_time)
