# test out cost estimation of different imputation methods on RI data set

#rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")

#=================================
# KNN1
#=================================
timestart <<- Sys.time()
d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pri.csv",
                          acs_house_csv="ss16hri.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          saveCSV = TRUE,
                          makelog=TRUE,
                          base_bene_level=.6,
                          impute_method="KNN1",
                          ext_resp_len = TRUE, len_method = 'mean', sens_var = 'resp_len', progalt_post_or_pre ='post',
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=0, topoff_minlength=10,
                          bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                          dependent_allow = 10,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20,
                          maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output="RI_KNN1_no_topoff",
                          output_stats=c('state_compar'),  random_seed=NULL)
timeend <<- Sys.time()
print(timeend - timestart)

#=================================
# KNN1
#=================================
timestart <<- Sys.time()
d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pri.csv",
                          acs_house_csv="ss16hri.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          saveCSV = TRUE,
                          makelog=TRUE,
                          base_bene_level=.6,
                          impute_method="KNN1",
                          ext_resp_len = TRUE, len_method = 'mean', sens_var = 'resp_len', progalt_post_or_pre ='post',
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=1, topoff_minlength=10,
                          bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                          dependent_allow = 10,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20,
                          maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output="RI_KNN1_RL",
                          output_stats=c('state_compar'),  random_seed=NULL)
timeend <<- Sys.time()
print(timeend - timestart)


