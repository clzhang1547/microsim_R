# test executions on full state data

#rm(list=ls())
cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_NEW_master_execution_function.R")

#=================================
#Rhode Island
#=================================
start_time <- Sys.time()
d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                  acs_person_csv="ss16pri.csv",
                  acs_house_csv="ss16hri.csv",
                  cps_csv="CPS2014extract.csv",
                  useCSV=TRUE,
                  saveDF=FALSE,
                  leaveprogram=TRUE,
                  bene_level=.6,
                  ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                  bene_effect=1, full_particip_needer=1, extend_leaves=1, wait_period=5, clone_factor=0, week_bene_cap_prop=.85,
                  own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                  illspouse_uptake=.25, illchild_uptake=.25, dependent_allow = 10,
                  maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                  maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520, random_seed=123)


# diagnostic tables
table(d_ri$eligworker)
table(d_ri$particip)
table(d_ri$particip_length)
table(d_ri$actual_benefits)
table(d_ri$exhausted_by, useNA = 'always')
table(d_ri$bene_effect_flg, useNA = 'always')

# Program Cost
#total
print(format(sum(d_ri$actual_benefits*d_ri$PWGTP),digits=0,big.mark=",",scientific=FALSE))

#by leave type
for (i in leave_types) {
  bene_var=paste("bene_",i,sep="")
  print(paste(i,"leave benefit costs"))
  print(format(sum(d_ri[,bene_var]*d_ri$PWGTP),digits=0,big.mark=",",scientific=FALSE))
}


end_time <- Sys.time()
print(end_time - start_time)

#=================================
#California
#=================================
start_time <- Sys.time()
d_ca <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pca.csv",
                          acs_house_csv="ss16hca.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          leaveprogram=TRUE,
                          bene_level=.55,
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                          bene_effect=1, full_particip_needer=1, extend_leaves=1, wait_period=5, clone_factor=0, week_bene_cap=1216,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                          maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                          maxlen_illspouse =30, maxlen_illchild =30,earnings=300, random_seed=125)


# diagnostic tables
table(d_ca$eligworker)
table(d_ca$particip)
table(d_ca$particip_length)
table(d_ca$actual_benefits)
table(d_ca$exhausted_by, useNA = 'always')
table(d_ca$bene_effect_flg, useNA = 'always')

# Program Cost
#total
print(format(sum(d_ca$actual_benefits*d_ca$PWGTP),digits=0,big.mark=",",scientific=FALSE))

#by leave type
for (i in leave_types) {
  bene_var=paste("bene_",i,sep="")
  print(paste(i,"leave benefit costs"))
  print(format(sum(d_ca[,bene_var]*d_ca$PWGTP),digits=0,big.mark=",",scientific=FALSE))
}
end_time <- Sys.time()
print(end_time - start_time)

#=================================
# New Jersey
#=================================
start_time <- Sys.time()
d_nj <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pnj.csv",
                          acs_house_csv="ss16hnj.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          leaveprogram=TRUE,
                          bene_level=.66,
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                          bene_effect=1, full_particip_needer=1, extend_leaves=1, wait_period=5, clone_factor=0, week_bene_cap=637,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent =30, 
                          maxlen_PFL= 30, maxlen_DI=130, maxlen_total=130,
                          maxlen_illspouse =30, maxlen_illchild =30,earnings=8400, random_seed=124)


# diagnostic tables
table(d_nj$eligworker)
table(d_nj$particip)
table(d_nj$particip_length)
table(d_nj$actual_benefits)
table(d_nj$exhausted_by, useNA = 'always')
table(d_nj$bene_effect_flg, useNA = 'always')

# Program Cost
#total
print(format(sum(d_nj$actual_benefits*d_nj$PWGTP),digits=0,big.mark=",",scientific=FALSE))

#by leave type
for (i in leave_types) {
  bene_var=paste("bene_",i,sep="")
  print(paste(i,"leave benefit costs"))
  print(format(sum(d_nj[,bene_var]*d_nj$PWGTP),digits=0,big.mark=",",scientific=FALSE))
}
end_time <- Sys.time()
print(end_time - start_time)

