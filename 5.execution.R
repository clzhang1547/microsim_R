
# """
# Executing input building, policy scenario, and simulation commands for a sample user
#  
# 9 Sept 2018
# Luke
#
# """

rm(list=ls())
cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("dplyr")
library("survey")

#source("1.clean_FMLA.R")
#source("2.clean_ACS.R")
source("2a.KNN1_match.R")
source("3.Policy_experiments.R")
source("4.NN_simulation.R")
source("4a.intra_fmla_NN_simulation.R")


#options(max.print=2000)

#options(error=NULL)
#options(error=recover)

# baseline data
#full_acs_csv <- read.csv("Mass_ACS_clean.csv")
fmla_csv <- read.csv("fmla_clean_2012.csv") 
acs_csv <- read.csv("ACS_clean.csv")


#----------------------------------------------------------------------------
# Prescence of leave program scenario - testing out all the parameters
#-----------------------------------------------------------------------------
start_time <- Sys.time()

d1 <- policy_simulation(filename="ACS_with_program", leaveprogram=TRUE,bene_level=.55,
                                      topoff_rate = .2, topoff_minlength = 5,
                                      d_fmla=fmla_csv, d_acs=acs_csv, bene_effect=1,
                                      full_particip_needer=1, extend_leaves=1, wait_period=5, clone_factor=0,
                                      extend_prob=.3, extend_days=5, extend_prop=1.1, week_bene_cap=1216,
                                      own_uptake=.8, matdis_uptake=.9, bond_uptake=.85, illparent_uptake=.6,
                                      illspouse_uptake=.6, illchild_uptake=.7,
                                      maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                                      maxlen_illspouse =30, maxlen_illchild =30)
end_time <- Sys.time()
print(end_time - start_time)

# diagnostic tables
table(d1$particip)
table(d1$particip_length)
table(d1$total_length)
table(d1$actual_benefits)
table(d1$actual_leave_pay)
# #------------------------------------
# # Abscence of leave program scenario
# #------------------------------------
# start_time <- Sys.time()
# d2 <- policy_simulation(filename="ACS_no_program",d_fmla=fmla_csv, d_acs=acs_csv)
# 
# end_time <- Sys.time()
# print(end_time - start_time)
# 
# 
# # print("-------------------------")
# # print("Paid Leve Program Impacts")
# # print("-------------------------")
# for (i in leave_types) {
#   take_var=paste("take_",i,sep="")
#   print(paste(table(d2[,take_var],d1[,take_var])[3]/table(d2[,take_var],d1[,take_var])[4]*100,
#               "percent increase in",i, "leave"))
# }
# 




#------------------------------------
# Abscence of leave program scenario, full ACS, Mass
#------------------------------------
#source("4a.intra_fmla_NN_simulation.R")
# 
# d3 <- policy_simulation(filename="ACS_with_program",leaveprogram=TRUE,bene_level=.55,
#                                                          d_fmla=fmla_csv, d_acs=full_acs_csv)
# end_time <- Sys.time()
# print(end_time - start_time)

