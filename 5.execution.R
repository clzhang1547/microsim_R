
# """
# Executing input building, policy scenario, and simulation commands for a sample user
#  
# 9 Sept 2018
# Luke
#
# """

# House keeping
#rm(list=ls())
cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("plyr")
library("dplyr")
library("survey")
library("class")
library("dummies")
library("varhandle")
library('MASS')
library('oglmx')
library('foreign')
library('ggplot2')
library('reshape2')

options(error=recover)
#options(error=NULL)

source("1.clean_FMLA.R")
source("2.clean_ACS.R")
source("2a.KNN1_match.R")
source("3.Policy_experiments.R")
source("4.NN_simulation.R")
source("4a.intra_fmla_NN_simulation.R")


# clean data
fmla_csv <- clean_fmla(csv=FALSE)
acs_csv <- clean_acs("ss15pma_short.csv","ss15hma_short.csv",csv=FALSE,filename='')

#----------------------------------------------------------------------------
# Prescence of leave program scenario - testing out all the parameters
#-----------------------------------------------------------------------------
# sample parameters below are California's 
start_time <- Sys.time()
d1 <- policy_simulation(filename="ACS_with_program",d_fmla=fmla_csv, d_acs=acs_csv,  leaveprogram=TRUE,bene_level=.55,
                        ext_base_effect=TRUE, extend_prob=.1, extend_days=1, extend_prop=1.1, topoff_rate=.1, topoff_minlength=10,
                        bene_effect=1, full_particip_needer=1, extend_leaves=1, wait_period=5, clone_factor=0, week_bene_cap=1216,
                        own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                        illspouse_uptake=.25, illchild_uptake=.25,
                        maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                        maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                        maxlen_illspouse =30, maxlen_illchild =30,earnings=300, minsize=50,random_seed=123)
end_time <- Sys.time()
print(end_time - start_time)

# # #------------------------------------
# # # Diagnostic tables
# # #------------------------------------
table(d1$eligworker)
table(d1$particip)
table(d1$particip_length)
table(d1$actual_benefits)
# 
# # #------------------------------------
# # # Abscence of leave program scenario
# # #------------------------------------
#  start_time <- Sys.time()
#  d2 <- policy_simulation(filename="ACS_no_program",d_fmla=fmla_csv, d_acs=acs_csv)
#  
#  end_time <- Sys.time()
#  print(end_time - start_time)
#  
#  
# # -------------------------
# # Paid Leve Program Impacts
# # -------------------------
#  # leave taking
#  for (i in leave_types) {
#    take_var=paste("take_",i,sep="")
#    print(paste(table(d2[,take_var],d1[,take_var])[3]/table(d2[,take_var],d1[,take_var])[4]*100,
#                "percent increase in",i, "leave taking"))
#  }
#  
#  # length of leave
#  for (i in leave_types) {
#    len_var=paste("length_",i,sep="")
#    print(paste(mean(d1[,len_var])/mean(d2[,len_var]),
#                "percent increase in",i, "leave length"))
#  }
# # -------------------------
# # Program Cost
# # -------------------------
# #total
# print(sum(d1$actual_benefits*d1$PWGTP*weight_factor))
# 
# #by leave type
# for (i in leave_types) {
#    bene_var=paste("bene_",i,sep="")
#    print(paste(i,"leave benefit costs"))
#    print(sum(d1[,bene_var]*d1$PWGTP*weight_factor))
# }

#------------------------------------
# Abscence of leave program scenario, full ACS, Mass
#------------------------------------
#source("4a.intra_fmla_NN_simulation.R")
# 
# d3 <- policy_simulation(filename="ACS_with_program",leaveprogram=TRUE,bene_level=.55,
#                                                          d_fmla=fmla_csv, d_acs=full_acs_csv)
# end_time <- Sys.time()
# print(end_time - start_time)

