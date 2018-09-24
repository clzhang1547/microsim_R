
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
source("3.Policy_experiments.R")

options(max.print=2000)

#options(error=NULL)


# baseline data
#full_acs_csv <- read.csv("Cali_ACS_clean.csv")
fmla_csv <- read.csv("fmla_clean_2012.csv")
acs_csv <- read.csv("ACS_clean.csv")

# build NN K = 1 mapping between data sets


#----------------------------------------------------------------------------
# Prescence of leave program scenario - testing out all the parameters
#-----------------------------------------------------------------------------
d1 <- policy_simulation(filename="ACS_with_program",leaveprogram=TRUE,bene_level=.75,
                                      topoff_rate = .5, topoff_minlength = 5,
                                      d_fmla=fmla_csv, d_acs=acs_csv, bene_effect=1)

#------------------------------------???
# Abscence of leave program scenario
#------------------------------------
#ACS_no_program <- policy_simulation(filename="ACS_no_program",d_fmla=fmla_csv, d_acs=acs_csv)


#------------------------------------
# Abscence of leave program scenario, full ACS, cali
#------------------------------------
#source("4a.intra_fmla_NN_simulation.R")

# Mass_no_program <- policy_simulation(filename="ACS_with_program",leaveprogram=TRUE,bene_level=.55,
#                                                          d_fmla=fmla_csv, d_acs=full_acs_csv)
# 
# 

# analyzing differences between the two scenarios
# print("-------------------------")
# print("Paid Leve Program Impacts")
# print("-------------------------")
# for (i in leave_types) {
#   take_var=paste("take_",i,sep="")
#   print(paste(table(out_no[,take_var],out_prog[,take_var])[3]/table(out_no[,take_var],out_prog[,take_var])[4]*100,
#               "percent increase in",i, "leave"))
# }
# 
# 


