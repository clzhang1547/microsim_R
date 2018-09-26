
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


options(max.print=2000)

#options(error=NULL)


# baseline data
#full_acs_csv <- read.csv("Mass_ACS_clean.csv")
fmla_csv <- read.csv("fmla_clean_2012.csv")
acs_csv <- read.csv("ACS_clean.csv")


#----------------------------------------------------------------------------
# Prescence of leave program scenario - testing out all the parameters
#-----------------------------------------------------------------------------
start_time <- Sys.time()
d1 <- policy_simulation(filename="ACS_with_program", leaveprogram=TRUE,bene_level=.50,
                                      topoff_rate = .25, topoff_minlength = 5,
                                      d_fmla=fmla_csv, d_acs=acs_csv, bene_effect=1)
end_time <- Sys.time()
print(end_time - start_time)

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

