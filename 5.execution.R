
# """
# Executing input building, policy scenario, and simulation commands
#  
# 9 Sept 2018
# Luke
#
# """

rm(list=ls())
cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("class")
library("dplyr")
library("survey")

source("3.NN_simulation.R")
source("4.Policy_experiments.R")

# baseline data
d_fmla <- read.csv("fmla_clean_2012.csv")
d_acs <- read.csv("ACS_clean.csv")

#------------------------------------
# Abscence of leave program scenario
#------------------------------------
# building input data sets
no_prog <- policy_scenario(d=d_fmla)

# running simulation 
out_no <- simulate(filename="ACS_no_program",d_leave=no_prog,d_test=d_acs)


#------------------------------------
# Prescence of leave program scenario
#------------------------------------
# building input data sets
with_prog <- policy_scenario(leaveprogram=TRUE,d=d_fmla)

# running simulation
out_prog <-simulate(filename="ACS_with_program",d_leave=with_prog,d_test=d_acs)


# analyzing differences between the two scenarios
print("-------------------------")
print("Paid Leve Program Impacts")
print("-------------------------")
for (i in leave_types) {
  take_var=paste("take_",i,sep="")
  print(paste(table(out_no[,take_var],out_prog[,take_var])[3]/table(out_no[,take_var],out_prog[,take_var])[4]*100,
              "percent increase in",i, "leave"))
}




