
# """
# Building modifications for policy simulations prior to ACS NN imputation 
#  
# 9 Sept 2018
# Luke
#
# """

cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("class")
library("dplyr")
library("survey")

options(error=recover)
#options(error=NULL)

#d_acs <- read.csv("ACS_clean.csv")
#d_fmla <- read.csv("fmla_clean_2012.csv")

#------------------------------------
# 0. Functions 
#------------------------------------

# ----------master function that sets policy scenario, and calls functions to alter FMLA data to match policy scenario---------------------
# default values are all false. In this case, simulation will result be prediction of leave taking in as-is scenario of "absence of program"
# Parameters:
# leaveprogram -> FALSE for absence of any leave program, TRUE for leave program with default assumptions
# uptake -> uptake calculation approach
# benefits -> schedule of benefits to apply for program participation

policy_scenario <- function(leaveprogram=FALSE, uptake="full", benefits="full",d) {
  if (leaveprogram==TRUE) {
    d <-LEAVEPROGRAM(d)
    d <-UPTAKE(d, uptake)
    d <-BENEFITS(d, benefits)
  }
  return(d)
}

#-----------global values across functions------------
leave_types=c("own","illspouse","illchild","illparent","matdis","bond")

# -----------LEAVEPROGRAM-----------------------------------------------------------------------------------------------------------------
# Baseline changes for addition of a leave program
# follows baseline changes of ACM model (see p.11 of ACM model description paper). Main change needed to base cleaning:
#   Leave needers who did not take a leave in the absence of a program, and
#   who said the reason that they did not take a leave was because they could not afford to
#   take one, take a leave in the presence of a program.
LEAVEPROGRAM <- function(d) {
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    need_var=paste("need_",i,sep="")
    d[,take_var] <- ifelse(d[,"A62a"]==1 & d[,need_var]==1,1,d[,take_var])
  }
  return(d)
}

#---------UPTAKE------------------------------------------------------------------------------------------------------------------------
# specifies uptake rate of those that are eligible for the paid leave program
# default is "full" - all who are eligible participate 

UPTAKE <- function(d,uptake) {
  if(uptake=="full") {
    d[,"particip"] <- ifelse(d[,"eligworker"]==1,1,d[,"particip"])
  }
  return(d)
}

# to do - programming other kinds of uptake


#---------BENEFITS------------------------------------------------------------------------------------------------------------------------
# Specifies the benefit schedule.
# default is full pay
BENEFITS <- function(d,benefits) {
  if(benefits=="full") {
    # raise benefits and cost of all participating individuals to full pay
    d[,"benefit_prop"] <- ifelse(d[,"particip"]==1,1,d[,"benefit_prop"])
    d[,"cost_prop"] <- ifelse(d[,"particip"]==1,1,d[,"cost_prop"])
  }
  return(d)
}


