# This program takes the cleaned FMLA data and runs a number of logit
# regressions to produce coefficient estimates (which are stored as csv files)
# to be used in the simulation model.

cat("\014")  
library("dplyr")
library("survey")

df <- read.csv("fmla_clean_2012.csv")


# ---------------------------------------------------------------------------------------------------------
# 0. Define Functions
# ---------------------------------------------------------------------------------------------------------

# Function to run logits
runLogit <- function(x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = df %>% filter_(y))
  svyglm(as.formula(x),data = df %>% filter_(y),family = "quasibinomial",design = des)
}

# Function to run Estimates
runEstimate <- function(x,y,z,lname){
  
  # Apply function to all leave types
  complete <- mapply(runLogit, x = x, y = y, z = z, SIMPLIFY = FALSE)
  
  # Extract Coefficient Estimates
	# switched to lapply, sapply was giving me errors for specif that were all the same functional form
  estimates <- lapply(complete, coef)
  
  # Save
  for (i in 1:length(estimates)){
    data_frame(var=names(estimates[[i]]),est=estimates[[i]]) %>%
      write.csv(paste0("./estimates/",lname,"_",names(estimates[i]),".csv"),row.names=F)
  }
}

# ---------------------------------------------------------------------------------------------------------
# 1. Probability of needing (including taking) a leave
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "type_own ~ age + lnfaminc + hourly + divorced + coveligd",
            illspouse = "type_illspouse ~ age + agesq + widowed",
            illchild = "type_illchild ~ nochildren + separated + divorced",
            illparent = "type_illparent ~ age + agesq + male + nevermarried + BAplus",
            matdis = "type_matdis ~ age + lnfaminc",
            bond = "type_bond ~ age + male + lnfaminc + nevermarried")

# subsetting data
conditional <- c(own = "TRUE",
                 illspouse = "nevermarried == 0 & divorced == 0",
                 illchild = "TRUE",
                 illparent = "TRUE",
                 matdis = "female == 1 & nochildren == 0",
                 bond = "nochildren == 0")

# weights
weight <- c(own = "~ fixed_weight",
                 illspouse = "~ fixed_weight",
                 illchild = "~ fixed_weight",
                 illparent = "~ weight",
                 matdis = "~ fixed_weight",
                 bond = "~ fixed_weight")

# Run Estimation
runEstimate(specif,conditional,weight,"needtake")

# ---------------------------------------------------------------------------------------------------------
# 2. Probability of seeing a doctor
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "doctor_take ~ age + male + ltHS",
            illspouse = "doctor_take ~ 1",
            illchild = "doctor_take ~ 1",
            illparent = "doctor_take ~ 1",
            matdis = "doctor_take ~ 1",
            bond = "doctor_take ~ 1")

# subsetting data
conditional <- c(own = "take_own == 1",
                 illspouse = "take_illspouse == 1",
                 illchild = "take_illchild == 1",
                 illparent = "take_illparent == 1",
                 matdis = "take_matdis == 1",
                 bond = "take_bond == 1")

# weights
weight <- c(own = "~ fixed_weight",
            illspouse = "~ weight",
            illchild = "~ weight",
            illparent = "~ weight",
            matdis = "~ weight",
            bond = "~ weight")

# Run Estimation
runEstimate(specif,conditional,weight,"seedoctor")

# ---------------------------------------------------------------------------------------------------------
# 3. Probability of Hospital Visit
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "hospital_take ~ age + hourly",
            illspouse = "hospital_take ~ 1",
            illchild = "hospital_take ~ 1",
            illparent = "hospital_take ~ 1",
            matdis = "hospital_take ~ 1",
            bond = "hospital_take ~ 1")

# subsetting data
conditional <- c(own = "take_own == 1 & doctor_take == 1",
                 illspouse = "take_illspouse == 1 & doctor_take == 1",
                 illchild = "take_illchild == 1 & doctor_take == 1",
                 illparent = "take_illparent == 1 & doctor_take == 1",
                 matdis = "take_matdis == 1 & doctor_take == 1",
                 bond = "take_bond == 1 & doctor_take == 1")

# weights
weight <- c(own = "~ weight",
            illspouse = "~ weight",
            illchild = "~ weight",
            illparent = "~ weight",
            matdis = "~ weight",
            bond = "~ weight")

# Run Estimation
runEstimate(specif,conditional,weight,"hospvisit")

# ---------------------------------------------------------------------------------------------------------
# 4. Probability of taking a leave conditional on needing one
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "take_own ~ age + male + lnfaminc + black + hisp + coveligd",
            illspouse = "take_illspouse ~ 1",
            illchild = "take_illchild ~ 1",
            illparent = "take_illparent ~ 1",
            matdis = "take_matdis ~ 1",
            bond = "take_bond ~ 1")

# subsetting data
conditional <- c(own = "type_own == 1",
                 illspouse = "type_illspouse == 1",
                 illchild = "type_illchild == 1",
                 illparent = "type_illparent == 1",
                 matdis = "type_matdis == 1",
                 bond = "type_bond == 1")

# weights
weight <- c(own = "~ weight",
            illspouse = "~ weight",
            illchild = "~ weight",
            illparent = "~ weight",
            matdis = "~ weight",
            bond = "~ weight")

# Run Estimation
runEstimate(specif,conditional,weight,"takeleave")

# ---------------------------------------------------------------------------------------------------------
# 5. Probability of receiving any pay
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "anypay ~ coveligd + hourly + age + agesq + lnfaminc",
            illspouse = "anypay ~ hourly",
            illchild = "anypay ~ hourly",
            illparent = "anypay ~ lnfaminc + hourly",
            matdis = "anypay ~ lnfaminc",
            bond = "anypay ~ lnfaminc + coveligd")

# subsetting data
conditional <- c(own = "take_own == 1",
                           illspouse = "take_illspouse == 1",
                           illchild = "take_illchild == 1",
                           illparent = "take_illparent == 1",
                           matdis = "take_matdis == 1",
                           bond = "take_bond == 1")

# weights
weight <- c(own = "~ 1",
            illspouse = "~ fixed_weight",
            illchild = "~ fixed_weight",
            illparent = "~ fixed_weight",
            matdis = "~ fixed_weight",
            bond = "~ fixed_weight")

# Run Estimation
runEstimate(specif,conditional,weight,"anypay")

# ---------------------------------------------------------------------------------------------------------
# 6. Probability of fully paid leave 
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "fullyPaid ~ hourly + age + lnfaminc + lnlength",
            illspouse = "fullyPaid ~ lnfaminc",
            illchild = "fullyPaid ~ 1",
            illparent = "fullyPaid ~ lnfaminc",
            matdis = "fullyPaid ~ hourly + lnlength",
            bond = "fullyPaid ~ lnlength")

# subsetting data
conditional <- c(own = "take_own == 1 & anypay == 1",
                 illspouse = "take_illspouse == 1 & anypay == 1",
                 illchild = "take_illchild == 1 & anypay == 1",
                 illparent = "take_illparent == 1 & anypay == 1",
                 matdis = "take_matdis == 1 & anypay == 1",
                 bond = "take_bond == 1 & anypay == 1")

# weights
weight <- c(own = "~ 1",
            illspouse = "~ fixed_weight",
            illchild = "~ weight",
            illparent = "~ fixed_weight",
            matdis = "~ fixed_weight",
            bond = "~ fixed_weight")

# Run Estimation
runEstimate(specif,conditional,weight,"fullypaid")

# ---------------------------------------------------------------------------------------------------------
# 7. Probability of extend leave if any/additional pay
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "longerLeave ~ age + agesq + female",
                      illspouse = "longerLeave ~ age + coveligd",
                      illchild = "longerLeave ~ 1",
                      illparent = "longerLeave ~ 1",
                      matdis = "longerLeave ~ 1",
                      bond = "longerLeave ~ 1")

# subsetting data
conditional <- c(own = "take_own == 1",
                           illspouse = "take_illspouse == 1",
                           illchild = "take_illchild == 1",
                           illparent = "take_illparent == 1",
                           matdis = "take_matdis == 1",
                           bond = "take_bond == 1")

# weights
weight <- c(own = "~ fixed_weight",
                        illspouse = "~ fixed_weight",
                      illchild = "~ fixed_weight",
                      illparent = "~ fixed_weight",
                      matdis = "~ fixed_weight",
                      bond = "~ fixed_weight")

# function to run logits
runLogit <- function(x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = df %>% filter_(y))
  svyglm(as.formula(x), data = df %>% filter_(y), family = "binomial", design = des)
}

# Run Estimation
runEstimate(specif,conditional,weight,"longerleave")

# ---------------------------------------------------------------------------------------------------------
# 8. Proportion of Pay Received (ordered logit)
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "factor(A50) ~ 1",
                     matdis = "factor(A50) ~ lnlength",
                     bond = "factor(A50) ~ 1")

# subsetting data
conditional <- c(own = "take_own == 1 & fullyPaid == 0",
                             matdis = "take_matdis == 1 & fullyPaid == 0",
                             bond = "take_bond == 1 & fullyPaid == 0")

# Weights
weight <- c(own = "~ fixed_weight",
                        matdis = "~ fixed_weight",
                        bond = "~ fixed_weight")

# function to run logits
runLogit <- function(x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = df %>% filter_(y))
  svyolr(as.formula(x), design = des)
}

# Run Estimation
runEstimate(specif,conditional,weight,"paygroup")


# adding missing regressions

# ---------------------------------------------------------------------------------------------------------
# 9. Would take leave if pay available
# ---------------------------------------------------------------------------------------------------------

# specifications
specif <- c(own = "unaffordable ~ lnfaminc",
                      illspouse = "unaffordable ~ lnfaminc",
                      illchild = "unaffordable ~ lnfaminc",
                      illparent = "unaffordable ~ lnfaminc",
                      matdis = "unaffordable ~ lnfaminc",
                      bond = "unaffordable ~ lnfaminc")

# subsetting data
conditional <- c(own = "need_own == 1",
                           illspouse = "need_illspouse == 1",
                           illchild = "need_illchild == 1",
                           illparent = "need_illparent == 1",
                           matdis = "need_matdis == 1",
                           bond = "need_bond == 1")

# weights
weight <- c(own = "~ fixed_weight",
                      illspouse = "~ fixed_weight",
                      illchild = "~ fixed_weight",
                      illparent = "~ fixed_weight",
                      matdis = "~ fixed_weight",
                      bond = "~ fixed_weight")

runLogit <- function(x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = df %>% filter_(y))
  svyglm(as.formula(x),data = df %>% filter_(y),family = "quasibinomial",design = des)
}


# Run Estimation
runEstimate(specif,conditional,weight,"unaffordable")



