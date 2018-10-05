# This program cleans CPS data and runs a number of logit
# regressions to produce coefficient estimates (which are stored as csv files)
# to be used in the simulation model.

CPS_impute <- function(d_acs, d_cps){
  
  # ---------------------------------------------------------------------------------------------------------
  # 1. Clean CPS
  # ---------------------------------------------------------------------------------------------------------
  #Create dummies for logit regressions
    # Gender
  d_cps <- d_cps %>% mutate(male = ifelse(a_sex == 1,1,0),
                      female = ifelse(a_sex == 2,1,0))
    
    # Education
  d_cps <- d_cps %>% mutate(ltHS = ifelse(a_hga <= 38,1,0),
                        someCol = ifelse(a_hga >= 40 & a_hga<=42,1,0),
                        BA = ifelse(a_hga == 43,1,0),
                        GradSch = ifelse(a_hga >= 44,1,0))
    # Race
  d_cps <- d_cps %>% mutate(black = ifelse(prdtrace==2 & pehspnon==2,1,0),
                      asian = ifelse(prdtrace==4 & pehspnon==2,1,0),
                      other = ifelse(((prdtrace==3)|((prdtrace>=5)&(prdtrace<=26)))&(pehspnon==2),1,0),
                      hisp = ifelse(pehspnon==1,1,0))
  
    # age squared
  d_cps <- d_cps %>% mutate(age = a_age)
  d_cps <- d_cps %>% mutate(agesq = a_age*a_age)
  
    # occupation
  d_cps <- d_cps %>% mutate(occ_1 = ifelse(a_mjocc == 1,1,0),
                      occ_2 = ifelse(a_mjocc == 2,1,0),
                      occ_3 = ifelse(a_mjocc == 3,1,0),
                      occ_4 = ifelse(a_mjocc == 4,1,0),
                      occ_5 = ifelse(a_mjocc == 5,1,0),
                      occ_6 = ifelse(a_mjocc == 6,1,0),
                      occ_7 = ifelse(a_mjocc == 7,1,0),
                      occ_8 = ifelse(a_mjocc == 8,1,0),
                      occ_9 = ifelse(a_mjocc == 9,1,0),
                      occ_10 = ifelse(a_mjocc == 10,1,0))
  d_cps <- d_cps %>% mutate(occ_1 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_1),
                      occ_2 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_2),
                      occ_3 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_3),
                      occ_4 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_4),
                      occ_5 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_5),
                      occ_6 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_6),
                      occ_7 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_7),
                      occ_8 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_8),
                      occ_9 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_9),
                      occ_10 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_10))
  
    # industry
  d_cps <- d_cps %>% mutate(ind_1 = ifelse(a_mjind == 1,1,0),
                      ind_2 = ifelse(a_mjind == 2,1,0),
                      ind_3 = ifelse(a_mjind == 3,1,0),
                      ind_4 = ifelse(a_mjind == 4,1,0),
                      ind_5 = ifelse(a_mjind == 5,1,0),
                      ind_6 = ifelse(a_mjind == 6,1,0),
                      ind_7 = ifelse(a_mjind == 7,1,0),
                      ind_8 = ifelse(a_mjind == 8,1,0),
                      ind_9 = ifelse(a_mjind == 9,1,0),
                      ind_10 = ifelse(a_mjind == 10,1,0),
                      ind_11 = ifelse(a_mjind == 11,1,0),
                      ind_12 = ifelse(a_mjind == 12,1,0),
                      ind_13 = ifelse(a_mjind == 13,1,0))
  d_cps <- d_cps %>% mutate(ind_1 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_1),
                      ind_2 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_2),
                      ind_3 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_3),
                      ind_4 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_4),
                      ind_5 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_5),
                      ind_6 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_6),
                      ind_7 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_7),
                      ind_8 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_8),
                      ind_9 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_9),
                      ind_10 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_10),
                      ind_11 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_11),
                      ind_12 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_12),
                      ind_13 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_13))
  
    # hourly pay
  d_cps <- d_cps %>% mutate(paid_hrly= ifelse(prerelg == 1, 0,NA))
  d_cps <- d_cps %>% mutate(paid_hrly= ifelse(prerelg == 1 & a_hrlywk == 1, 1, paid_hrly))
  
  # Making zero/negative earnings into NaN so we can take natural log
  d_cps <- d_cps %>% mutate(lnearn = ifelse(pearnval<=0,NA,pearnval))  
  d_cps <- d_cps %>% mutate(lnearn = log(lnearn))  
  # Making other values 0 per ACM code
  d_cps <- d_cps %>% mutate(lnearn = ifelse(pearnval<=0, 0, lnearn))  
  
  # employer provided health insurance
  d_cps$hiemp <- as.numeric(d_cps$hiemp)
  d_cps <- d_cps %>% mutate(hiemp = ifelse(hiemp == 0, NA, hiemp))  
  d_cps <- d_cps %>% mutate(hiemp = ifelse(hiemp == 2, 0, hiemp))  
  
  # weeks worked 
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=50 & wkswork<=52, 1,NA))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=48 & wkswork<=49, 2,wks_cat))
  d_cps <- d_cps %>% mutate(wks_48_49= ifelse(wkswork==49, 1,0))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=40 & wkswork<=47, 3,wks_cat))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=27 & wkswork<=39, 4,wks_cat))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=14 & wkswork<=26, 5,wks_cat))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=0 & wkswork<=13, 6,wks_cat))
  
  # presence of children
  d_cps <- d_cps %>% mutate(fem_cu6= ifelse(pextra1==2,1,0))
  d_cps <- d_cps %>% mutate(fem_c617= ifelse(pextra1==3,1,0))
  d_cps <- d_cps %>% mutate(fem_cu6and617= ifelse(pextra1==4,1,0))
  d_cps <- d_cps %>% mutate(fem_nochild= ifelse(pextra1==5,1,0))
  
  # employer size
  d_cps <- d_cps %>% mutate(emp_size=noemp) 

  #write.csv(d_cps, file = "CPS_extract_clean.csv", row.names = FALSE)
  
  # ---------------------------------------------------------------------------------------------------------
  # 2. Define Functions
  # ---------------------------------------------------------------------------------------------------------

  
  # Function to run logits
  runLogit <- function(x,y,z){
    des <- svydesign(id = ~1,  weights = as.formula(z), data = d_cps %>% filter_(y))
    svyglm(as.formula(x),data = d_cps %>% filter_(y),family = "quasibinomial",design = des)
  }

  # MASS implementation
  runOrdinal <- function(x,y,z){
    estimate <- polr(as.formula(x), data = d_cps %>% filter_(y))
    return(estimate)
  }
# 
#   # OGLMX implementation - gives pretty non sensical results from my efforts
#   runOrdinal <- function(x,y,z){
#      results.ologit <- oglmx(as.formula(x), data = d_cps %>% filter_(y), weights=marsupwt)
#      pause()
#      return(estimate)
#   }
  
  runEstimate <- function(x,y,z, mname){
    
    # Apply function to all leave types
    if (mname == "logit") {
      complete <- mapply(runLogit, x = x, y = y, z = z, SIMPLIFY = FALSE)
      
      # Extract Coefficient Estimates
      # switched to lapply, sapply was giving me errors for specif that were all the same functional form
      estimates <- lapply(complete, coef)
    }
    if (mname == "ordinal") {
      estimates <- mapply(runOrdinal, x = x, y = y, z = z, SIMPLIFY = FALSE)
    }
    return(estimates)
  }
  
  # Function to apply estimates to training data set, logit
  runLogitImpute <- function(d_in, estimates, varname, tcond) {
    d <- d_in %>% filter_(tcond)
    model=estimates[[1]]
    d['var_score']=estimates[[1]]['(Intercept)']
    for (dem in names(model)) {
      if (dem !='(Intercept)') { 
        d[is.na(d[,dem]),dem]=0
        d[,'var_score']= d[,'var_score'] + d[,dem]*model[dem]
      }
    }
    d <- d %>% mutate(cumprob= exp(var_score)/(1+exp(var_score)))
    d['rand']=runif(nrow(d))
    d[varname] <- with(d, ifelse(rand>cumprob,0,1))
    
    # keep just the new variable and id
    d <- d[c(varname, "empid")]
    d <- merge( d, d_in, by="empid",all.y=TRUE)
    
    return(d)
  }
  
  # Function to apply estimates to training data set, ordinal
  runOrdinalImpute <- function(d_in, estimates, varname, tcond) {
    d <- d_in %>% filter_(tcond)
    
  
    # calculate score from ordinal model
    model=estimates$coefficients
    d['var_score']=0
    for (dem in names(model)) {
      if (dem !='(Intercept)') { 
        d[is.na(d[,dem]),dem]=0
        d[,'var_score']= d[,'var_score'] + d[,dem]*model[dem]
      }
    }
    
    # assign categorical variable based on ordinal cuts
    cuts= estimates$zeta
    cat_num= length(cuts)+1
    d[varname] <- 0
    d['rand']=runif(nrow(d))
    for (i in seq(cat_num)) {
      if (i!=cat_num) {
        d <- d %>% mutate(cumprob= var_score-cuts[i])
        d <- d %>% mutate(cumprob2= exp(cumprob)/(1+exp(cumprob)))
        d[varname] <- with (d, ifelse(get(varname)==0 & rand>=cumprob2,i,get(varname)))
      }
      if (i==cat_num) {
        d[varname] <- with (d, ifelse(get(varname)==0,i,get(varname)))
      }
    }
    # keep just the new variable and id
    d <- d[c(varname, "empid")]
    d <- merge( d, d_in, by="empid",all.y=TRUE)
    
    return(d)
  }  
  
  # ---------------------------------------------------------------------------------------------------------
  # 3. Run logit models
  # ---------------------------------------------------------------------------------------------------------
  
  # logit for hourly paid regression

  specif = c(paid_hrly =  paste("paid_hrly ~ female + black + a_age + agesq + BA",
    "+ GradSch + occ_1 + occ_3 + occ_5 + occ_7 + occ_8",
     "+ occ_9 + occ_10 + ind_5 + ind_8 + ind_11 + ind_12"))
  conditional = c(paid_hrly= "TRUE")
  weight = c(paid_hrly = "~ marsupwt")

  estimates <- runEstimate(specif,conditional,weight, "logit")
  d_acs <-runLogitImpute(d_acs, estimates,"paid_hrly","TRUE") 
  

  # ordered logit for number of employers
  # biggest problem with ordered logit currently is it is unweighted; can't use CPS weight without getting a non-convergence error
  specif = c(num_employers= paste("factor(phmemprs) ~  age + agesq + asian + hisp",
                                  "+ ltHS + someCol + BA + GradSch + lnearn",
                                  "+ hiemp + ind_4 + ind_5 + ind_6 + ind_8",
                                  "+ ind_13 + occ_1 + occ_6 + occ_7 + occ_9 + occ_10"))
  conditional = c(num_employers= "TRUE")
  weight = c(num_employers = "marsupwt")


  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$num_employers
  d_acs <- runOrdinalImpute(d_acs, estimates,"num_emp","TRUE")

  # ordered logit for weeks worked - 50-52 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + agesq +  black + hisp + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==1")
  weight = c(iweeks_worked = "marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$iweeks_worked
  d_acs <- runOrdinalImpute(d_acs, estimates,"wks_50_52","WKW==1")

  # logit for weeks worked - 48-49 weeks
  specif = c(iweeks_worked= paste("wks_48_49 ~ age + agesq + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==2")
  weight = c(iweeks_worked = "~ marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "logit")
  estimates <- estimates$iweeks_worked
  d_acs <- runLogitImpute(d_acs, estimates,"wks_48_49","WKW==2")
  
  # ordered logit for weeks worked - 40-47 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==3")
  weight = c(iweeks_worked = "marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$iweeks_worked
  d_acs <- runOrdinalImpute(d_acs, estimates,"wks_40_47","WKW==3")
  
  # ordered logit for weeks worked - 27-39 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + fem_cu6 + fem_c617 + fem_cu6and617 + female + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==4")
  weight = c(iweeks_worked = "marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$iweeks_worked
  d_acs <- runOrdinalImpute(d_acs, estimates,"wks_27_39","WKW==4")
  
  # ordered logit for weeks worked - 14-26 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + hisp + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==5")
  weight = c(iweeks_worked = "marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$iweeks_worked
  d_acs <- runOrdinalImpute(d_acs, estimates,"wks_14_26","WKW==5")
  
  # ordered logit for weeks worked - <13 weeks
  specif = c(iweeks_worked= paste("factor(wkswork) ~  age + agesq + female + lnearn"))
  conditional = c(iweeks_worked= "wks_cat==6")
  weight = c(iweeks_worked = "marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$iweeks_worked
  d_acs <- runOrdinalImpute(d_acs, estimates,"wks_0_13","WKW==6")
  
  # create single weeks worked var
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_50_52),wks_50_52+49,0))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_48_49),wks_48_49+48,iweeks_worked)) # "+48" is intentaional, this is a 0/1 var
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_40_47),wks_40_47+39,iweeks_worked))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_27_39),wks_27_39+26,iweeks_worked))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_14_26),wks_14_26+13,iweeks_worked))
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_0_13),wks_0_13,iweeks_worked))
  
  # Ordered logit employer size categories
  specif = c(emp_size =  paste("factor(emp_size) ~ a_age + black + ltHS + someCol + BA + GradSch + lnearn",
                                "  + hiemp + ind_1 + ind_3 + ind_5 + ind_6 + ind_8 + ind_9",
                                "+ ind_11 + ind_12 + ind_13 + occ_1 + occ_4 + occ_5 + occ_6 + occ_7 + occ_9"))
  conditional = c(emp_size= "TRUE")
  weight = c(emp_size = "marsupwt")
  estimates <- runEstimate(specif,conditional,weight, "ordinal")
  estimates <- estimates$emp_size
  d_acs <- runOrdinalImpute(d_acs, estimates,"emp_size","TRUE")
  
  
  # then do random draw within assigned size range
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==1,sample(1:9, nrow(d_acs), replace=T),0))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==2,sample(10:49, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==3,sample(50:99, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==4,sample(100:499, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==5,sample(500:999, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==6,sample(1000:9999, nrow(d_acs), replace=T),temp_size))
  d_acs <- d_acs %>% mutate(emp_size=temp_size)
  d_acs <- d_acs %>% mutate(weeks_worked_cat=weeks_worked)
  d_acs <- d_acs %>% mutate(weeks_worked=iweeks_worked)
  
  
  # clean up vars
  d_acs <- d_acs[, !(names(d_acs) %in% c('rand','temp_size','iweeks_worked',
                                         "wks_0_13", "wks_14_26", "wks_27_39", "wks_40_47", 
                                         "wks_48_49", "wks_50_52", "num_emp" ))]
  
  return(d_acs)
}