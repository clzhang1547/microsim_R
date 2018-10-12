
# """
# 3_post_imputation_functions
#
# These functions are Post-imputation Leave Parameter Functions [on post-imputation ACS data set]
# that execute the policy simulation after leave taking behavior has been established.
#
# 
# """

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. CLONEFACTOR
# 2. PROGRAM_VARS
# 3. ELIGIBILITYRULES
    #3a. runLogitEstimate
# 4. UPTAKE
# 5. BENEFITS
# 6. BENEFITEFFECT
# 7. TOPOFF
# 8. DEPENDENTALLOWANCE
# 9. CLEANUP



#-------------------------CLONEFACTOR------------------
# allow users to clone ACS individuals

CLONEFACTOR <- function(d, clone_factor) {
  if (clone_factor > 0) {
    d$clone_flag=0
    num_clone <- round(clone_factor*nrow(d), digits=0)
    d_clones <- data.frame(sample(d$empid,num_clone,replace=TRUE))
    colnames(d_clones)[1] <- "empid"
    d_clones <- join(d_clones,d,by='empid', type='left')
    d_clones$clone_flag=1
    d <- rbind(d,d_clones)
  }
  return(d)
}


# -------------------PROGRAM_VARS------------------
# Adding base values for new ACS variables involving imputed FMLA values

PROGRAM_VARS <- function(d, classes, leaveprogram) {
  
  
  # replace leave taking and length NA's with zeros now
  # wanted to distinguish between NAs and zeros in FMLA survey, 
  # but no need for that in ACS now that we're "certain" of ACS leave taking behavior 
  # We are "certain" because we only imputed leave takers/non-takers, discarding those with 
  # uncertain/ineligible status (take_[type]=NA).
  
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    take_var=paste("take_",i,sep="")
    d[len_var] <- with(d, ifelse(is.na(get(len_var)),0,get(len_var)))
    d[take_var] <- with(d, ifelse(is.na(get(take_var)),0,get(take_var)))
  }
  
  return(d)
} 

# -------------------PAY_SCHEDULE------------------
# Calculate pay schedule for employer paid leave

PAY_SCHEDULE <- function(d) {
  
  # two possible pay schedules: paid the same amount each week, or paid in full until exhausted
  # Here we randomly assign one of these three pay schedules 
  # based on conditional probabilities of total pay received and pay schedules 
  # probabilities are obtained from 2001 Westat survey which ACM used for this purpose
  # dist <- read.csv("pay_dist_prob.csv")
  
  # columns from this csv written manually to avoid dependency on csv file
  # proportion of pay received (prop_pay in FMLA data)
  # Westat 2001 survey: About how much of your usual pay did you receive in total?
  Total_paid=c("Less than half","Half","More than half")
  
  # Prob of 1st pay schedule - some pay, all weeks
  # Westat 2001 survey: Receive receive some  pay for each pay period  that you were on leave?
  Always_paid=c(0.6329781, 0.8209731, 0.9358463)
  
  # Prob of 2nd pay schedule - full pay, all weeks
  # Westat 2001 survey: If not, when you did receive pay, was it for your full salary?
  Fully_paid=c(0.3273122,0.3963387,0.3633615)
  
  # Prob of 3rd pay schedule - some pay, some weeks
  # Neither paid each pay period, nor receive full pay when they did receive pay.
  Neither_paid=1-Fully_paid
  
  d_prob=data.frame(Total_paid,Always_paid,Fully_paid,Neither_paid)
  
  # denote bucket of proportion of pay
  d <- d %>% mutate(Total_paid= ifelse(prop_pay>0 & prop_pay<.5,"Less than half",NA))
  d <- d %>% mutate(Total_paid= ifelse(prop_pay==.5, "Half" ,Total_paid))
  d <- d %>% mutate(Total_paid= ifelse(prop_pay>.5 & prop_pay<1, "More than half",Total_paid))
  
  # merge probabilities in
  d <- join(d,d_prob, type="left",match="all",by=c("Total_paid"))
  
  # assign pay schedules
  d['rand']=runif(nrow(d))
  d['rand2']=runif(nrow(d))
  
  d <- d %>% mutate(pay_schedule= ifelse(rand<Always_paid,"some pay, all weeks",NA))
  d <- d %>% mutate(pay_schedule= ifelse(rand>=Always_paid & rand2<Fully_paid,"all pay, some weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(rand>=Always_paid & rand2>=Fully_paid,"some pay, some weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(prop_pay==1,"all pay, all weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(prop_pay==0,"no pay",pay_schedule))
  
  # total_length - number of days leave taken of all types
  d['total_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d <- d %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  # count up number of types of leaves
  d['total_leaves']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d <- d %>% mutate(total_leaves = ifelse(get(paste(take_var))==1, total_leaves+1,total_leaves))
  }
  
  # Keep track of what day employer benefits will be exhausted for those receiving pay in some but not all of their leave
  # all pay, some weeks
  d <-  d %>% mutate(exhausted_by=ifelse(pay_schedule=="all pay, some weeks",round(total_length*prop_pay, digits=0), NA))
  
  # some pay, some weeks - like ACM, assumes equal distribution of partiality among pay proportion and weeks taken
  d <-  d %>% mutate(exhausted_by=ifelse(pay_schedule=="some pay, some weeks",round(total_length*sqrt(prop_pay), digits=0), exhausted_by))
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand','rand2','Always_paid','Total_paid','Fully_paid', 'Neither_paid'))]
  
  return(d)
}

#---------ELIGIBILITYRULES-------------------------------------------------------------------------------------------------------------
# apply user-specified eligibility criteria and set initial 

ELIGIBILITYRULES <- function(d, earnings=NULL, weeks=NULL, annhours=NULL, minsize=NULL, bene_level) {
  
  d['eligworker']= 1
  
  # prior earnings in past 12 months
  if (!is.null(earnings)) {
    d <- d %>% mutate(eligworker= ifelse(WAGP>=earnings & eligworker==1, 1,0))
  }
  
  # prior weeks worked in past 12 months
  if (!is.null(weeks)) {
    d <- d %>% mutate(eligworker= ifelse(weeks_worked>=weeks & eligworker==1, 1,0))
  }
  
  # total hours worked in past 12 months
  if (!is.null(annhours)) {
    d <- d %>% mutate(eligworker= ifelse(weeks_worked*WKHP>=annhours & eligworker==1, 1,0))
  }
  
  # firm size of employer
  if (!is.null(minsize)) {              
    d <- d %>% mutate(eligworker= ifelse(emp_size>=minsize & eligworker==1, 1,0))
  }
  
  # calculate initial participation
  # calculate general participation decision based on employer pay vs state program pay
  
  d["benefit_prop"] <- bene_level
  d["particip"] <- 0
  
  # those who will receive more under the program will participate
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,"prop_pay"]<d[,"benefit_prop"],1,0)  
  
  # those who exhaust employer benefits before leave ends will participate
  d["particip"] <- ifelse(d[,"eligworker"]==1 & !is.na(d[,'exhausted_by']),1,d[,"particip"])  
  
  return(d)  
}


# -------------------EXTENDLEAVES------------------
# Option to simulate extension of leaves in the presence of an FMLA program

EXTENDLEAVES <-function(d_train, d_test,wait_period, ext_base_effect, 
                        extend_prob, extend_days, extend_prop, fmla_protect) {
  
  # copy original leave lengths
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    orig_var=paste("orig_len_",i,sep="")
    d_test[orig_var] <- with(d_test, get(len_var))
  }
  
  # Base extension effect from ACM model (referred to as the "old" extension simulation there)
  if (ext_base_effect==TRUE) { 
    
    # specifications
    # using ACM specifications
    specif <- c("longerLeave ~ age + agesq + female")
    
    # subsetting data
    conditional <- c("TRUE")
    
    test_conditional <- c("TRUE")
    
    # weights
    weight <- c("~ fixed_weight")
    
    # Run Estimation
    estimates <- mapply(runLogitEstimate, x = specif, y = conditional, z = weight, 
                        MoreArgs=list(d_in=d_train), 
                        SIMPLIFY = FALSE)
  
    # calculate a column of probabilities of each leave type based on individual characteristics
    var_prob= "longerLeave_prob"
    model=estimates
    d_test[var_prob]=model[[1]][["(Intercept)"]]
    for (dem in names(model[[1]])) {
      if (dem !='(Intercept)') { 
        d_test[is.na(d_test[,dem]),dem]=0
        d_test[,var_prob]= d_test[,var_prob] + d_test[,dem]*model[[1]][[dem]]
      }
      d_test[,var_prob]=exp(d_test[,var_prob])/(1+exp(d_test[,var_prob]))
    }
    
    # randomly determine if longer leave is taken
    
    d_test['rand']=runif(nrow(d_test))
    d_test <- d_test %>% mutate(longer_leave=ifelse(longerLeave_prob>rand,1,0))
    
    
    # Following ACM implementation:
    # i. For workers who have leave lengths in the absence of a program that are
    # less than the waiting period for the program: the leave is extended for 1 week into the program.
    
    d_test["extend_flag"]=0
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      d_test["extend_flag"] <- with(d_test, ifelse(get(len_var)<wait_period & particip==1 &
                                                     longer_leave == 1 & get(take_var)==1
                                                   ,1,extend_flag))
      d_test[len_var] <- with(d_test, ifelse(get(len_var)<wait_period & particip== 1 &
                                               longer_leave == 1 & get(take_var)==1
                                             ,get(len_var)+wait_period+5,get(len_var)))
      d_test["total_length"] <-  with(d_test, ifelse(get(len_var)<wait_period & particip== 1 &
                                                       longer_leave == 1 & get(take_var)==1
                                                     ,total_length+wait_period+5, total_length))
    }
    
    # ii. For workers who do not receive any employer pay or who exhaust their
    # employer pay and then go on the program: The probability of extending a leave using
    # program benefits is set to 25 percent; and for those who do extend their leave, the
    # extension is equal to 25 percent of their length in the absences of a program.
    d_test['rand']=runif(nrow(d_test))
    d_test <- d_test %>% mutate(longer_leave=ifelse(.25>rand,1,0))
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      d_test["extend_flag"] <- with(d_test, ifelse((prop_pay==0 | !is.na(exhausted_by)) & particip==1 &
                                                     longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                                   ,1,extend_flag))
      d_test[len_var] <- with(d_test, ifelse((prop_pay==0 | !is.na(exhausted_by)) & particip==1 &
                                               longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                             ,get(len_var)*1.25,get(len_var)))
      d_test["total_length"] <-  with(d_test, ifelse((prop_pay==0 | !is.na(exhausted_by)) & particip==1 &
                                                       longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                                     ,total_length+get(len_var)*.25, total_length))
    }
    
    # iii. For workers who exhaust program benefits and then receive employer pay:
    #   In this case the simulator assigns a 50 percent probability of taking an extended leave
    # until their employer pay is exhausted.
    
    # Not implemented, don't really get why this would be allowed or with what probability if it was
    
    # clean up vars
    d_test <- d_test[, !(names(d_test) %in% c("longerLeave_prob"))]
  } 
  
  # Additional option to extend leave a+bx additional days with c probability if the user wishes. 
  # a = extend_days
  # b = extend_prop
  # c = extend_prob
  # simplified from the ACM model; there they allowed it to be customized by leave type, just allowing for overall adjustments for now.
  
  
  if (extend_prob > 0) {
    d_test['rand']=runif(nrow(d_test))
    d_test["extend_flag"] <- with(d_test, ifelse(rand<extend_prob & particip==1 & total_length!=0,1,extend_flag))
    
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      d_test[len_var] <- with(d_test, ifelse(rand<extend_prob & particip==1 & get(paste(len_var))!=0,
                                             round(get(paste(len_var))*extend_prop),get(paste(len_var))))
      d_test[len_var] <- with(d_test, ifelse(rand<extend_prob & particip==1 & get(paste(len_var))!=0,
                                             round(get(paste(len_var))+(extend_days/total_leaves)),get(paste(len_var))))
    }
    
    # clean up vars
    d_test <- d_test[, !(names(d_test) %in% c("rand","extend_amt"))]
  }
  
  # FMLA Protection Constraint option
  # If enabled, leaves that are extended in the presence of a program that
  # originally were less than 12 weeks in length are constrained to be no longer than
  # 12 weeks in the presence of the program.
  
  if (fmla_protect==TRUE) {
    d_test["fmla_constrain_flag"] <- 0
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      orig_var=paste("orig_len_",i,sep="")
      d_test["fmla_constrain_flag"] <- with(d_test, ifelse(extend_flag==1 & get(len_var)>60 & get(orig_var)<=60
                                                           ,1,fmla_constrain_flag))
      d_test[len_var] <- with(d_test, ifelse(extend_flag==1 & get(len_var)>60 & get(orig_var)<=60
                                             ,60,get(len_var)))
    }
  }
  
  # adjust total_length to match extensions of individual leaves
  d_test['total_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d_test <- d_test %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  return(d_test)
}

# ============================ #
# 3A. runLogitEstimate
# ============================ #
# See function 1A, file 2 - impute functions


#---------UPTAKE------------------------------------------------------------------------------------------------------------------------
# specifies uptake rate of those that are eligible for the paid leave program
# default is "full" - all who are eligible and would receive more money than employer would pay
# would pay choose to participate 


UPTAKE <- function(d, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                   illspouse_uptake, illchild_uptake, full_particip_needer, wait_period, 
                   maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                   maxlen_total, maxlen_DI, maxlen_PFL) {
  
  # calculate uptake -> days of leave that program benefits are collected
  d['particip_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    uptake_val=paste(i,"_uptake",sep="")
    plen_var= paste("plen_",i, sep="")
    d['rand']=runif(nrow(d))
    d <- d %>% mutate(particip_length=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                               rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1, 
                                             particip_length+get(paste('length_',i,sep=""))-wait_period, particip_length))
    d[plen_var] <- with(d, ifelse(wait_period<get(paste('length_',i,sep="")) &
                                    rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1, 
                                  get(paste('length_',i,sep=""))-wait_period, 0))
    d <- d %>% mutate(change_flag=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                           rand<get(uptake_val) & particip==1 & get(paste(take_var)) == 1,1,0))
    
    # Option for if leave needers always take up benefits when they receive more than their employer pays in leave
    if (full_particip_needer==1) {
      d <- d %>% mutate(particip_length=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                                 rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1, 
                                               particip_length+get(paste('length_',i,sep=""))-wait_period, particip_length))
      d[plen_var] <- with(d, ifelse(wait_period<get(paste('length_',i,sep="")) &
                                      rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1, 
                                    get(paste('length_',i,sep=""))-wait_period, get(plen_var)))
      d <- d %>% mutate(change_flag=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                             rand>=get(uptake_val) & particip==1 & get(paste(take_var))== 1 & unaffordable==1,1, change_flag))  
    }
    
    # subtract days spent on employer benefits from those that exhausting employer benefits (received pay for some days of leave)
    # Also accounting for wait period here, as that can tick down as a person is still collecting employer benefits
    d <- d %>% mutate(particip_length= ifelse(change_flag==1 & !is.na(exhausted_by),
                                              ifelse(get(paste('length_',i,sep="")) > exhausted_by & exhausted_by>wait_period, 
                                                     particip_length - exhausted_by + wait_period, particip_length), particip_length))
    d[plen_var] <- with(d, ifelse(change_flag==1 & !is.na(exhausted_by),
                                  ifelse(get(paste('length_',i,sep="")) > exhausted_by & exhausted_by>wait_period, 
                                         get(plen_var) - exhausted_by + wait_period, get(plen_var)), get(plen_var)))
  }
  
  # make sure those with particip_length 0 are also particip 0
  d <- d %>% mutate(particip= ifelse(particip_length==0,0, particip))
  
  # cap particip_length at max program days
  # for each individual leave type
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    max_val=paste("maxlen_",i,sep="")
    d[plen_var] <- with(d, ifelse(get(plen_var)>get(max_val),get(max_val), get(plen_var)))
  }
  
  # apply cap for DI and PFL classes of leaves
  if (maxlen_DI!=maxlen_bond+maxlen_matdis) {
    d <- d %>% mutate(DI_plen=plen_matdis+plen_own)
    d['DI_plen'] <- with(d, ifelse(DI_plen>maxlen_DI,maxlen_DI,DI_plen))
    # evenly distributed cap among leave types
    d['reduce'] <- with(d, ifelse(plen_matdis+plen_own!=0, DI_plen/(plen_matdis+plen_own),0))
    d['plen_matdis']=round(d[,'plen_matdis']*d[,'reduce'])
    d['plen_own']=round(d[,'plen_own']*d[,'reduce'])
  }
  
  if (maxlen_PFL!=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond) {  
    d <- d %>% mutate(PFL_plen=plen_bond+plen_illparent+plen_illchild+plen_illspouse)
    d['PFL_plen'] <- with(d, ifelse(PFL_plen>maxlen_PFL,maxlen_PFL,PFL_plen))
    # evenly distributed cap among leave types
    d['reduce'] <- with(d, ifelse(plen_bond+plen_illparent+plen_illchild+plen_illspouse!=0, 
                                  PFL_plen/(plen_bond+plen_illparent+plen_illchild+plen_illspouse),0))
    d['plen_bond']=round(d[,'plen_bond']*d[,'reduce'])
    d['plen_illchild']=round(d[,'plen_illchild']*d[,'reduce'])
    d['plen_illspouse']=round(d[,'plen_illspouse']*d[,'reduce'])
    d['plen_illparent']=round(d[,'plen_illparent']*d[,'reduce'])
  }
  
  # apply cap for all leaves
  if (maxlen_total!=maxlen_DI+maxlen_PFL | maxlen_total!=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond+maxlen_bond+maxlen_matdis) {
    d['particip_length']=0
    for (i in leave_types) {
      plen_var=paste("plen_",i,sep="")
      d <- d %>% mutate(particip_length=particip_length+get(plen_var))
    }
    d['particip_length'] <- with(d, ifelse(particip_length>maxlen_total,maxlen_total,particip_length))
    d['reduce'] <- with(d, ifelse(plen_matdis+plen_own+plen_bond+plen_illparent+plen_illchild+plen_illspouse!=0, 
                                  particip_length/(plen_matdis+plen_own+plen_bond+plen_illparent+plen_illchild+plen_illspouse),0))
    
    # evenly distributed cap among leave types
    d['plen_matdis']=round(d[,'plen_matdis']*d[,'reduce'])
    d['plen_own']=round(d[,'plen_own']*d[,'reduce'])
    d['plen_bond']=round(d[,'plen_bond']*d[,'reduce'])
    d['plen_illchild']=round(d[,'plen_illchild']*d[,'reduce'])
    d['plen_illspouse']=round(d[,'plen_illspouse']*d[,'reduce'])
    d['plen_illparent']=round(d[,'plen_illparent']*d[,'reduce'])
    
    # recalculate DI/PFL lengths
    d <- d %>% mutate(DI_plen=plen_matdis+plen_own)
    d <- d %>% mutate(PFL_plen=plen_bond+plen_illparent+plen_illchild+plen_illspouse)
  }
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand', 'change_flag','reduce'))]
  return(d)
}


# -------------------BENEFITS------------------
# Adding base values for new ACS variables involving imputed FMLA values

BENEFITS <- function(d, leaveprogram) {
  
  if (leaveprogram==TRUE) {
    # base benefits received from program
    d <- d %>% mutate(base_benefits=WAGP/(round(weeks_worked*5))*particip_length*benefit_prop)
    d <- d %>% mutate(base_benefits=ifelse(is.na(base_benefits),0,base_benefits))
  }
  # base pay received from employer based on schedule
  # pay received is same across all pay schedules
  d <- d %>% mutate(base_leave_pay=WAGP/(round(weeks_worked*5))*total_length* prop_pay)
  d <- d %>% mutate(base_leave_pay=ifelse(is.na(base_leave_pay),0,base_leave_pay))
  
  # actual pay and benefits - to be modified by remaining parameter functions
  d <- d %>% mutate(actual_leave_pay=base_leave_pay)
  if (leaveprogram==TRUE) {
    d <- d %>% mutate(actual_benefits=base_benefits)
  }
  return(d)
}
# -------------------BENEFITEFFECT------------------
# Accounting for some "cost" of applying for the program when deciding between employer paid leave and program


BENEFITEFFECT <- function(d) {
  # Create uptake probabilities dataframe 
  # obtained from 2001 Westat survey which ACM used for this purpose
  # d_prob <- read.csv("bene_effect_prob.csv")
  
  # Hardcoding above CSV to remove dependency
  
  # Three columns of data set
  #Family income category
  finc_cat=rep(seq.int(10000,100000, by = 10000),4)
  
  #Benefit difference  
  bene_diff=c(rep(0,10),rep(25,10),rep(50,10),rep(125,10))
  
  #Probability of taking up benefits
  uptake_prob=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.12, 0.08, 0.05, 
                0.04, 0.02, 0.02, 0.01, 0.01, 0, 0, 0.59, 0.48, 
                0.38, 0.28, 0.21, 0.15, 0.1, 0.07, 0.05, 0.03, 
                1, 1, 1, 1, 1, 1, 0.99, 0.99, 0.98, 0.98)

  # create data frame
  d_prob=data.frame(finc_cat,bene_diff,uptake_prob)
  
  
  # define benefit difference to match 2001 Westat survey categories
  d <- d %>% mutate(bene_diff=(actual_benefits-actual_leave_pay)/particip_length*5)
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=25, 0, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=50 & bene_diff>25, 25, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=125 & bene_diff>50, 50, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff>125, 125, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(is.na(bene_diff), 0, bene_diff))
  d['bene_diff']=as.integer(d[,'bene_diff'])
  
  # PLACEHOLDER dealing with missing faminc values for the test ACS data set.
  # need to come up with systematic way of addressing missing values in ACS eventually
  d <- d %>% mutate(faminc=ifelse(is.na(faminc),WAGP,faminc))
  
  # define family income to match 2001 Westat survey categories
  d <- d %>% mutate(finc_cat=ifelse(faminc<=10000,10000,NA))
  inc_cut <- seq(10000, 90000, by=10000)
  for (i in inc_cut) {
    d <- d %>% mutate(finc_cat=ifelse(faminc>i & faminc<=i+10000,i,finc_cat))
  }
  d <- d %>% mutate(finc_cat=ifelse(faminc>100000,100000,finc_cat))
  
  d['finc_cat']=as.numeric(d[,'finc_cat'])
  
  # recalculate uptake based on bene_diff
  d <- join(d,d_prob, type="left",match="all",by=c("bene_diff", "finc_cat"))
  d['rand']=runif(nrow(d))
  
  # exclude those participants that will not be affected by benefit effect
  # those who exhaust employer benefits before leave ends will always participate
  d["universe"] <- ifelse(d[,"eligworker"]==1 & !is.na(d[,'exhausted_by']),0,1)  
  
  # those who choose to extend leaves in the presence of the program will always participate
  d["universe"] <- ifelse(d[,"eligworker"]==1 & d[,'extend_flag']==1,0,d[,'universe']) 
  
  # flag those who are not claiming benefits due to benefit effect
  d <- d %>% mutate(bene_effect_flg=ifelse(rand>uptake_prob & particip==1 & universe==1,1,0))
  
  # update leave vars
  d <- d %>% mutate(actual_benefits=ifelse(rand>uptake_prob & particip==1 & universe==1,0,actual_benefits))
  d <- d %>% mutate(particip_length=ifelse(rand>uptake_prob & particip==1 & universe==1,0,particip_length))
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    d[plen_var] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,get(plen_var)))
  }
  d['DI_plen'] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,DI_plen))
  d['PFL_plen'] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,PFL_plen))
  d <- d %>% mutate(particip=ifelse(rand>uptake_prob & particip==1 & universe==1,0,particip))
  
  d <- d[, !(names(d) %in% c('rand','bene_diff','finc_cat','uptake_prob','universe'))]
  
  return(d)
}

# -------------------TOPOFF------------------
# employers who would pay their employees
# 100 percent of wages while on leave would instead require their employees to participate
# in the program and would "top-off" the program benefits by paying the difference
# between program benefits and full pay. 
# User can specify percent of employers that engage in this, and minimum length of leave this is required for

TOPOFF <- function(d, topoff_rate, topoff_minlength) {
  len_vars <- c("length_own", "length_illspouse", "length_illchild","length_illparent","length_matdis","length_bond")
  d['topoff_rate'] <- topoff_rate
  d['topoff_min'] <- topoff_minlength
  d['rand'] <- runif(nrow(d))
  d <- d %>% mutate(topoff= ifelse(rand<topoff_rate & prop_pay==1,1,0))
  d <- d %>% mutate(topoff_count=0)
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    plen_var=paste("plen_",i,sep="")
    take_var=paste("take_",i,sep="")
    d['topoff_temp'] <- with(d,ifelse(topoff==1 & topoff_min<=get(paste(len_var)) & get(paste(take_var))==1,1,0))
    d[plen_var] <- with(d,ifelse(topoff_temp==1,get(len_var),get(plen_var)))
    d <- d %>% mutate(topoff_count= ifelse(topoff_temp==1 ,topoff_count+1,topoff_count))
  }
  d['particip_length']=0
  for (i in leave_types) {
    plen_var=paste("plen_",i,sep="")
    d <- d %>% mutate(particip_length=particip_length+get(plen_var))
  }
  
  # recalculate benefits based on updated participation length
  # actual benefits received from program
  # note: topoff will override benefiteffect changes
  d <- d %>% mutate(actual_benefits=WAGP/(round(weeks_worked*5))*particip_length*benefit_prop)
  d <- d %>% mutate(actual_benefits=ifelse(is.na(actual_benefits),0,actual_benefits))
  
  #subtract benefits from pay
  d <- d %>% mutate(actual_leave_pay=ifelse(topoff_count>0,base_leave_pay-actual_benefits,actual_leave_pay))
  
  d <- d %>% mutate(topoff_flg= ifelse(topoff_count>0,1,0))
  
  # adjust participation flag. leave taken assumed to not be affected by top off behavior
  d <- d %>% mutate(particip=ifelse(topoff_count>0,1,particip))
  d <- d[, !(names(d) %in% c('rand','topoff_rate','topoff_temp','topoff_min','topoff', 'topoff_count'))]
  
  return(d)
}

# -------------------------DEPENDENTALLOWANCE------------------
# include a flat weekly dependent allowance for families with children

DEPENDENTALLOWANCE <- function(d,dependent_allow) {
  d <- d %>% mutate(actual_benefits=ifelse(particip==1 & nochildren==0, actual_benefits+(dependent_allow)*particip_length/5,actual_benefits))
  return(d)
}

# -------------------------CLEANUP------------------
# Final variable alterations and consistency checks

CLEANUP <- function(d, week_bene_cap,maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                    maxlen_total,maxlen_DI,maxlen_PFL) {
  
  # Check caps again 
  
  # cap particip_length at max program days
  # for each individual leave type
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    max_val=paste("maxlen_",i,sep="")
    d[plen_var] <- with(d, ifelse(get(plen_var)>get(max_val),get(max_val), get(plen_var)))
  }
  
  # apply cap for DI and PFL classes of leaves
  if (maxlen_DI!=maxlen_bond+maxlen_matdis) {
    d <- d %>% mutate(DI_plen=plen_matdis+plen_own)
    d['DI_plen'] <- with(d, ifelse(DI_plen>maxlen_DI,maxlen_DI,DI_plen))
    # evenly distributed cap among leave types
    d['reduce'] <- with(d, ifelse(plen_matdis+plen_own!=0, DI_plen/(plen_matdis+plen_own),0))
    d['plen_matdis']=round(d[,'plen_matdis']*d[,'reduce'])
    d['plen_own']=round(d[,'plen_own']*d[,'reduce'])
  }
  
  if (maxlen_PFL!=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond) {  
    d <- d %>% mutate(PFL_plen=plen_bond+plen_illparent+plen_illchild+plen_illspouse)
    d['PFL_plen'] <- with(d, ifelse(PFL_plen>maxlen_PFL,maxlen_PFL,PFL_plen))
    # evenly distributed cap among leave types
    d['reduce'] <- with(d, ifelse(plen_bond+plen_illparent+plen_illchild+plen_illspouse!=0, 
                                  PFL_plen/(plen_bond+plen_illparent+plen_illchild+plen_illspouse),0))
    d['plen_bond']=round(d[,'plen_bond']*d[,'reduce'])
    d['plen_illchild']=round(d[,'plen_illchild']*d[,'reduce'])
    d['plen_illspouse']=round(d[,'plen_illspouse']*d[,'reduce'])
    d['plen_illparent']=round(d[,'plen_illparent']*d[,'reduce'])
  }
  
  # apply cap for all leaves
  if (maxlen_total!=maxlen_DI+maxlen_PFL | maxlen_total!=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond+maxlen_bond+maxlen_matdis) {
    d['particip_length']=0
    for (i in leave_types) {
      plen_var=paste("plen_",i,sep="")
      d <- d %>% mutate(particip_length=particip_length+get(plen_var))
    }
    d['particip_length'] <- with(d, ifelse(particip_length>maxlen_total,maxlen_total,particip_length))
    d['reduce'] <- with(d, ifelse(plen_matdis+plen_own+plen_bond+plen_illparent+plen_illchild+plen_illspouse!=0, 
                                  particip_length/(plen_matdis+plen_own+plen_bond+plen_illparent+plen_illchild+plen_illspouse),0))
    
    # evenly distributed cap among leave types
    d['plen_matdis']=round(d[,'plen_matdis']*d[,'reduce'])
    d['plen_own']=round(d[,'plen_own']*d[,'reduce'])
    d['plen_bond']=round(d[,'plen_bond']*d[,'reduce'])
    d['plen_illchild']=round(d[,'plen_illchild']*d[,'reduce'])
    d['plen_illspouse']=round(d[,'plen_illspouse']*d[,'reduce'])
    d['plen_illparent']=round(d[,'plen_illparent']*d[,'reduce'])
    
    # recalculate DI/PFL lengths
    d <- d %>% mutate(DI_plen=plen_matdis+plen_own)
    d <- d %>% mutate(PFL_plen=plen_bond+plen_illparent+plen_illchild+plen_illspouse)
  }
  
  # cap benefit payments at program's weekly benefit cap
  d <- d %>% mutate(actual_benefits= ifelse(actual_benefits>week_bene_cap*particip_length/5,
                                            week_bene_cap*particip_length/5, actual_benefits))
  
  # make sure those with particip_length 0 are also particip 0
  d <- d %>% mutate(particip= ifelse(particip_length==0,0, particip))
  
  
  # calculate leave specific benefits
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    ben_var=paste("bene_",i,sep="")  
    d[ben_var] <- with(d, actual_benefits*(get(len_var)/total_length))
    d[ben_var] <- with(d, ifelse(is.na(get(ben_var)),0,get(ben_var)))
  }
  
  return(d)
}
