#########################################################################
########## MICROSIMULATION LEAVE ANALYSIS: PRE PROCESSING DATA ##########
#########################################################################
# --------------------------------------------------------------------
# this piece of code replicates the employees_var.do STATA do-file
# --------------------------------------------------------------------
clean_fmla <-function(csv=FALSE) {
  
  fmla_2012 <- read.csv("fmla_2012_employee_restrict_puf.csv")
  
  # --------------------------------------------------------------------
  # demographic characteristics
  # --------------------------------------------------------------------
  
  # FMLA eligible worker
  fmla_2012_clean <- fmla_2012 %>% mutate(eligworker = ifelse(E13 == 1 & (E14 == 1 | (E15_CAT >= 5 & E15_CAT <= 8)),1,0))
  
  # covered workplace
  fmla_2012_clean <- fmla_2012_clean %>% mutate(covwrkplace = ifelse(E11 == 1 | (E12 >= 6 & E12 <=9),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(covwrkplace = ifelse(is.na(covwrkplace) ==  1,0,covwrkplace),
                                                cond1 = ifelse(is.na(E11) == 1 & is.na(E12) == 1,1,0),
                                                cond2 = ifelse(E11 == 2 & is.na(E11) == 0 & is.na(E12) == 1,1,0),
                                                miscond = ifelse(cond1 == 1 | cond2 == 1,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(covwrkplace = ifelse(miscond == 1,NA,covwrkplace))
  
  # covered and eligible 
  fmla_2012_clean <-  fmla_2012_clean %>% mutate(coveligd = ifelse(covwrkplace == 1 & eligworker == 1,1,0))
  
  # hourly worker
  fmla_2012_clean <- fmla_2012_clean %>% mutate(hourly = ifelse(E9_1 == 2,1,0))
  
  # union member
  fmla_2012_clean <- fmla_2012_clean %>% mutate(union = ifelse(D3 == 1,1,0))
  
  # age at midpoint of category
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 1,21,AGE_CAT))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 2,27,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 3,32,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 4,37,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 5,42,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 6,47,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 7,52,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 8,57,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 9,63.5,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(age = ifelse(AGE_CAT == 10,70,age))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(agesq = age^2)
  
  
  
  # sex
  fmla_2012_clean <- fmla_2012_clean %>% mutate(male = ifelse(GENDER_CAT == 1,1,0),
                                                female = ifelse(GENDER_CAT == 2,1,0))
  
  # no children
  fmla_2012_clean <- fmla_2012_clean %>% mutate(nochildren = ifelse(D7_CAT==0,1,0))  
  
  # educational level
  fmla_2012_clean <- fmla_2012_clean %>% mutate(ltHS = ifelse(D1_CAT == 1,1,0),
                                                someHS = ifelse(D1_CAT == 2,1,0),
                                                HSgrad = ifelse(D1_CAT == 3,1,0),
                                                someCol = ifelse(D1_CAT == 5,1,0),
                                                BA = ifelse(D1_CAT == 6,1,0),
                                                GradSch = ifelse(D1_CAT == 7,1,0),
                                                noHSdegree = ifelse(ltHS == 1 | someHS == 1,1,0),
                                                BAplus = ifelse((BA == 1 | GradSch == 1),1,0))
  
  # family income using midpoint of category
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 3,15,NA))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 4,25,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 5,32.5,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 6,37.5,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 7,45,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 8,62.5,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 9,87.5,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(faminc = ifelse(D4_CAT == 10,130,faminc))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(lnfaminc = log(faminc))
  
  # marital status
  fmla_2012_clean <- fmla_2012_clean %>%  mutate(married = ifelse(D10 == 1,1,0),
                                                 partner = ifelse(D10 == 2,1,0),
                                                 separated = ifelse(D10 == 3,1,0),
                                                 divorced = ifelse(D10 == 4,1,0),
                                                 widowed = ifelse(D10 == 5,1,0),
                                                 nevermarried = ifelse(D10 == 6,1,0))
  
  # race/ethnicity
  fmla_2012_clean <- fmla_2012_clean %>% mutate(raceth = ifelse(is.na(D5) == 0 & D5 == 1,7,D6_1_CAT),
                                                native = ifelse(raceth == 1,1,0),
                                                asian = ifelse(raceth == 2,1,0),
                                                black = ifelse(raceth == 4,1,0),
                                                white = ifelse(raceth == 5,1,0),
                                                other = ifelse(raceth == 6,1,0),
                                                hisp = ifelse(raceth == 7,1,0))
  # --------------------------------------------------------------------
  # leave characteristics
  # --------------------------------------------------------------------
  
  # number of reasons leaves taken
  fmla_2012_clean <- fmla_2012_clean %>% mutate(num_leaves_taken=A4a_CAT)
  fmla_2012_clean <- fmla_2012_clean %>% mutate(num_leaves_taken=ifelse(is.na(num_leaves_taken)== TRUE, 0 ,num_leaves_taken))
  
  # number of reasons leaves needed
  fmla_2012_clean <- fmla_2012_clean %>% mutate(num_leaves_need=B5_CAT)
  fmla_2012_clean <- fmla_2012_clean %>% mutate(num_leaves_need=ifelse(is.na(num_leaves_need)== TRUE, 0 ,num_leaves_need))
  
  # leave reason for most recent leave
  fmla_2012_clean <- fmla_2012_clean %>% mutate(reason_take = ifelse(is.na(A20) == FALSE & A20 == 2,A5_2_CAT,A5_1_CAT))
  
  # leave reason for most recent leave (revised)
  fmla_2012_clean <- fmla_2012_clean %>% mutate(reason_take_rev = ifelse(is.na(A20) == FALSE & A20 == 2,A5_2_CAT_REV,A5_1_CAT_rev))
  
  # leave reason for longest leave (if different)
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_reason = ifelse(is.na(A20) == FALSE & A20 == 2,A5_1_CAT,NA))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_reason = ifelse(long_reason==reason_take,NA,long_reason))
  
  # taken doctor
  fmla_2012_clean <- fmla_2012_clean %>% mutate(YNdoctor_take = ifelse(is.na(A20) == FALSE & A20 == 2,A11_2,A11_1),
                                                doctor_take = ifelse(YNdoctor_take == 1,1,0))
  
  # taken hospital
  fmla_2012_clean <- fmla_2012_clean %>% mutate(YNhospital_take = ifelse(is.na(A20) == FALSE & A20 == 2, A12_2, A12_1),
                                                hospital_take = ifelse(YNhospital_take == 1, 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(hospital_take = ifelse(is.na(hospital_take) == TRUE & doctor_take == 0, 0, hospital_take))
  
  # need doctor
  fmla_2012_clean <- fmla_2012_clean %>% mutate(doctor_need = ifelse(B12_1 == 1, 1, 0))
  
  # need hospital
  fmla_2012_clean <- fmla_2012_clean %>% mutate(hospital_need = ifelse(B13_1 == 1, 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(hospital_need = ifelse(is.na(hospital_need) == TRUE & doctor_need == 0, 0, hospital_need))
  
  # taken or needed doctor or hospital for leave category
  fmla_2012_clean <- fmla_2012_clean %>% mutate(doctor1 = ifelse(is.na(LEAVE_CAT) == FALSE & LEAVE_CAT == 2, doctor_need, doctor_take))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(doctor2 = ifelse(is.na(LEAVE_CAT) == FALSE & (LEAVE_CAT == 2 | LEAVE_CAT == 4), doctor_need, doctor_take))  
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(hospital1 = ifelse(is.na(LEAVE_CAT) == FALSE & LEAVE_CAT == 2, hospital_need, hospital_take))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(hospital2 = ifelse(is.na(LEAVE_CAT) == FALSE & (LEAVE_CAT == 2 | LEAVE_CAT == 4), hospital_need, hospital_take))  
  
  # length of leave for most recent leave
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length = ifelse(is.na(A20) == FALSE & A20 == 2, A19_2_CAT_rev, A19_1_CAT_rev))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_length = ifelse(is.na(A20) == FALSE & A20 == 2, A19_1_CAT_rev, NA))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_length = ifelse(long_reason==reason_take,NA,long_length))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(lengthsq = length^2,
                                                lnlength = log(length),
                                                lnlengthsq = lnlength^2)
  
  # --------------------------
  # Benefits and pay received
  # --------------------------
  
  # fully paid
  fmla_2012_clean <- fmla_2012_clean %>% mutate(fullyPaid = ifelse(A49 == 1, 1, 0))
  
  # longer leave if more pay
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longerLeave = ifelse(A55 == 1, 1, 0))
  
  # could not afford to take leave
  fmla_2012_clean <- fmla_2012_clean %>% mutate(unaffordable = ifelse(B15_1_CAT == 5, 1, 0))
  
  # any pay received
  fmla_2012_clean <- fmla_2012_clean %>% mutate(anypay = ifelse(A45 == 1, 1, 0))
  
  # proportion of pay received from employer (mid point of ranges provided in FMLA)
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A50 == 1, .125, NA))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A50 == 2, .375, prop_pay))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A50 == 3, .5, prop_pay))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A50 == 4, .625, prop_pay))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A50 == 5, .875, prop_pay))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A49 == 1, 1, prop_pay))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(prop_pay = ifelse(A45 == 2, 0, prop_pay))
  
  # Adding values in leave program variables for starting condition (absence of program)
  
  # Leave Program Participation
  # baseline is absence of program, so this will start as a nonparticipant
  fmla_2012_clean  <- fmla_2012_clean  %>% mutate(particip = 0)
  
  # Benefits received as proportion of pay
  # baseline is employer-provided pay: starting at 0, will be imputed
  fmla_2012_clean  <- fmla_2012_clean  %>% mutate(benefit_prop = 0)
  
  # Cost to program as proportion of pay
  # baseline is 0
  fmla_2012_clean  <- fmla_2012_clean  %>% mutate(cost_prop = 0)
  
  # state program
  fmla_2012_clean <- fmla_2012_clean %>% mutate(recStateFL = ifelse(A48b == 1, 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(recStateFL = ifelse(is.na(recStateFL) == TRUE & anypay == 0, 0, recStateFL))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(recStateDL = ifelse(A48c == 1, 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(recStateDL = ifelse(is.na(recStateDL) == TRUE & anypay == 0, 0, recStateDL))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(recStatePay = ifelse(recStateFL == 1 | recStateDL == 1, 1, 0))
  
  # weights
  w_emp <- fmla_2012_clean %>% filter(LEAVE_CAT == 3) %>% summarise(w_emp = mean(weight))
  w_leave <- fmla_2012_clean %>% filter(LEAVE_CAT != 3) %>% summarise(w_leave = mean(weight))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(fixed_weight = ifelse(LEAVE_CAT == 3, w_emp, w_leave),
                                                freq_weight = round(weight))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(fixed_weight = unlist(fixed_weight))
  
  # --------------------------
  # dummies for leave type 
  # -------------------------- 
  
  # there are four variables for each leave type for most recent leave:
  # (1) taking a leave - take_*
  # (2) needing a leave - need_*
  # (3) taking or needing a leave - type_*
  # (4) length of most recent leave - length_*
  
  # also need to know if the longest leave is that type, and different from most recent type - long_*
  # Length of above leave - longlength_*
  
  # maternity disability
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_matdis = ifelse((A5_1_CAT == 21 & A11_1 == 1 & GENDER_CAT == 2 & (A20 != 2|is.na(A20)))
                                                                     | A5_1_CAT_rev == 32 , 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_matdis = ifelse(is.na(take_matdis) == 1,0,take_matdis))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_matdis = ifelse(is.na(A5_1_CAT) == 1 & is.na(A5_2_CAT) == 1, NA, take_matdis))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_matdis = ifelse(is.na(take_matdis) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, take_matdis))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_matdis = ifelse((A5_1_CAT == 21 & A11_1 == 1 & GENDER_CAT == 2 & A20 == 2 ) 
                                                                     | A5_1_CAT_rev == 32, 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_matdis = ifelse(is.na(long_matdis) == 1,0,long_matdis))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_matdis = ifelse(is.na(A5_1_CAT) == 1 & is.na(A5_2_CAT) == 1, NA, long_matdis))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_matdis = ifelse(is.na(long_matdis) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, long_matdis))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_matdis = ifelse((B6_1_CAT == 21 & B12_1 == 1 & GENDER_CAT == 2) | B6_1_CAT_rev == 32, 1, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_matdis = ifelse(is.na(need_matdis) == 1,0,need_matdis))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_matdis = ifelse(is.na(B6_1_CAT) == 1, NA, need_matdis))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_matdis = ifelse(is.na(need_matdis) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0, need_matdis))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_matdis = ifelse((take_matdis == 1 | need_matdis == 1),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_matdis = ifelse((is.na(take_matdis) == 1 | is.na(need_matdis) == 1),NA,type_matdis))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length_matdis = ifelse(take_matdis==1,length, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longlength_matdis = ifelse(long_matdis==1,long_length, 0))
  
  # new child/bond
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_bond = ifelse(A5_1_CAT==21 & (is.na(A11_1) == 1 | GENDER_CAT == 1 | (GENDER_CAT==2 & A11_1==2 & A5_1_CAT_rev!=32)) & (A20 != 2|is.na(A20)),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_bond = ifelse(is.na(A5_1_CAT) == 1, NA, take_bond))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_bond = ifelse(is.na(take_bond) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, take_bond))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_bond = ifelse(A5_1_CAT==21 & (is.na(A11_1) == 1 | GENDER_CAT == 1 | (GENDER_CAT==2 & A11_1==2 & A5_1_CAT_rev!=32)) & A20 == 2,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_bond = ifelse(is.na(A5_1_CAT) == 1, NA, long_bond))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_bond = ifelse(is.na(long_bond) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, long_bond))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_bond = ifelse(B6_1_CAT==21 & (is.na(B12_1) == 1 | GENDER_CAT == 1 | (GENDER_CAT==2 & B12_1==2 & B6_1_CAT_rev!=32)),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_bond = ifelse(is.na(B6_1_CAT) == 1, NA, need_bond))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_bond = ifelse(is.na(need_bond) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0, need_bond))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_bond = ifelse((take_bond == 1 | need_bond == 1),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_bond = ifelse((is.na(take_bond) == 1 | is.na(need_bond) == 1),NA,type_bond))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length_bond = ifelse(take_bond==1,length, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longlength_bond = ifelse(long_bond==1,long_length, 0))
  
  #odie = fmla_2012_clean %>% select(take_bond)
  # own health
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_own = ifelse(reason_take == 1,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_own = ifelse(is.na(take_own) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_own))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_own = ifelse(long_reason == 1,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_own = ifelse(is.na(long_own) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_own))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_own = ifelse(B6_1_CAT == 1|B6_2_CAT == 1,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_own = ifelse(is.na(need_own)==1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_own))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_own = ifelse((take_own == 1 | need_own == 1),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_own = ifelse((is.na(take_own) == 1 | is.na(need_own) == 1),NA,type_own))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length_own = ifelse(take_own==1,length, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longlength_own = ifelse(long_own==1,long_length, 0))
  
  #ill child
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_illchild = ifelse(reason_take == 11,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_illchild = ifelse(is.na(take_illchild) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_illchild))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_illchild = ifelse(long_reason == 11,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_illchild = ifelse(is.na(long_illchild) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_illchild))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_illchild = ifelse(B6_1_CAT == 11|B6_2_CAT == 11,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_illchild = ifelse(is.na(need_illchild) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_illchild))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_illchild = ifelse((take_illchild == 1 | need_illchild == 1),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_illchild = ifelse((is.na(take_illchild) == 1 | is.na(need_illchild) == 1),NA,type_illchild))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length_illchild = ifelse(take_illchild==1,length, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longlength_illchild = ifelse(long_illchild==1,long_length, 0))
  
  #ill spouse
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_illspouse = ifelse(reason_take == 12,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_illspouse = ifelse(is.na(take_illspouse) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_illspouse))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_illspouse = ifelse(long_reason == 12,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_illspouse = ifelse(is.na(long_illspouse) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_illspouse))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_illspouse = ifelse(B6_1_CAT == 12,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_illspouse = ifelse(is.na(need_illspouse) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_illspouse))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_illspouse = ifelse((take_illspouse == 1 | need_illspouse == 1),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_illspouse = ifelse((is.na(take_illspouse) == 1 | is.na(need_illspouse) == 1),NA,type_illspouse))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length_illspouse = ifelse(take_illspouse==1,length, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longlength_illspouse = ifelse(long_illspouse==1,long_length, 0))
  
  #ill parent
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_illparent = ifelse(reason_take == 13,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(take_illparent = ifelse(is.na(take_illparent) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_illparent))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_illparent = ifelse(long_reason == 13,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(long_illparent = ifelse(is.na(long_illparent) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_illparent))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_illparent = ifelse(B6_1_CAT == 13,1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(need_illparent = ifelse(is.na(need_illparent) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_illparent))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_illparent = ifelse((take_illparent == 1 | need_illparent == 1),1,0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(type_illparent = ifelse((is.na(take_illparent) == 1 | is.na(need_illparent) == 1),NA,type_illparent))
  
  fmla_2012_clean <- fmla_2012_clean %>% mutate(length_illparent = ifelse(take_illparent==1,length, 0))
  fmla_2012_clean <- fmla_2012_clean %>% mutate(longlength_illparent = ifelse(long_illparent==1,long_length, 0))
  
  # saving data
  if (csv==TRUE) {
    write.csv(fmla_2012_clean, file = "fmla_clean_2012.csv", row.names = FALSE)  
  }
  return(fmla_2012_clean)
}
  
