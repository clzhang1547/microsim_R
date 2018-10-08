

# """
# Program to create a basic k=1 Nearest Neighbor imputation of leave-taking behavior in ACS data from FMLA observations
#  
# 9 Sept 2018
# Luke
#
# """

# ---------------------------------------------------------------------------------------------------------
# 0. Define Functions
# ---------------------------------------------------------------------------------------------------------

#  function to loop through leaves types
fmla_impute <- function(filename, d_fmla, d_acs,leaveprogram) {
     # ---------------------------------------------------------------------------------------------------------
   # 1. the probability of taking a leave
   # ---------------------------------------------------------------------------------------------------------
  # classes of leave
  classes <- c(own = "take_own", 
               illspouse = "take_illspouse",
               illchild = "take_illchild",
               illparent = "take_illparent",
               matdis = "take_matdis",
               bond = "take_bond")
  # I forget the logic for these conditionals. We should discuss.
  conditional <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0",
                   bond = "nochildren == 0")
  
  # Produces a 7 column dataframe of empid and indicator of whether each type of leave was taken
  predict <- runKNNestimate(d_fmla,d_acs,"empid", classes, conditional, conditional, "take_")
  
  # Merge these with the acs. Use left_join here.
  d_acs <- merge(d_acs,predict, by="empid")
  
  
  # ---------------------------------------------------------------------------------------------------------
  # 2. Days of Leave Taken
  # ---------------------------------------------------------------------------------------------------------
  #Days of leave taken - currently takes length from most recent leave only
  classes <- c(own = "length_own",
               illspouse = "length_illspouse",
               illchild = "length_illchild",
               illparent = "length_illparent",
               matdis = "length_matdis",
               bond = "length_bond")
  
  #   Leave lengths are the same, except for own leaves, which are instead taken from the distribution of leave takers in FMLA survey reporting 
  #   receiving some pay from state programs
  # Why are these imputed separately? Couldn't this be done in the above? In other words, why doesn't the above also
  # grab the length from the nearest neighbor instead of just the take/don't take indicator?
  
  if (leaveprogram==TRUE) {
    conditional <- c(own = "recStatePay == 1 &length_own>0 & is.na(length_own)==FALSE",
                     illspouse = "recStatePay == 1 &length_illspouse>0 & is.na(length_illspouse)==FALSE & nevermarried == 0 & divorced == 0",
                     illchild = "recStatePay == 1 &length_illchild>0 & is.na(length_illchild)==FALSE",
                     illparent = "recStatePay == 1 &length_illparent>0 & is.na(length_illparent)==FALSE",
                     matdis = "recStatePay == 1 &length_matdis>0 & is.na(length_matdis)==FALSE & female == 1 & nochildren == 0",
                     bond = "recStatePay == 1 &length_bond>0 & is.na(length_bond)==FALSE & nochildren == 0")
  }
  if (leaveprogram==FALSE) {
    conditional <- c(own = "recStatePay == 0 & length_own>0 & is.na(length_own)==FALSE",
                     illspouse = "recStatePay == 0 & length_illspouse>0 & is.na(length_illspouse)==FALSE & nevermarried == 0 & divorced == 0",
                     illchild = "recStatePay == 0 & length_illchild>0 & is.na(length_illchild)==FALSE",
                     illparent = "recStatePay == 0 & length_illparent>0 & is.na(length_illparent)==FALSE",
                     matdis = "recStatePay == 0 & length_matdis>0 & is.na(length_matdis)==FALSE & female == 1 & nochildren == 0",
                     bond = "recStatePay == 0 & length_bond>0 & is.na(length_bond)==FALSE & nochildren == 0")
  } 
  
  test_conditional <- c(own = "take_own==1",
                        illspouse = "take_illspouse==1 & nevermarried == 0 & divorced == 0",
                        illchild = "take_illchild==1",
                        illparent = "take_illparent==1",
                        matdis = "take_matdis==1 & female == 1 & nochildren == 0",
                        bond = "take_bond==1 & nochildren == 0")
  
  d_acs <- impute_leave_length(d_fmla, d_acs, conditional, test_conditional)

  # ---------------------------------------------------------------------------------------------------------
  # 3. Other Leave characteristics needed
  # ---------------------------------------------------------------------------------------------------------
  classes <- c("prop_pay","unaffordable")
  
  conditional <- rep("TRUE",length(classes))
  
  predict<- runKNNestimate(d_fmla,d_acs,"empid",classes, conditional, conditional, "")
  
  d_acs <- merge(d_acs,predict, by="empid")

  write.csv(d_acs, file = paste(filename,".csv",sep=""), row.names = FALSE)
  return(d_acs)
}



