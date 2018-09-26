
runKNN <- function(d_train,d_test,y,z,test_cond) {
  
  xvars <- c("widowed", "divorced", "separated", "nevermarried", "female", 
             "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
             "white", "asian", "hisp","nochildren")
  
  # filter fmla cases
  d_temp <- d_train %>% filter_(z)
  d_temp <- d_temp[c(y, xvars)]
  d_temp <- d_temp %>% filter(complete.cases(.))
  
  # create labels, training data
  label <- d_temp[c(y)]
  train <- d_temp[c(xvars)]
  
  # filter out acs vars
  temp_test <- d_test %>% filter_(test_cond)
  temp_test <- temp_test %>% filter(complete.cases(temp_test[c(xvars)]))
  
  # create test data set 
  test <- temp_test[c(xvars)]
  
  # estimate KNN
  estimate <-knn(train, test, as.factor(label[,y]), k=1, use.all=FALSE)
  
  # NOT YET ADDRESSED
  # in case of ties, knn() function randomly assigns neighbor among tied candidates
  # fixing random seed to preserve replicability 
  
  
  return(data.frame(temp_test["empid"],unfactor(estimate)))
}


runKNNestimate <- function(d_train,d_test,y,z,test_cond,lname) {
  # get KNN estimates for all leave types
  estimates <- mapply(runKNN,y=y,z=z, test_cond=test_cond, MoreArgs=list(d_train,d_test), SIMPLIFY = FALSE)
  names(estimates) <-paste(lname,names(estimates),sep= "")
  
  # compile estimates into a single dataframe
  est_df <- data.frame(row.names=d_test$empid)
  est_df$empid <- d_test$empid
  j <- 0
  
  for (i in estimates) {
    j <- j+1
    colnames(i) <- c("empid", names(estimates)[j] )
    est_df <- merge(est_df, i, by="empid", all.x=TRUE)
  }
  return(est_df)
  
}


# # ---------------------------------------------------------------------------------------------------------
# 3. Days of Leave taken for multiple leave takers
# REMOVED - JUST GOING TO DO AT END OF PRE-IMPUTATION FUNCTIONS
# ---------------------------------------------------------------------------------------------------------
#Days of leave taken - KNN imputation from most recent leaves

#   Leave lengths are the same, except for own leaves, which are instead taken from the distribution of leave takers in FMLA survey reporting 
#   receiving some pay from state programs



# 
# if (leaveprogram==TRUE) {
#   conditional <- c(own = "recStatePay == 1 & length_own>0 & is.na(length_own)==FALSE",
#                    illspouse = " length_illspouse>0 & is.na(length_illspouse)==FALSE & nevermarried == 0 & divorced == 0 & recStatePay == 1",
#                    illchild = "length_illchild>0 & is.na(length_illchild)==FALSE & recStatePay == 1",
#                    illparent = "length_illparent>0 & is.na(length_illparent)==FALSE & recStatePay == 1",
#                    matdis = "length_matdis>0 & is.na(length_matdis)==FALSE & female == 1 & nochildren == 0 & recStatePay == 1",
#                    bond = "nochildren == 0 & recStatePay == 1")
# }
# 
# if (leaveprogram==FALSE) {
#   conditional <- c(own = "recStatePay == 0",
#                    illspouse = "nevermarried == 0 & divorced == 0 & recStatePay == 0",
#                    illchild = "recStatePay == 0",
#                    illparent = "recStatePay == 0",
#                    matdis = "female == 1 & nochildren == 0 & recStatePay == 0",
#                    bond = "nochildren == 0 & recStatePay == 0")
# }
# 
# test_conditional <- c(own = "take_own==1 & num_leaves_taken>1", 
#                       illspouse = "take_illspouse==1 & num_leaves_taken>1",
#                       illchild = "take_illchild==1  & num_leaves_taken>1",
#                       illparent = "take_illparent==1  & num_leaves_taken>1",
#                       matdis = "take_matdis==1  & num_leaves_taken>1",
#                       bond = "take_bond==1  & num_leaves_taken>1")
# 
# d_fmla <- impute_leave_length(d_fmla, d_fmla, conditional, test_conditional,leaveprogram)

# -----------LEAVEPROBABILITYFACTORS-----------------------------------------------------------------------------------------------------------------
# allow for users to adjust leave taking probability by type
# CANNOT IMPLEMENT THIS PARAMETER WHEN NOT USING LOGIT
# PROBABILITIES OF TAKING LEAVE ARE NOT ESTIMATED WITH KNN method


# LEAVEPROBABILITYFACTORS <- function(d, illchild_prob,own_prob, matdis_prob, bond_prob, illparent_prob, 
#                                     illspouse_prob) {
#   for (i in leave_types) {
#     prob_val=paste(i,"_prob",sep="")
#     take_var=paste("take_",i,sep="")
#     d['rand']=runif(nrow(d))
#     d['prob_val']=prob_val
#     if (prob_val<1) {
#       d <- d %>% mutate(take_var=ifelse(rand>prob_val,0,take_var))
#     }
#     if (prob_val>1) {
#       d <- d %>% mutate(take_var=ifelse(rand<1/prob_val,1,take_var))
#     }
#   }
#   d <- d[, !(names(d) %in% c('rand'))]
# }