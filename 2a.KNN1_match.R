
# """
# Program to create a basic k=1 Nearest Neighbor imputation of leave-taking behavior in ACS data from FMLA observations
#  
# 9 Sept 2018
# Luke
#
# """

# build a NN K=1 function from scratch
# H: Let's run this on a super simple dataset to see if it gets the same results as a canned package.
    # L: I put an R file doing this entitled "KNN1_testing.R" on my github. Does that satisfy your comment?
KNN1_scratch <- function(d_train, d_test, id_var, imp_var, train_cond, test_cond) { 
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.
  
  # Data manipulation
  
  # H:  Maybe we should take these outside the function
        #L: agreed, definitely is something to be cleaned up
  xvars <- c("empid", "widowed", "divorced", "separated", "nevermarried", "female", 
             "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
             "white", "asian", "hisp","nochildren")
  
  # filter fmla m_test
  # filter on conditions, select just the variables we want to impute and the variables used to calculate distance
  # H: Use dplyr select here. ALl 3 lines could just be one line
      # L: Great, I will do that
  temp_train <- d_train %>% filter_(train_cond)
  temp_train <- temp_train[c(imp_var, xvars)]
  temp_train <- temp_train %>% filter(complete.cases(.))
  
  # create labels, training data
  # This is a dataframe just with the variables in the fmla that will be used to compute distance
  train <- temp_train[c(xvars)]
  
  # filter out acs vars
  temp_test <- d_test %>% filter_(test_cond)
  temp_test <- temp_test %>% filter(complete.cases(temp_test[c(xvars)]))
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  test <- temp_test[c(xvars)]
  
  
  # Initial checks
  
  # check for data frames
  if ((!is.data.frame(train)) | (!is.data.frame(test))) {
    stop("train_set and test_set must be data frames")
  }  
  
  # check for missing data
  if (anyNA(train) | anyNA(test)) {
    stop("missing values not allowed in train_test or test_set")
  }
  
  # make sure colnames of train and test data are the same
  if (!all(colnames(train)==colnames(test))) {
    stop("Error: columns of train and test differ from one another")
  }
  
  # normalize training data to equally weight differences between variables
  # H: I'm not sure whether we normalize before or after computing the distance. We should check this.
        # L: This is normalizing before checking the distance. I'm pretty sure that's where it should happen.
  for (i in colnames(train)) {
    if (i != id_var & sum(train[i])!=0 ){
      train[i] <- scale(train[i],center=0,scale=max(train[,i]))
    }
  } 
  
  for (i in colnames(test)) {
    if (i != id_var & sum(test[i])!=0 ){
      test[i] <- scale(test[i],center=0,scale=max(test[,i]))
    }
  } 
  
  # id var must be first variable of data
  
  # find distance

  m_test <- as.matrix(test)
  m_train <-as.matrix(train)
  
  cols <-ncol(m_test)
  nest_test <- list()
  nest_train <- list()
  # nested lists of vectors for apply functions
  
  nest_test <- lapply(seq(1,nrow(m_test)) , function(y){ 
    m_test[y,2:cols]
  })
  nest_train <- lapply(seq(1,nrow(m_train)) , function(y){ 
    m_train[y,2:cols]
  })
  
  # mark neighbor as minimium distance
  start_time <-Sys.time()
  
  min_start <- ncol(train)
  
  f1 <- function(j) {
    min_dist <- min_start
    nbor <- NA
    d <- mapply(find_dist, x=nest_train, MoreArgs=list(y=j))
    return(which.min(d))
  }
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
  temp <- lapply(nest_test, f1)
  temp <- unlist(temp)
  temp <- cbind(temp_test["empid"],as.data.frame(unlist(temp)))
  colnames(temp)[colnames(temp)=="unlist(temp)"] <- "nbor"
  temp_train$nbor <- rownames(temp_train)
  temp <- join(temp[c("empid","nbor")], temp_train[c("nbor",imp_var)], by=c("nbor"), type="left")
  return(temp[c("empid",imp_var)])
}

runKNNestimate <- function(d_train,d_test,id_var,imp_var,train_cond,test_cond,lname) {
  # get KNN estimates for all leave types
  # This creates a list of list of 6 lists (one for each leve type, with indicator variables for each empid)
  # H:  Why is mapply needed here? Can't we just call KNN1_scratch? I'm not sure what we're looping over
  #     I believe that est_df will be the same as the dataset that's returned if KNN1_scratch is simply called directly
      # L: You're right, I don't think I use this as an iterable currently. 
      #    When I rewrite to reduce KNN calls, I believe mapply will come in handy here
  
  estimates <- mapply(KNN1_scratch, imp_var=imp_var,train_cond=train_cond, test_cond=test_cond, MoreArgs=list(d_train,d_test,id_var), SIMPLIFY = FALSE)
  # compile estimates into a single dataframe
  est_df <- data.frame(row.names=d_test$empid)
  est_df$empid <- d_test$empid
  j <- 0
  
  for (i in estimates) {
    j <- j+1
    colnames(i) <- c("empid", paste(lname,names(estimates)[j],sep="") )
    est_df <- merge(est_df, i, by="empid", all.x=TRUE)
  }
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.
  return(est_df)
  
}
