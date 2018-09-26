
# """
# Program to create a basic k=1 Nearest Neighbor imputation of leave-taking behavior in ACS data from FMLA observations
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
library("varhandle")

options(error=recover)
#options(error=NULL)

# build a NN K=1 function from scratch
KNN1_scratch <- function(d_train, d_test, id_var, imp_var, train_cond, test_cond) { 
  
  # Data manipulation
  
  xvars <- c("empid", "widowed", "divorced", "separated", "nevermarried", "female", 
             "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
             "white", "asian", "hisp","nochildren")
  
  # filter fmla m_test
  
  d_temp <- d_train %>% filter_(train_cond)
  d_temp <- d_temp[c(imp_var, xvars)]
  d_temp <- d_temp %>% filter(complete.cases(.))
  
  # create labels, training data
  train <- d_temp[c(xvars)]
  
  # filter out acs vars
  temp_test <- d_test %>% filter_(test_cond)
  temp_test <- temp_test %>% filter(complete.cases(temp_test[c(xvars)]))
  
  # create test data set 
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
  temp <- test
  temp$min_dist <- ncol(train)
  temp$nbor <- NA
  
  m_test <- as.matrix(test)
  m_train <-as.matrix(train)
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
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
  
  # for (i in seq(1,nrow(m_train))) {
  #   nest_train[i] <- list(m_train[i,2:cols])
  # }
  
  
  # mark neighbor as minimium distance
  
  min_start <- ncol(train)
  
  f1 <- function(j) {
    min_dist <- min_start
    nbor <- NA
    count <- 0
    mapply(f2, i=nest_train,moreArgs=list(j,min_dist,nbor))
  }

  f2 <- function(i,k,min_dist,nbor) {
    dist <- find_dist(i,j)
    count <- count + 1
    if (min_dist > dist) {
      min_dist <- dist
      nbor <- count
    }
    list(dist, nbor)
  }
  
  z <- lapply(nest_test, f1)
  pause()
  for (j in seq(1,nrow(m_test))) {
      for (i in seq(1,nrow(m_train))) {
        dist <- find_dist(m_test[j,2:cols], m_train[i,2:cols] )
        if (temp[j,"min_dist"]> dist ) {
           temp[j,"min_dist"] <- dist
           temp[j,"nbor"] <- d_temp[i,'empid']
        }
      }
  }
  # impute neighbor's variable of interest
  d_train$nbor <- d_train$empid
  temp <- join(temp[c("empid","nbor")], d_train[c("nbor",imp_var)], by=c("nbor"), type="left")
  return(temp[c("empid",imp_var)])
}

runKNNestimate <- function(d_train,d_test,id_var,imp_var,train_cond,test_cond,lname) {
  # get KNN estimates for all leave types
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
  return(est_df)
  
}

d_fmla <- read.csv("fmla_clean_2012.csv")
d_acs <- read.csv("ACS_clean.csv")

classes <- c(own = "take_own")

conditional <- c(own = "TRUE")

start_time <-Sys.time()
predict <- runKNNestimate(d_fmla,d_acs,"empid", classes, conditional, conditional, "take_")
end_time <- Sys.time()
print(end_time - start_time)