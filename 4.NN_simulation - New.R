# build a NN K=1 function from scratch
KNN1_scratch <- function(train, test, id_var) { 
  
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
    train[i] <- scale(train[i],center=0,scale=max(train[,i]))  
  } 
  
  # find distance
  temp <- test
  temp$min_dist <- ncol(train)
  temp$nbor <- NA
  
  for (j in seq(1,nrow(test))) {
    print(j)
    for (i in seq(1,nrow(train))) {
      dist <- distance(c(unlist(unname(train[i,]))),c(unlist(unname(test[j,]))), measure="euclidean")
      if (temp[j,"min_dist"]> dist) {
        temp[j,"min_dist"] <- dist
        temp[j,"nbor"] <- i
      }
    }
  }
}

d_train <- read.csv("fmla_clean_2012.csv")
d_test <- read.csv("ACS_clean.csv")


xvars <- c("empid","widowed", "divorced", "separated", "nevermarried", "female", 
           "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
           "white", "asian", "hisp","nochildren")

# filter fmla cases
d_temp <- d_train
d_temp <- d_temp[c(xvars)]
d_temp <- d_temp %>% filter(complete.cases(.))

# create labels, training data
train <- d_temp[c(xvars)]

# filter out acs vars
temp_test <- d_test
temp_test <- temp_test %>% filter(complete.cases(temp_test[c(xvars)]))

# create test data set 
test <- temp_test[c(xvars)]

KNN1_scratch(train,test,"empid")
