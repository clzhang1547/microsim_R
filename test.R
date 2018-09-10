# This program cleans CPS data and runs a number of logit
# regressions to produce coefficient estimates (which are stored as csv files)
# to be used in the simulation model.

cat("\014")  
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("dplyr")
library("survey")

require(MASS)
require(stringr)
require(mvord)
require(ordinal)

#options(error=recover)

df <- read.csv("CPS2014extract.csv")

# ---------------------------------------------------------------------------------------------------------
# 1. Clean CPS
# ---------------------------------------------------------------------------------------------------------
#Create dummies for logit regressions
# Gender
df <- df %>% mutate(male = ifelse(a_sex == 1,1,0),
                    female = ifelse(a_sex == 2,1,0))

# Education
df <- df %>% mutate(lths = ifelse(a_hga <= 38,1,0),
                    somecol = ifelse(a_hga >= 40 & a_hga<=42,1,0),
                    ba = ifelse(a_hga == 43,1,0),
                    maplus = ifelse(a_hga >= 44,1,0))
# Race
df <- df %>% mutate(black = ifelse(prdtrace==2 & pehspnon==2,1,0),
                    asian = ifelse(prdtrace==4 & pehspnon==2,1,0),
                    other = ifelse(((prdtrace==3)|((prdtrace>=5)&(prdtrace<=26)))&(pehspnon==2),1,0),
                    hispanic = ifelse(pehspnon==1,1,0))

# age squared
df <- df %>% mutate(agesq = a_age*a_age)

# occupation
df <- df %>% mutate(occ_1 = ifelse(a_mjocc == 1,1,0),
                    occ_2 = ifelse(a_mjocc == 2,1,0),
                    occ_3 = ifelse(a_mjocc == 3,1,0),
                    occ_4 = ifelse(a_mjocc == 4,1,0),
                    occ_5 = ifelse(a_mjocc == 5,1,0),
                    occ_6 = ifelse(a_mjocc == 6,1,0),
                    occ_7 = ifelse(a_mjocc == 7,1,0),
                    occ_8 = ifelse(a_mjocc == 8,1,0),
                    occ_9 = ifelse(a_mjocc == 9,1,0),
                    occ_10 = ifelse(a_mjocc == 10,1,0))
df <- df %>% mutate(occ_1 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_1),
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
df <- df %>% mutate(ind_1 = ifelse(a_mjind == 1,1,0),
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
df <- df %>% mutate(ind_1 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_1),
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
df <- df %>% mutate(paid_hrly= ifelse(prerelg == 1, 0,NA))
df <- df %>% mutate(paid_hrly= ifelse(prerelg == 1 & a_hrlywk == 1, 1, paid_hrly))

# Making zero/negative earnings into NaN so we can take natural log
df <- df %>% mutate(lnearn = ifelse(pearnval<=0,NA,pearnval))  
df <- df %>% mutate(lnearn = log(lnearn))  
# Making other values 0 per ACM code
df <- df %>% mutate(lnearn = ifelse(pearnval<=0, 0, lnearn))  

# employer provided health insurance
df$hiemp <- as.numeric(df$hiemp)
df <- df %>% mutate(hiemp = ifelse(hiemp == 0, NA, hiemp))  
df <- df %>% mutate(hiemp = ifelse(hiemp == 2, 0, hiemp))  

# id variable
id <- rownames(df)
df <- cbind(id=id, df)


#write.csv(df, file = "CPS_extract_clean.csv", row.names = FALSE)

# ---------------------------------------------------------------------------------------------------------
# 2. Define Functions
# ---------------------------------------------------------------------------------------------------------

# Function to run logits
runLogit <- function(x,y,z){
  des <- svydesign(id = ~1,  weights = as.formula(z), data = df %>% filter_(y))
  svyglm(as.formula(x),data = df %>% filter_(y),family = "quasibinomial",design = des)
}

runOrdinal <- function(x,y,z){
  output <- polr(as.formula(x), data = df %>% filter_(y), weights=marsupwt)
  print(output)
}

runEstimate <- function(x,y,z,lname, mname){
  
  # Apply function to all leave types
  if (mname == "logit") {
    complete <- mapply(runLogit, x = x, y = y, z = z, SIMPLIFY = FALSE)
  }
  if (mname == "ordinal") {
    complete <- mapply(runOrdinal, x = x, y = y, z = z, SIMPLIFY = FALSE)
  }
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
# 3. Run logit models
# ---------------------------------------------------------------------------------------------------------

# logit for hourly paid regression

specif = c(paid_hrly =  paste("paid_hrly ~ female + black + a_age + agesq + ba",
                              "+ maplus + occ_1 + occ_3 + occ_5 + occ_7 + occ_8",
                              "+ occ_9 + occ_10 + ind_5 + ind_8 + ind_11 + ind_12"))
conditional = c(paid_hrly= "TRUE")
weight = c(paid_hrly = "~ marsupwt")

runEstimate(specif,conditional,weight,"cps_impute", "logit")

# ordered logit for number of employers, weeks worked 

 specif = c(num_employers= paste("factor(phmemprs) ~ a_age + agesq + asian + hispanic",
                                "+ lths + somecol + ba + maplus + lnearn",
                                "+ hiemp + ind_4 + ind_5 + ind_6 + ind_8",
                                "+ ind_13 + occ_1 + occ_6 + occ_7 + occ_9 + occ_10"))
cond = c(num_employers= "TRUE")
weight = c(num_employers = "marsupwt")


runEstimate(specif,conditional,weight,"cps_impute_ord", "ordinal")

