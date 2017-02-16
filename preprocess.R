data_analysis <- read.csv("/Users/yuyanzhang/Desktop/UVa/7th semester/Capstone/Data/CapstoneData_new.csv")
View(data_analysis)
nrow(data_analysis)
#Check how many distinctive patients are there in the dataset
length(which(duplicated(data_analysis$patient_number)==TRUE))
length(which(duplicated(data_analysis$patient_number)==FALSE))

#Check Missing data
length(c((which(is.na(data_analysis)))))/nrow(data_analysis) #51.09% of the data entries contain missing data

#Add in a new column to indicate the date of admission: admit_date
data_analysis$dschrg_dt <- as.Date(data_analysis$dschrg_dt,format="%Y-%m-%d")
#data_analysis$admit_date <- as.Date(data_analysis$dschrg_dt - data_analysis$days_stay_cnt,format="%m/%d/%Y")
data_analysis$admsn_dt <- as.Date(data_analysis$admsn_dt,format="%Y-%m-%d")
data_analysis$admit_date <- data_analysis$admsn_dt



#Aggregate Multiple entries in the data set
#Add new columns
#-----------------------------------#
#readmin_days: days after first readmission, -1 means not readmitted
#readmin_times: number of times being admitted before this admission
#-----------------------------------#
data_readmin = data_analysis
data_readmin$readmin_days = NA
data_readmin$readmin_times = NA
data_readmin <- data_readmin[(which(!is.na(data_readmin$patient_number))),]

#data_readmin$dschrg_dt <- as.Date(data_readmin$dschrg_dt,format = "%m/%d/%Y")
#Number of unique patients in the dataset
patient_num = nrow(data.frame(data_analysis[which(duplicated(data_analysis$patient_number)==FALSE),]))


#First sort by admit_date
data_readmin = data_readmin[order(data_readmin$admit_date),]
#Calculate values for new fields
count = 0
for (i in 1:patient_num){
  last_discharge_date = -1
  num_admin = 0
  for (j in 1:nrow(data_readmin)){
    if (data_readmin[j,1] == i){
      count = count +1
      if (last_discharge_date == -1){
        #First admission in the dataset
        last_discharge_date = data_readmin[j,2]
        data_readmin[j,31] = 0
        data_readmin[j,32] = 0
        num_admin = num_admin+1
        next
        
      }
      data_readmin[j,31] = as.integer(data_readmin[j,13]-last_discharge_date)
      data_readmin[j,32] = num_admin
      num_admin = num_admin+1
      last_discharge_date = data_readmin[j,2]
    }
  }
}
count

#Create a binary feature for unplanned 30-day readmission
#By definition, use only emergency and ugernt admission (admsn_type_cd: 1 and 2) within 30 days of discharge

#Since we cannot recover missing info about admission type, remove all records with missing values
data_readmin = data_readmin[which(!is.na(data_readmin$admsn_type)),]
data_readmin$admsn_type_cd
nrow(data_readmin) 

data_readmin$outcome <- NA
for ( i in 1:nrow(data_readmin)){
  data_readmin[i,]$outcome <- data_readmin[i,]$readmin_days <= 30 && data_readmin[i,]$readmin_days>0 && data_readmin[i,]$admsn_type %in% c("Emergency","Urgent")
}

for ( i in 1:nrow(data_readmin)){
  if (data_readmin[i,]$outcome == TRUE){
    data_readmin[i,]$outcome <- 1
  }else{
    data_readmin[i,]$outcome <- 0
  }
}


barplot(table(data_readmin$outcome))
length(which(data_readmin$outcome==TRUE))/nrow(data_readmin) #13.4 unplanned 30-day readmission


#Check if there is any missing data in the dataset
which(is.na(data_readmin), arr.ind = TRUE)
#Get rid of these 4 records
miss = which(is.na(data_readmin), arr.ind = TRUE)
nrow(data_readmin)
data_readmin = data_readmin[-c(as.vector(miss)[1:nrow(miss)]),]
nrow(data_readmin) 

#Add in a new column to indicate days passed since first day of the study: days_since
library(survival)
first_day = min(data_readmin$admit_date)
first_day
data_readmin$days_since = data_readmin$admit_date- first_day
data_readmin$days_since <- as.integer(data_readmin$days_since)
############################Fit Cox Proportional Model###########################################
#Create a survival object, outcome== 1 is unplanned readmission
surv <- Surv(data_readmin$days_since,  data_readmin$outcome == 1)
data_readmin$SurvObj <- with(data_readmin, Surv(days_since,  outcome == 1))

## Fit Cox regression
## Features
#"dschrg_cd"    +   "days_stay_cnt"  + "prcdr_cnt"  
# + "ed_route"        "diabetes"        "sex_cd"         
#  "hyperten"     #   "chf"        #     "cad"          #   "smoking"     
#"age"        +     "median_income"   + "readmin_days"    "readmin_times"

#Next steps (too many levels): admtg_diag_cd, "prncpl_diag_cd", "admsn_src_cd" 
#convert to factors
data_readmin$dschrg_cd <- as.factor(data_readmin$dschrg_cd)
levels(data_readmin$admit_diag_text)
data_readmin$admtg_diag_cd <- as.factor((data_readmin$admtg_diag_cd))
data_readmin$prncpl_diag_cd <- as.factor(data_readmin$prncpl_diag_cd)
data_readmin$admsn_src_cd <- as.factor(data_readmin$admsn_src_cd)
data_readmin$ed_route <- as.factor(data_readmin$ed_route)
data_readmin$diabetes <- as.factor(data_readmin$diabetes)
data_readmin$sex_cd <- as.factor(data_readmin$sex_cd)
data_readmin$hyperten <- as.factor(data_readmin$hyperten)
data_readmin$chf <- as.factor(data_readmin$chf)
data_readmin$cad <- as.factor(data_readmin$cad)
data_readmin$smoking <- as.factor(data_readmin$smoking)
data_readmin$readmin_days <- as.integer(data_readmin$readmin_days)
data_readmin$readmin_times <- as.integer(data_readmin$readmin_times)
data_readmin$age <- as.integer(data_readmin$age)
class(data_readmin$prncpl_category)

res.cox1 <- coxph(SurvObj ~ ., data =  data_readmin)
res.cox1
summary(res.cox1)

## Check for violation of proportional hazard (constant HR over time)
(res.zph1 <- cox.zph(res.cox1))

## Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve.
plot(res.zph1)


res.cox1.step <- step(res.cox1)
summary(res.cox1.step)

anova(res.cox1,res.cox1.step)


plot(cox.zph(res.cox1.step))


#With diagnosis code

res.cox2 <- coxph(SurvObj ~ dschrg_cd      + prcdr_cnt +
                    +diabetes+sex_cd+hyperten+chf+cad+smoking
                  + age +  median_income + readmin_days+readmin_times + prncpl_category, data =  data_readmin)
res.cox2
summary(res.cox2)

anova(res.cox1,res.cox2)


#Prediction
pred <- predict(res.cox2, data_readmin)
survConcordance(surv ~ predict(res.cox2))


#Setting up trainning set and testing set
set.seed(123456)
smp_size <- floor(0.75 * nrow(data_readmin))
train_ind <- sample(seq_len(nrow(data_readmin)), size = smp_size)
train <- data_readmin[train_ind, ]
test <- data_readmin[-train_ind, ]

#Fitting
res.cox2.split <- coxph(SurvObj ~ dschrg_cd      + prcdr_cnt +
                          +diabetes+sex_cd+hyperten+chf+cad+smoking
                        + age +  median_income + readmin_days+readmin_times + prncpl_category, data =  train)
summary(res.cox2.split)
pred <- predict(res.cox2.split, test)
survConcordance(test$SurvObj ~ pred)

res.cox1.split <- coxph(SurvObj ~ dschrg_cd      + prcdr_cnt +
                    +diabetes+sex_cd+hyperten+chf+cad+smoking
                  + age +  median_income + readmin_days+readmin_times, data =  train)
summary(res.cox1.split)
pred <- predict(res.cox1.split, test)
(survConcordance(test$SurvObj ~ pred))


#Cross validation k = 10, model 1
set.seed(1000)
k = floor(nrow(data_readmin)/10*9)
CI_test <- c()

for (i in (1:10)){
  train_ind <- sample((nrow(data_readmin)), size = k)
  train <- data_readmin[train_ind, ]
  test <- data_readmin[-train_ind, ]
  
  #Fitting
  res.cox2.split <- coxph(SurvObj ~ dschrg_cd      + prcdr_cnt +
                            +diabetes+sex_cd+hyperten+chf+cad+smoking
                          + age +  median_income + readmin_days+readmin_times + prncpl_category, data =  train)
 
  pred <- predict(res.cox2.split, test)
  print("Fold 1")
  print(survConcordance(test$SurvObj ~ pred))
  CI_test <- c(CI_test,survConcordance(test$SurvObj ~ pred)$concordance)
  
}

mean(CI_test)


#Cross validation k = 10, model 2
set.seed(1000)
k = nrow(data_readmin)/10*9
CI_test <- c()

for (i in (1:10)){
  train_ind <- sample((nrow(data_readmin)), size = k)
  train <- data_readmin[train_ind, ]
  test <- data_readmin[-train_ind, ]
  
  #Fitting
  res.cox1.split <- coxph(SurvObj ~ dschrg_cd      + prcdr_cnt +
                            +diabetes+sex_cd+hyperten+chf+cad+smoking
                          + age +  median_income + readmin_days+readmin_times, data =  train)
  pred <- predict(res.cox1.split, test)
  print("Fold 1")
  print(survConcordance(test$SurvObj ~ pred))
  CI_test <- c(CI_test,survConcordance(test$SurvObj ~ pred)$concordance)
  
}

mean(CI_test)

cox.step <- step(res.cox2)



#Cross validation k = 10, step model
set.seed(1000)
k = nrow(data_readmin)/10*9
CI_test <- c()

for (i in (1:10)){
  train_ind <- sample((nrow(data_readmin)), size = k)
  train <- data_readmin[train_ind, ]
  test <- data_readmin[-train_ind, ]
  
  #Fitting
  cox.step <- coxph(SurvObj ~ dschrg_cd + prcdr_cnt + diabetes + sex_cd + hyperten + 
                      chf + cad + smoking + age + readmin_days + readmin_times, data =  train)
  pred <- predict(cox.step, test)
  print("Fold 1 - step model")
  print(survConcordance(test$SurvObj ~ pred))
  CI_test <- c(CI_test,survConcordance(test$SurvObj ~ pred)$concordance)
  
}

mean(CI_test)

####################Comparing to random survival forest model####################
