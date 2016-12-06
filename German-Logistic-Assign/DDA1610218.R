setwd("E:\\UpGrad IIIT PGDDA Program\\Course 3\\Graded Assignment\\Logistic Regression")
#-------------------------------------------------------------------------------------------
#---------------------------------LOGISTIC REGRESSION---------------------------------------
#-------------------------------------------------------------------------------------------
library(caret)
library(car)
library(caTools)
library(ROCR)
library(MASS)
library(stats)
library(GGally)
library(ggplot2)
library(Hmisc)

###--------------------------- Checkpoint-1: Data Preparation-------------------------------

## Load the file german.csv into data frame german_credit

german_credit <- read.csv("german.csv",stringsAsFactors = FALSE)

#now we will check the structure of the file

str(german_credit)

#we see that there are 1000 obs with 20 Independent and 1 Dependent variable Default_Status.
#we also see that there are 13 character variables and 8 integer varaibles

#we will now check for any NA values in our data set

sum(is.na(german_credit))

#the data set does not have any NA values so we are good with it, we will also check if there are any duplicate rows

x <- unique(german_credit)

#since the rows of x is same as of our original data sets we don't have any duplicate rows, 
#Now we will perfom feature transformation of Age in years, so we will divide age into three equal bin of "junior","Mid_level",and "Senior"

#so lets do the transformation

german_credit$Age.in.Years <-
  ifelse(
    german_credit$Age.in.Years <= 35,
    "Juniour",
    ifelse(
      german_credit$Age.in.Years >= 36 &
        german_credit$Age.in.Years <= 45,
      "Mid_Level",
      ifelse(german_credit$Age.in.Years >= 46, "Seniour", "NA")
    )
  )

#now we will check if the transformation is correct 

Age_In_Year <- ggplot(german_credit,aes(x=german_credit$Age.in.Years)) + geom_bar(fill = "#3366FF") + ggtitle("Bar Graph of Age In Year") + xlab("Age In Year") + ylab("Count")

Age_In_Year

#now we will transfor credit month into equal bin of "short_term","medium_term","long_term"

german_credit$Duration.in.month <-
  ifelse(
    german_credit$Duration.in.month <= 24,
    "Short_Term",
    ifelse(
      german_credit$Duration.in.month >= 24 &
        german_credit$Duration.in.month <= 48,
      "Medium_Term",
      ifelse(german_credit$Duration.in.month >= 49, "Long_Term", "NA")
    )
  )

#now we will check if the transformation is correct 

credit_term <- ggplot(german_credit,aes(x=Duration.in.month)) + geom_bar(fill = "#3366FF") + ggtitle("Bar Graph of Duration In Months") + xlab("Duration In Months") + ylab("Count")

credit_term

#since we have done with our transformation we will check for the structure of the data set again

str(german_credit)

#----------------------- Checkpoint 2: Exploratory Data Analysis -------------------------------

###Univariate Analysis###

# Summary Statistics
# Distribution plot
# Outlier treatment

summary(german_credit)

#now we will check for outliers in credit amount by using density plot

credit_amount <- ggplot(german_credit,aes(x=Credit.amount)) + geom_density(col="red") + ggtitle("Density of Credit Amount") + xlab("Credit Amount") + ylab("Density")  

credit_amount

#the density of observation are from 250 to 18424, and the distribution is right skewd, so there is outliers
#we will again check with the boxplot

boxplot_credit_amount <- ggplot(german_credit,aes(x=1,y=german_credit$Credit.amount)) + geom_boxplot(fill = "white",color = "blue",outlier.colour = "red") + ggtitle("BoxPlot of Credit Amount") + xlab("Width of boxplot") + ylab("Credit Amount")  

boxplot_credit_amount

#we see that there are outliers presemt in the upper hinge, so we will use the IQR to cap the outliers

#first we will calculate the IQR

IQR <- quantile(german_credit$Credit.amount,.75) - quantile(german_credit$Credit.amount,.25)

#now we will store the value of 1.5*IQR + Q3 in cap_value

cap_value <- quantile(german_credit$Credit.amount,.75) + 1.5*IQR

#now we will impute cap_value to all the outliers

german_credit$Credit.amount[which(german_credit$Credit.amount %in% boxplot.stats(german_credit$Credit.amount)$out)] <- cap_value

#now we will again check for the outliers to see whether we have imputed correctly

ggplot(german_credit,aes(x=Credit.amount)) + geom_density(col="red") + ggtitle("Density of Credit Amount") + xlab("Credit Amount") + ylab("Density")

ggplot(german_credit,aes(x=1,y=german_credit$Credit.amount)) + geom_boxplot(fill = "white",color = "blue",outlier.colour = "red") + ggtitle("BoxPlot of Credit Amount") + xlab("Width of boxplot") + ylab("Credit Amount")

#now let's explore the categorical variables - Univariate Analysis----------------------------- 

##Status.of.existing.checking.account

ggplot(german_credit,aes(x=Status.of.existing.checking.account)) + geom_bar(color = "red",fill = "#3366FF") + ggtitle("Bar Graph of Existing Checking Account") + xlab("Status of existing checking account") + ylab("Count")

#we see that large number of loan is given to people who don't have existing checking account with this bank

##Credit History

ggplot(german_credit,aes(x=german_credit$Credit.history)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar Graph of Credit History") + xlab("Credit History") + ylab("Count")

#we see that most number of people given loan to have paid their existing credit with this account

##purpose

ggplot(german_credit,aes(x=Purpose)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar Graph of Purpose of Credit") + xlab("Purpose") + ylab("count")

#we see that mostly people take credit for radio/television or for new or old cars

##Savings.account.bonds

ggplot(german_credit,aes(x=Savings.account.bonds)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar Graph of Savings account bonds") + xlab("Savings account bonds") + ylab("Count")

#we see that mostly people have a saving account bond of less than 100$

##Present.employment.since.

ggplot(german_credit,aes(x=Present.employment.since.)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar Graph of Present employment since") + xlab("Present employment since") + ylab("Count")

#we see that most people take loan who are in the currect organisation for less than 4 year

##Personal.status.and.sex

ggplot(german_credit,aes(x=Personal.status.and.sex)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar graph of Personal status and sex") + xlab("Personal status and sex") + ylab("Count")

#we see that most people who take loan are male and are single

##Property

ggplot(german_credit,aes(x=Property)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar graph of Property") + xlab("Property") + ylab("Count")

#we see that most people take loan who have property such as car

##Housing

ggplot(german_credit,aes(x=Housing.)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar Graph of Housing") + xlab("Housing") + ylab("count")

#we see that most people are given loan who have their own house

##Job Status

ggplot(german_credit,aes(x=Job_status)) + geom_bar(color = "#3366FF",fill = "grey") + ggtitle("Bar graph of Job Satus") + xlab("Job Status") + ylab("Count")

#we see that most loans are given to skill employees / official

# Now, Let's move on to the analysis of more than one variable at a time. 

#-------------------------------------------------------------------------------------------------------

#### --------------------------------------Multivariate analysis ---------------------------------------

### Correlation table

#Lets subset the continous variables so that we can calculate correlation of all the pairs at the same time

german_credit_continous <- german_credit[,c(5,8,11,16,18,21)]
  
#Determine the correlation

german_credit_continous_matrix <- cor(german_credit_continous)

german_credit_continous_matrix

##Let's see the correlation matrix for all the continous variables

ggpairs(german_credit_continous)

##From the correlation matrix and the correlation values it is clear that 

# 1.) there is no correlation between any of the continous variables

### now we will show tha Distribution of dependent variable across different levels of two categorical variables:

#Duration in months vs credit.amount

ggplot(german_credit,aes(x=factor(Default_status))) + geom_bar(fill = "grey",col="#3366FF") + facet_wrap(~Duration.in.month) + ggtitle("Status of Default on Loan Term") + xlab("Default Status") + ylab("Count")

#we see that equal proportion of people who take loan for long term tends to default

##Credit History vs default status

ggplot(german_credit,aes(x=factor(Default_status))) + geom_bar(fill = "grey",col="#3366FF") + facet_wrap(~Credit.history) + ggtitle("Status Of Default on Credit History") + xlab("Default Status")

#we see that most of the loan were give to person who has paid back their loan on time 

##Puppose

ggplot(german_credit,aes(x=factor(Default_status))) + geom_bar(fill = "grey",col="#3366FF") + facet_wrap(~Purpose) + ggtitle("Loan Status on Purpose") + xlab("Default Status")

#we see that most of the loan which were taken for new car are not bein paid

#now we will check the structure of the data sets

str(german_credit)

#we see that spart from Credit.amount and Default_status all other variable should be of character type
#so we will create a seperate data frame for character variable and countinuous variable

german_credit_chr <- german_credit[,-c(5,21)]

#and we will create a DF for countinuous variable as well

german_credit_con <- german_credit[,c(5,21)]

#now we will check the structure of both the DF

str(german_credit_chr)

str(german_credit_con)

#so character DF has 19 variables and countinuous DF has 2 variables
#we will now convert the variable of character DF to character

german_credit_chr$Installment.rate.in.percentage.of.disposable.income <- as.character(german_credit_chr$Installment.rate.in.percentage.of.disposable.income)

german_credit_chr$Present.residence.since <- as.character(german_credit_chr$Present.residence.since)

german_credit_chr$Number.of.existing.credits.at.this.bank. <- as.character(german_credit_chr$Number.of.existing.credits.at.this.bank.)

german_credit_chr$Number.of.people.being.liable.to.provide.maintenance.for. <- as.character(german_credit_chr$Number.of.people.being.liable.to.provide.maintenance.for.)

#Now after converting all the variable in german_credit_chr we will check its structure

str(german_credit_chr)

#since now all the variables are in character format we will go ahead and convert the character to factor
#will use my df.factor funtion as below

df.factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    }
    return(x)
  }

#now we will use the above function and store the value in german_credit_chr

german_credit_chr <- df.factor(german_credit_chr)

#now we will check the str of the df german_credit_chr

str(german_credit_chr)

#now we are ready to create dummy variables out of these factor variables
#we will use my df.matrix function as below to create dummy variables

df.matrix <-
  function(x,name="dummy")
    for (i in 1:ncol(x)) {
      if (class(x[, i]) == "factor") {
        y <- NA
        y <- data.frame(rep(0, nrow(x)))
        for (j in 1:(nlevels(x[, i])-1)) {
          y[, j] <- rep(0, nrow(x))
          colnames(y)[j] <-
            paste(colnames(x)[i], levels(x[, i])[j], sep = " : ")
          y[which(x[, i] == levels(x[, i])[j]), j] <- 1
        }
        assign(paste(name, colnames(x[i]), sep = "_"),
               y,
               environment(df.matrix))
      }
    }

#now we will use the above function to create dummy variabs for all the factor varaibles 

df.matrix(german_credit_chr)

#now we will cbind all the dummy variables with the countinuous variables and store it in final df

Final_German_Credit <- cbind(german_credit_con,dummy_Age.in.Years,dummy_Credit.history,dummy_Duration.in.month,dummy_foreign.worker,dummy_Housing.,dummy_Installment.rate.in.percentage.of.disposable.income,dummy_Job_status,dummy_Number.of.existing.credits.at.this.bank.,dummy_Number.of.people.being.liable.to.provide.maintenance.for.,dummy_Other.debtors...guarantors,dummy_Other.installment.plans,dummy_Personal.status.and.sex,dummy_Present.employment.since.,dummy_Present.residence.since,dummy_Property,dummy_Purpose,dummy_Savings.account.bonds,dummy_Status.of.existing.checking.account,dummy_Telephone.)

#now we will check the structure of our final DF

str(Final_German_Credit)

#so now we have final DF with 1000 obs and 57 variables
#we also need to scale the value of our countinous variable credit amount

Final_German_Credit$Credit.amount <- scale(Final_German_Credit$Credit.amount)

#now we will again check the value of our final DF

str(Final_German_Credit)

#we see that all our variables are in numeric format, so our data is good for model building


#now our data is prepared for creating training and testing sets
#so we need to divite our final data into 70:30 ration, will use sample split

split_indices <- sample.split(Final_German_Credit$Default_status,SplitRatio = 0.70)

#lets see the value of split_indices

split_indices

# There are 70% TRUE and 30% FALSE values in this vector. We can now assign the TRUE indices as the train data and FALSE indices as the test data.

train <- Final_German_Credit[split_indices == TRUE,]

#now lets check the structure of the train ds

str(train)

#so the train data set have 700 obs with 57 variables
#now lets create test data sets

test <- Final_German_Credit[split_indices == FALSE,]

#so our test data sets is of 300 obs and 57 variables

# now we will Create the first logistic regression model using glm(). As usual, the first argument is the formula Default_status~. where the dot indicates that we want to include all
# the variables as predictors. The second argument is family = binomial to indicate that we want a logistic regression model. The third and the last one is data =
# train. Store the results in 'model_1'.

model_1 <- glm(Default_status~.,family = binomial, data = train)

#now lets look at the summary of the model

summary(model_1)

#now we will use

# The stepwise selection approach is an automated way of identifying the important variables. The algorithm compares a large number of possible models by adding or
# subtracting each variable and comparing one of the evaluative metrics like R-square, AIC, BIC (Bayesian Information Criterion) etc. The stepAIC() function compares
# the AIC values (Akaike Information Criterion) of the models and chooses the combination of variables with the least AIC.

# Apply stepwise variable selection using the stepAIC() function. The first argument is the name of the model and the second is direction = "both" to specify that we
# want to use a bidirectional selection approach (forward and backward). Store the results in a variable 'step'.

step <- stepAIC(model_1,direction = "both")

# we will Type 'step' to look at the model suggested by stepAIC().

step

#now we will use the model suggetsde by step in model_2

model_2 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` + `Credit.history : A32` + `Credit.history : A33` + 
                 `Duration.in.month : Medium_Term` + `foreign.worker : A201` + 
                 `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Number.of.existing.credits.at.this.bank. : 1` + `Other.debtors...guarantors : A101` + 
                 `Other.debtors...guarantors : A102` + `Other.installment.plans : A141` + 
                 `Personal.status.and.sex : A93` + `Present.employment.since. : A72` + 
                 `Present.residence.since : 1` + `Present.residence.since : 2` + 
                 `Property : A121` + `Property : A122` + `Property : A123` + 
                 `Purpose : A40` + `Purpose : A41` + `Purpose : A46` + `Purpose : A48` + 
                 `Savings.account.bonds : A61` + `Savings.account.bonds : A62` + 
                 `Savings.account.bonds : A63` + `Status.of.existing.checking.account : A11` + 
                 `Status.of.existing.checking.account : A12`, family = binomial, 
               data = train)

#now we will check the summary of model_2

summary(model_2)

# We need to check whether any of the variables are collinear with some other ones. If so, we will remove one of them (whichever is relatively less significant).
#Use vif(model_2) to identify the correlated variables.

vif(model_2)

#we see that `Credit.history : A32` has a VIF of greater than 2, so we will remove it since it is low significant as per p value
#we will create a new model 3

model_3 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` + `Credit.history : A33` + 
                 `Duration.in.month : Medium_Term` + `foreign.worker : A201` + 
                 `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Number.of.existing.credits.at.this.bank. : 1` + `Other.debtors...guarantors : A101` + 
                 `Other.debtors...guarantors : A102` + `Other.installment.plans : A141` + 
                 `Personal.status.and.sex : A93` + `Present.employment.since. : A72` + 
                 `Present.residence.since : 1` + `Present.residence.since : 2` + 
                 `Property : A121` + `Property : A122` + `Property : A123` + 
                 `Purpose : A40` + `Purpose : A41` + `Purpose : A46` + `Purpose : A48` + 
                 `Savings.account.bonds : A61` + `Savings.account.bonds : A62` + 
                 `Savings.account.bonds : A63` + `Status.of.existing.checking.account : A11` + 
                 `Status.of.existing.checking.account : A12`, family = binomial, 
               data = train)

#now we will again check the summary of model 3

summary(model_3)

#we will again check for VIF

vif(model_3)

#we see that `Other.debtors...guarantors : A101` has vif greater than 2 and is not significant enough so we will remove it.


model_4 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` + `Credit.history : A33` + 
                 `Duration.in.month : Medium_Term` + `foreign.worker : A201` + 
                 `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Number.of.existing.credits.at.this.bank. : 1` +  
                 `Other.debtors...guarantors : A102` + `Other.installment.plans : A141` + 
                 `Personal.status.and.sex : A93` + `Present.employment.since. : A72` + 
                 `Present.residence.since : 1` + `Present.residence.since : 2` + 
                 `Property : A121` + `Property : A122` + `Property : A123` + 
                 `Purpose : A40` + `Purpose : A41` + `Purpose : A46` + `Purpose : A48` + 
                 `Savings.account.bonds : A61` + `Savings.account.bonds : A62` + 
                 `Savings.account.bonds : A63` + `Status.of.existing.checking.account : A11` + 
                 `Status.of.existing.checking.account : A12`, family = binomial, 
               data = train)

#now we will again check the summary of model 4

summary(model_4)

#we will again check for VIF

vif(model_4)

#we see that `Property : A121` has vif greater than 2 and is not significant enough so we will remove it.

model_5 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` + `Credit.history : A33` + 
                 `Duration.in.month : Medium_Term` + `foreign.worker : A201` + 
                 `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Number.of.existing.credits.at.this.bank. : 1` +  
                 `Other.debtors...guarantors : A102` + `Other.installment.plans : A141` + 
                 `Personal.status.and.sex : A93` + `Present.employment.since. : A72` + 
                 `Present.residence.since : 1` + `Present.residence.since : 2` + 
                 `Property : A122` + `Property : A123` + 
                 `Purpose : A40` + `Purpose : A41` + `Purpose : A46` + `Purpose : A48` + 
                 `Savings.account.bonds : A61` + `Savings.account.bonds : A62` + 
                 `Savings.account.bonds : A63` + `Status.of.existing.checking.account : A11` + 
                 `Status.of.existing.checking.account : A12`, family = binomial, 
               data = train)


#now we will again check the summary of model 5

summary(model_5)

#we will again check for VIF

vif(model_5)

#we now don't see any variable having VIF greater than 2 so we are good with the VIF, but we have to reduce the number of
#variable as per significant, so we will again use stepaic to check whether there is another good model or not in term of aic

step_1 <- stepAIC(model_5,direction = "both")

#now we will check the step_1 model suggeted by aic

step_1

#now we will store the value of step-1 call in model 6

model_6 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` + `Duration.in.month : Medium_Term` + 
                 `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Other.installment.plans : A141` + `Personal.status.and.sex : A93` + 
                 `Present.employment.since. : A72` + `Present.residence.since : 1` + 
                 `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                 `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                 `Savings.account.bonds : A62` + `Savings.account.bonds : A63` + 
                 `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
               family = binomial, data = train)

#now we will again check the summary of model 6

summary(model_6)

#we will again check for VIF

vif(model_5)

#we will now go ahead and remove those variable which are not significant based on P-value
#we see that `Present.residence.since : 1` is of very low significant so we will remove it

model_7 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` + `Duration.in.month : Medium_Term` + 
                 `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Other.installment.plans : A141` + `Personal.status.and.sex : A93` + 
                 `Present.employment.since. : A72` +  
                 `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                 `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                 `Savings.account.bonds : A62` + `Savings.account.bonds : A63` + 
                 `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
               family = binomial, data = train)

#now we will again check the summary of model 7

summary(model_7)

#we also see that after removing `Present.residence.since : 1` the AIC doesnot increase significantly

#we see that `Duration.in.month : Medium_Term` is of very low significant so we will remove it

model_8 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` +  
                 `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Other.installment.plans : A141` + `Personal.status.and.sex : A93` + 
                 `Present.employment.since. : A72` +  
                 `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                 `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                 `Savings.account.bonds : A62` + `Savings.account.bonds : A63` + 
                 `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
               family = binomial, data = train)

#now we will again check the summary of model 8

summary(model_8)

#we also see that after removing `Duration.in.month : Medium_Term` the AIC doesnot increase significantly

#we see that `Other.installment.plans : A141` is of very low significant so we will remove it

model_9 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                 `Credit.history : A31` +  
                 `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                 `Personal.status.and.sex : A93` + 
                 `Present.employment.since. : A72` +  
                 `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                 `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                 `Savings.account.bonds : A62` + `Savings.account.bonds : A63` + 
                 `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
               family = binomial, data = train)

#now we will again check the summary of model 9

summary(model_9)

#we also see that after removing `Other.installment.plans : A141` the AIC doesnot increase significantly

#we see that `Purpose : A48` is of very low significant so we will remove it

model_10 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                  `Credit.history : A31` +  
                  `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Personal.status.and.sex : A93` + 
                  `Present.employment.since. : A72` +  
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + 
                  `Savings.account.bonds : A62` + `Savings.account.bonds : A63` + 
                  `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
                family = binomial, data = train)

#now we will again check the summary of model 10

summary(model_10)

#we also see that after removing `Purpose : A48` the AIC has increase significantly, so we will not remove `Purpose : A48`,
#but we will `Savings.account.bonds : A63` which also has low significant in model_9

model_11 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                  `Credit.history : A31` +  
                  `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Personal.status.and.sex : A93` + 
                  `Present.employment.since. : A72` +  
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                  `Savings.account.bonds : A62` +  
                  `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
                family = binomial, data = train)

#now we will again check the summary of model 11

summary(model_11)

#we also see that after removing `Savings.account.bonds : A63` the AIC doesnot increase significantly

#we see that `Savings.account.bonds : A62` is of very low significant so we will remove it

model_12 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                  `Credit.history : A31` +  
                  `foreign.worker : A201` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Personal.status.and.sex : A93` + 
                  `Present.employment.since. : A72` +  
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                  `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
                family = binomial, data = train)

#now we will again check the summary of model 12

summary(model_12)

#we also see that after removing `Savings.account.bonds : A62` the AIC doesnot increase significantly

#we see that `foreign.worker : A201` is of very low significant so we will remove it

model_13 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                  `Credit.history : A31` +  
                  `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Personal.status.and.sex : A93` + 
                  `Present.employment.since. : A72` +  
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Purpose : A48` + `Savings.account.bonds : A61` + 
                  `Status.of.existing.checking.account : A11` + `Status.of.existing.checking.account : A12`, 
                family = binomial, data = train)

#now we will again check the summary of model 13

summary(model_13)

#we also see that after removing `foreign.worker : A201` the AIC doesnot increase significantly

#now since we are done with rempving all the insignificant value, lets again check the best model by using stepaic function

step_2 <- stepAIC(model_13,direction = "both")

# now we will check the model suggested by step aic is

step_2

#we take that the model suggested by stepaic as model 14

model_14 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                     `Credit.history : A31` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                     `Personal.status.and.sex : A93` + `Present.employment.since. : A72` + 
                     `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                     `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                     `Status.of.existing.checking.account : A12`, family = binomial, 
                   data = train)

#now we will again check the summary of model 14

summary(model_14)

#we still see that there are still some insignificant variable oresent in our model
##we see that `Personal.status.and.sex : A93` is of very low significant so we will remove it

model_15 <- glm(formula = Default_status ~ Credit.amount + `Credit.history : A30` + 
                  `Credit.history : A31` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Present.employment.since. : A72` + 
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 15

summary(model_15)

##we also see that after removing `Personal.status.and.sex : A93` the AIC doesnot increase significantly
##we see that `Credit.history : A30` is of very low significant so we will remove it

model_16 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Housing. : A151` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Present.employment.since. : A72` + 
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 16

summary(model_16)

##we also see that after removing `Credit.history : A30` the AIC doesnot increase significantly
##we see that `Housing. : A151` is of very low significant so we will remove it

model_17 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Present.employment.since. : A72` + 
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 17

summary(model_17)

##we also see that after removing `Housing. : A151` the AIC doesnot increase significantly
##we see that `Present.employment.since. : A72` is of very low significant so we will remove it

model_18 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Present.residence.since : 2` + `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 18

summary(model_18)

##we also see that after removing `Present.employment.since. : A72` the AIC doesnot increase significantly
##we see that `Present.residence.since : 2` is of very low significant so we will remove it

model_19 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Purpose : A40` + `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 19

summary(model_19)

##we also see that after removing `Present.residence.since : 2` the AIC doesnot increase significantly
##we see that `Purpose : A40`  is of very low significant so we will remove it

model_20 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Purpose : A41` + 
                  `Purpose : A46` + `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 20

summary(model_20)

##we also see that after removing `Purpose : A40` the AIC doesnot increase significantly
##we see that `Purpose : A46`  is of very low significant so we will remove it

model_21 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Purpose : A41` + 
                  `Savings.account.bonds : A61` + `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 21

summary(model_21)

##we also see that after removing `Purpose : A46` the AIC doesnot increase significantly
##we see that `Savings.account.bonds : A61`  is of very low significant so we will remove it

model_22 <- glm(formula = Default_status ~ Credit.amount +  
                  `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                  `Purpose : A41` + 
                  `Status.of.existing.checking.account : A11` + 
                  `Status.of.existing.checking.account : A12`, family = binomial, 
                data = train)

#now we will again check the summary of model 22

summary(model_22)

##we also see that after removing `Savings.account.bonds : A61` the AIC doesnot increase significantly
##Now we see that all our variables in he model are having either 2 or 3 stars, so we will stop here

#So our final model will be 

model_final <- glm(formula = Default_status ~ Credit.amount +  
                     `Credit.history : A31` + `Installment.rate.in.percentage.of.disposable.income : 1` + 
                     `Purpose : A41` + 
                     `Status.of.existing.checking.account : A11` + 
                     `Status.of.existing.checking.account : A12`, family = binomial, 
                   data = train)

#now we will again check the summary of model final

summary(model_final)


##-----------------------------------------------------Checkpoint 5: Model Evaluation--------------------------------------------------------------------------------
#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.
#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

train$predicted_prob <- predict(model_final,type = "response")

#now we will use the rcorr.cens function to calculate c-statistic

rcorr.cens(train$predicted_prob,train$Default_status)

#since we see that the C-statistic is greater than 70% so our model is good on train data, but to check the actual performance we 
#will test out model on test data as well

test$predicted_prob <- predict(model_final,newdata = test[,-2],type = "response")

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(test$predicted_prob,test$Default_status)

#we see that the c-statistic for train data is 7.78 and test data is 8.07, which is close to 1, we can safely say that our model
#good discriminative power.

#now we will evaluate our model using KS-statistic, we will prediction() function from package (ROCR)
#first we will store the prediction object in model_score

model_score <- prediction(train$predicted_prob,train$Default_status)

#now we will create performance object using performance function

model_perf <- performance(model_score,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table <- attr(model_perf,"y.values")[[1]] - (attr(model_perf,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks <- max(ks_table)

ks

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_index <- which(ks_table == ks)

#now since the row is 308, we will divide it by total number of rows in trainind ds which is 700

ks_decile <- ks_index/nrow(train)

ks_decile

#so our ks-statistic is 0.37 which is good, in 3rd decile in train data set.

#first we will store the prediction object in model_score_test

model_score_test <- prediction(test$predicted_prob,test$Default_status)

#now we will create performance object using performance function

model_perf_test <- performance(model_score_test,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks_test <- max(ks_table_test)

ks_test

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_test_index <- which(ks_table_test == ks_test)

#now since the row is 109, we will divide it by total number of rows in trainind ds which is 300

ks_test_decile <- ks_test_index/nrow(test)

ks_test_decile

#since the ks-statistic is at 0.48, therefore our ks-ststistic lies in the 5th decile

##---------------------------------------------------Checkpoint 6: Threshold value-----------------------------------------------------------------------------------

# In the next few steps, we will predict the probabilities of Default_status for test data, use the predictions to make an ROC curve of the model and find the area under
# the curve.
# 
# We have a decent model now, but that doesn't mean that it discriminates well between people with high and low default_rate. Besides, all we have now is
# probabilities of a person being under default, but at which probability do we say 'default = 1'? Is the threshold 50%, 60%, 70%?
# 
# We need to set a threshold probability which gives us the highest true positive rate (i.e. identify truly risky ones accurately) and, at the same time, the least
# false positive rate (i.e. the least probability of false alarm).
# ROC Curves are used to see how well the model can separate positive and negative examples and to identify the best threshold for separating them.

#To create an ROC Curve, the general procedure is - 1) Predict probabilities of test data, 2) Create a 'predictions' object using the predicted probabilities and the
#test labels and 3) evaluate the performance of predictions using a 'performance' object

# First, we will use the predict() function to predict the probabilities of 'default = 1' for the test points. Use the command predict(model_final, newdata = test[, -2], type =
# "response"). The second argument is the test data without the class labels; type = "response" returns the predicted probabilities. Store the prediction results in the
# variable 'predictions_data'.

prediction_data <- predict(model_final,newdata = test[,-2],type = "response")

#Type predictions_data to have a look at the predicted probabilities.

prediction_data

test$Default_status

# Create a prediction object named 'pred_object' using prediction(predictions_data, test$Default_status). The function will evaluate the test data labels using the
# probabilities stored in predictions_data.

pred_object <- prediction(prediction_data,test$Default_status)

# Now we are at the third step of creating the ROC curve - creating a 'performance' object. All types of predictor evaluations are performed using the performance()
# function. The performance() function takes in three arguments - the prediction object (pred_object) and two performance measures to evaluate the model.

# For ROC curves, we use the True Positive Rate and the False Positive Rate as the two evaluation metrics. Create the performance object using performance(). The first
# argument is the prediction object created above; measure = "tpr" and x.measure = "fpr" are the second and third arguments respectively. Store the results in the
# variable performance_measures.

performance_measures <- performance(pred_object,measure = "tpr",x.measure = "fpr")

#Plotting the ROC curve is easy now. Type plot(performance_measures).

plot(performance_measures)

# The curve lies ahead of the y=x line, which is a good news. That's the first sanity check we must perform. Next, we need to know the area under the curve. Higher area
# (like 0.90) indicates that the TPR is high and the FPR low for some probability thresholds.

# The performance() function also calculates the area under the curve by simply mentioning "auc" as the evaluation measure. In the performance() command above, replace
# the measure with "auc" and remove the x.measure. Store the result in a variable named 'auc'.

auc <- performance(pred_object,measure = "auc")

# Type auc <- auc@y.values[[1]] to store the auc value in the variable.

auc <- auc@y.values[[1]]

# Type auc to see the value of the area under the ROC curve.

auc

# The auc value lying above 0.70 is a good sign. As a lower bound, it should be more than 0.50 since 0.50 is the area under the line y=x (or TPR = FPR). Equal values of
# TPR and FPR may as well be acheived by a random model (by guessing). An upper bound is obviously 1 which corresponds to the area of the entire area. This is the
# (unrealistic) case when the True Positive Rate is skyrocketing while the FPR is always 0.

#since predicting the bad customer good is bad for the bank, we will set our threshold to .40 so as to accuratelt predict the bad customer


#now we will construct confusion matrix with predicted propability .33

confusionMatrix(as.numeric(train$predicted_prob > 0.33),train$Default_status)

#so we have 333 true positive and 141 true negative

#with Accuracy : 0.6771, Sensitivity : 0.6796, Specificity : 0.6714

#now we will check for test data set

confusionMatrix(as.numeric(test$predicted_prob > 0.33),test$Default_status)

#so we have 141 true positive and 69 true negative

#with Accuracy : 0.7, Sensitivity : 0.6714, Specificity : 0.7667

#so our model is doing a good job of classifying a bad customer with 70 accuracy on test data set.


##-------------------------------------------------End-------------------------------------------------------------------------------------------------------------------------------

