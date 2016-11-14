#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Telecom Churn Case Study--------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

# Set Working Directory

setwd("E:\\UpGrad IIIT PGDDA Program\\Course 3\\Graded Assignment\\Predictive Analytics 1")

# Install and Load the required packages

library(caret)
library(car)
library(caTools)
library(ROCR)
library(MASS)
library(stats)
library(GGally)
library(ggplot2)
library(Hmisc)
library(e1071)
library(class)

# Load the given files.

customer <- read.csv("customer_data.csv",stringsAsFactors = FALSE)

internet <- read.csv("internet_data.csv",stringsAsFactors = FALSE)

churn_data <- read.csv("churn_data.csv",stringsAsFactors = FALSE)

#we now view the file 

View(customer)

View(internet)

View(churn_data)

#now that all the files have one common variable customer id and 7043 observation, we now have to merge all three files into one file for our analysis
#but before that we will check for any duplicate variables in customer id

#first we will standardize the format of customer id

customer$customerID <- trimws(tolower(customer$customerID))

internet$customerID <- trimws(tolower(internet$customerID))

churn_data$customerID <- trimws(tolower(churn_data$customerID))

#now we will check for any duplicate in the customer id

which(duplicated(customer$customerID))

which(duplicated(internet$customerID))

which(duplicated(churn_data$customerID))

#so, since we don't have any duplicate value in our customer id variable we are good to merge the file

merge_1 <- merge(customer,internet,by = "customerID",all = FALSE)

#so we have 7043 obs and 13 variable after first merge, since customer id of any one obs is retain

final_merge <- merge(merge_1,churn_data,by = "customerID",all = FALSE )

#Now our final master file have 7043 obs and 21 variable, now we will check for any NA's value in our data sets

sum(is.na(final_merge))

#so we have 11 na values in our data sets, so now we will use sappy to see which rows has the na values

sapply(final_merge, function(x) sum(is.na(x)))

#since we see that ony 11 values are na which account for 0.15% of the total sample we will remove those values from our observation

final_merge_1 <- final_merge[-which(is.na(final_merge$TotalCharges)),]

#now we will check for the na values to confirm if we have successfully removed those observation

sum(is.na(final_merge_1))

#now we will check for the summary of our data sets

summary(final_merge_1)

#we will also check for the structure of the data sets

str(final_merge_1)

#now we will convert all the character variable to factor using my below function, which can also be downloaded from github

df.factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    }
    return(x)
  }

churn_1 <- df.factor(final_merge_1)

#now we will check for the structure of our data sets

str(churn_1)

#we see that all the character variables are converted to factor, now we will perform univariant analysis

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Checkpoint 2: Exploratory Data Analysis-----------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

###Univariate Analysis###

# Summary Statistics
# Distribution plot
# Outlier treatment

summary(churn_1)

#we check for outliers in the tenure variable using density plot

tenure <- ggplot(churn_1,aes(x=churn_1$tenure)) + geom_histogram(col = "red",fill = "#3366FF") + ggtitle("Distribution of Tenure") + xlab("Tenure") + ylab("Frequency")

tenure

#we see that there is high number of people who are using the services for more than a year

#we will also check the outliers using boxplot

boxplot(churn_1$tenure)

#we see that there is no outliers present in the tenure, we will move to next variable which is monthly charges

monthly_charges <- ggplot(churn_1,aes(x=churn_1$MonthlyCharges)) + geom_density(col = "pink",fill = "grey") + ggtitle("Distribution of Monthly Charges") + xlab("Monthly Charges") + ylab("Frequency")

monthly_charges

#From the above plot we see that there is large number of concentation on monthly charges less than 25 and between 65 to 100

#we will now use boxplot to check for outliers

boxplot(churn_1$MonthlyCharges)

#so we don't see any outliers in our variable, so we will move to the next variable which is total charges

total_charges <- ggplot(churn_1,aes(x=churn_1$TotalCharges)) + geom_density(col = "red",fill = "yellow") + ggtitle("Distribution of Total Charges") + xlab("Total Charges") + ylab("Frequency")

total_charges

#we see that high number of concentration is on charges below 2500

#now we will check for outliers

boxplot(churn_1$TotalCharges)

#so we don't have any outliers in any one of our continuous variable, so we will move towards categorical variables

#we will now see that distribution of gender

gender <- ggplot(churn_1,aes(x=churn_1$gender,fill = churn_1$gender)) + geom_bar() + ggtitle("Distribution of Gender") + xlab("") + ylab("Count") + labs(fill = "Gender")

gender

#we see that there is more of a equal distribution of male and female, next will check for SeniorCitizen

SeniorCitizen <- ggplot(churn_1,aes(x=churn_1$SeniorCitizen,fill = factor(churn_1$SeniorCitizen))) + geom_bar() + ggtitle("Distribution of Senior Citizen") + xlab("") + ylab("Count") + labs(fill = "Senior Citizen") 

SeniorCitizen

#we see that about 16% of the total are SeniorCitizen, will now move towards internet service

internet_service <- ggplot(churn_1,aes(x=churn_1$InternetService,fill = churn_1$InternetService)) + geom_bar() + ggtitle("Distribution of Internet Service Connection Type") + xlab("")  + ylab("Count") + labs(fill = "Internet Service")

internet_service

#we see that about 1500 customers don't use internet service, next will check phone service

phone_service <- ggplot(churn_1,aes(x=churn_1$PhoneService,fill = churn_1$PhoneService)) + geom_bar() + ggtitle("Distribution of Phone Service") + xlab("") + ylab("Count") + labs(fill = "Phone Service")

phone_service

#we see that about 700 customers don't use phone service, will now check for contract

contract <- ggplot(churn_1,aes(x=churn_1$Contract,fill= churn_1$Contract)) + geom_bar() + ggtitle("Service Contact") + xlab("") + labs(fill = "Contract")

contract

#we see that most customer are on month to month contract, will now look for churn

churn_1_plot <- ggplot(churn_1,aes(x=churn_1$Churn,fill = churn_1$Churn)) + geom_bar() +ggtitle("No of Customers who Churn") + xlab("") + labs(fill = "Churn")

churn_1_plot

#we see that only about 1900 customers are churn. Now we will move towards Multivariate analysis.

###Multivariate analysis###

# Correlation table

str(churn_1)

#Lets subset the continous variables so that we can calculate correlation of all the pairs at the same time

con_var <- churn_1[,c(3,14,19,20)]

#now we check if all the variable are continuous 

str(con_var)

#now we have all the continuous variable in one data set we will plot correlation matrix using ggpair

# ggpairs(con_var)

#we see that tenure and monthly charges are correlated with total charges, which means if tenure increases total charges also increases and when monthly
#charges increases total charges also increases, we will also confirm this by using correlation matrix

cor(con_var)

#we see that correlation of tenure and totalcharges is 0.82 and monthly charges is 0.65 and both are positivilt correlated

#will not plot the graph for tenure and churn

tenure_churn_1 <- ggplot(churn_1,aes(x=churn_1$tenure,fill = churn_1$Churn)) + geom_bar(position = "dodge") + ggtitle("Distribution of Tenure over Churn") + xlab("Tenure") + labs(fill = "Churn")

tenure_churn_1

#we see that if the tenure is increases the churn is reduces, so we can conclude that tenure is negativelt corelated with churn

monthly_churn_1 <- ggplot(churn_1,aes(x=churn_1$MonthlyCharges,fill = churn_1$Churn)) + geom_histogram(position = "dodge") + ggtitle("Distribution of Monthly Charges over Churn") + xlab("Monthly Charges") + labs(fill = "Churn")

monthly_churn_1

#we see that highest frequency of customers who churn are between 65 to 105, next we will see gender

gender_churn_1 <- ggplot(churn_1,aes(x=churn_1$gender,fill = churn_1$Churn)) + geom_bar(position = "dodge") + ggtitle("Distribution of Gendor Over Churn") + xlab("Gender") + labs(fill = "Churn")

gender_churn_1

#we see that about 25% of both male and female tends to Churn, next we will see for internet service

internet_service_churn_1 <- ggplot(churn_1,aes(x=churn_1$InternetService,fill = churn_1$Churn)) + geom_bar(position = "dodge") + ggtitle("Distribution of Internet Service over Churn") + xlab("Internet Service") + labs(fill = "Churn")

internet_service_churn_1

#we see that most customers who churn are on fiber optic, next we will look for PaymentMethod

PaymentMethod_churn_1 <- ggplot(churn_1,aes(x=churn_1$PaymentMethod,fill = churn_1$Churn)) + geom_bar(position = "dodge") + ggtitle("Distribution of PaymentMethod over Churn") + xlab("Payment Method") + labs(fill = "Churn")

PaymentMethod_churn_1

#we see that most customers who churn are using payment method electronic check, next we will look for Dependents

Dependents_churn_1 <- ggplot(churn_1,aes(x=churn_1$Dependents,fill = churn_1$Churn)) + geom_bar(position = "fill") + ggtitle("Distribution of Dependents over Churn") + xlab("Dependents") + labs(fill = "Churn")

Dependents_churn_1

#we see an increase in customers who churn and don't have any Dependents about 25%. Now we will move on to data modelling

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Checkpoint 3: Data Modelling----------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#we will first remove the unique key customr ID from our data sets

churn_2 <- churn_1[,-1]

#now we will bin the variable tenure into three equal parts
#we first need to find out the maximum range of the variable tenure

ten_max <- max(churn_2$tenure)

#Since we need to create three equal bin we will divide max by 3

bin <- ten_max/3

#so our each bin will have 24 width, now we will create bin "Short","Medium","Long"

churn_2$tenure <-
  ifelse(
    churn_2$tenure <= 24,
    "Short",
    ifelse(
      churn_2$tenure >= 25 &
        churn_2$tenure <= 48,
      "Medium",
      ifelse(churn_2$tenure >= 49, "Long", "NA")
    )
  )

#now to check whether we have correctly group the variable we will first convert the variable to factor

churn_2$tenure <- factor(churn_2$tenure,levels = c("Short","Medium","Long"))

#now we will check the levels of the variable tenure

levels(churn_2$tenure)

#now we will plot to see whether the binning is correct on total charges

plot_1 <- ggplot(churn_2,aes(x=1,y=churn_2$TotalCharges,fill = churn_2$tenure)) + geom_boxplot() + ggtitle("Distribution of Total Charges on Tenure") + xlab("") + ylab("Total Charges") + labs(fill = "Tenure")

plot_1

#will now check for structure of our data set

str(churn_2)

#now we will create dummy variable using my below function

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

#now we will create dummy variable for all the factor variable using my above function

df.matrix(churn_2)

str(churn_2)

#now we will create our new data as churn_2 using cbind all the dummy variable with the numeric variable

churn_3 <- cbind(churn_2[,c(2,18,19,20)],dummy_Contract,dummy_Dependents,dummy_DeviceProtection,dummy_gender,dummy_InternetService,dummy_MultipleLines,dummy_OnlineBackup,dummy_OnlineSecurity,dummy_PaperlessBilling,dummy_Partner,dummy_PaymentMethod,dummy_PhoneService,dummy_StreamingMovies,dummy_StreamingTV,dummy_TechSupport,dummy_tenure)

#now we will check the structure of our churn_3 data sets

str(churn_3)

#we see that all our variables are numeric type which is reqired for machine learning, we also need to standardized our model data

churn_4 <- data.frame(sapply(churn_3[,c(2,3)], function(x) scale(x)))

#now we will check the structure of final mode

str(churn_4)

churn <- cbind(churn_3[,-c(2,3)],churn_4)

#now since our data has been standardized we can mow begin with model building

str(churn)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Checkpoint 4: Model Building----------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#first we will split the data into training and testing data sets

s = sample.split(churn$Churn,SplitRatio = 0.70)

# There are 70% TRUE and 30% FALSE values in this vector. We can now assign the TRUE indices as the train data and FALSE indices as the test data.

churn_train <- churn[s == TRUE,]

#and the testing data should contain 30% of the data

churn_test <- churn[s == FALSE,]

str(churn_train)

#we first need to assign the True class labels of training data

cl <- churn_train[,2]

#now we need to create the training and test data without the true class level

train <- churn_train[,-2]

test <- churn_test[,-2]

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model - K-NN--------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#1. Model - K-NN:

#Now we can train knn and obtain its class probabilities from the "prob" attribute.

#now we will use KNN with 1NN

Model_KNN_1 <- knn(train,test,cl,k=1,prob=TRUE)

model_1 <- train(Churn~.,data=churn_train,method = "knn",tuneGrid = expand.grid(.k=1:50),metric = "Accuracy",trControl=trainControl(method = 'repeatedcv',number=10,repeats=15))

table(Model_KNN_1,churn_test[,2])

plot(model_1)

confusionMatrix(Model_KNN_1,churn_test[,2],positive = "No")

summary(Model_KNN_1)

#we see that by using 1 NN we are getting a accuracy of 72, Sensitivity of 80% and Specificity of 50%

#now we will use KNN with 3NN

Model_KNN_2 <- knn(train,test,cl,k=3,prob=TRUE)

table(Model_KNN_2,churn_test[,2])

confusionMatrix(Model_KNN_2,churn_test[,2],positive = "No")

summary(Model_KNN_2)

#we see that by using 3 NN we are getting a accuracy of 74, Sensitivity of 83% and Specificity of 50%

#now we will use KNN with 7NN

Model_KNN_3 <- knn(train,test,cl,k=7,prob=TRUE)

table(Model_KNN_3,churn_test[,2])

confusionMatrix(Model_KNN_3,churn_test[,2],positive = "No")

summary(Model_KNN_3)

#we see that by using 7 NN we are getting a accuracy of 77, Sensitivity of 85% and Specificity of 55%

#now we will use KNN with 15NN

Model_KNN_4 <- knn(train,test,cl,k=15,prob=TRUE)

table(Model_KNN_4,churn_test[,2])

confusionMatrix(Model_KNN_4,churn_test[,2],positive = "No")

summary(Model_KNN_4)

#we see that by using 15 NN we are getting a accuracy of 78, Sensitivity of 86% and Specificity of 57%

#now we will use KNN with 30NN

Model_KNN_5 <- knn(train,test,cl,k=30,prob=TRUE)

table(Model_KNN_5,churn_test[,2])

confusionMatrix(Model_KNN_5,churn_test[,2],positive = "No")

summary(Model_KNN_5)

#we see that by using 30 NN we are getting a accuracy of 79, Sensitivity of 86% and Specificity of 60%

#now we will use KNN with 50NN

Model_KNN_6 <- knn(train,test,cl,k=47,prob=TRUE)

table(Model_KNN_6,churn_test[,2])

confusionMatrix(Model_KNN_6,churn_test[,2],positive = "No")

summary(Model_KNN_6)

#we see that by using 47 NN we are getting a accuracy of 80, Sensitivity of 87% and Specificity of 61%

#which means with an accuracy of 80% we are predicting the customer will churn with an accuracy of 61%

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model Evaluation K-NN-----------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#we will now use the ROC curve to see that optimal value of K

#We need to set a threshold probability which gives us the highest true positive rate (i.e. identify truly risky ones accurately) and, at the same time, the least
# false positive rate (i.e. the least probability of false alarm).
# ROC Curves are used to see how well the model can separate positive and negative examples and to identify the best threshold for separating them.

#To create an ROC Curve, the general procedure is - 1) Predict probabilities of test data, 2) Create a 'predictions' object using the predicted probabilities and the
#test labels and 3) evaluate the performance of predictions using a 'performance' object

prob <- attr(Model_KNN_6, "prob")

#However,  come on a form that ROCR does not accept so we need to invert them for the -1 class and rescale them.

prob <- 2*ifelse(Model_KNN_6 == "-1", 1-prob, prob) - 1

cl_test <- churn_test[,2]

cl_testing <- ifelse(cl_test == "No",1,-1)

pred_knn <- prediction(prob, cl_testing)

pref_knn <- performance(pred_knn, "tpr", "fpr")

plot(pref_knn,colorize=T, lwd=3, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),main="A ROC curve of KNN Model")

#we see that ROC curve is bending between 40 to 60 so our 47NN is the optimal

# calculating AUC

auc <- performance(pred_knn,"auc")

auc <- auc@y.values[[1]]

auc

#we now see that AUC is 75% which is a good sign!!.

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model Naive Bayes---------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#since we already have create our training and testing data we will proceed with learning naive bayes

# We can now build and evaluate the model on comparable train and test data. The naiveBayes() function is in e1071 package. Load it using library.

# Our target variable is Churn and all the other are features. Thus, Churn~. will be the first argument. The second argument,
# data, is train_data. Run NaiveBayes() and store the results as naive_model. It needs only two arguments.

naive_model <- naiveBayes(Churn~.,data = churn_train)

# NaiveBayes() uses train_data to calculate the probabilities of each observation being Churn / non-Churn.

# Next, we'll use the test data set to predict the class. The predict() function does that. Obviously, while predicting, we do not let predict() know the true class
# labels (Churn). We only pass the dataframe of all the features as an argument.

predict_model <- predict(naive_model,test)

# Type  predict_model to see the prediction results.

x <- as.data.frame(predict_model)

# You'll note that it has 2 main sections - $class and $posterior. class stores the predicted classes of the test points (No /Yes) and posterior stores the posterior
# probability of each test point being No / Yes. Note that the sum of posterior probabilities of each row is 1. They are the predicted probabilities of each test point
# being Churn / non-Churn.

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model Evaluation Naive Bayes----------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

# Evaluation of model means to compare what the model had predicted (class labels) with the actual, true labels in test_data. In other words, we want to compare
# predcit_model$class with churn_test$Churn.

# To compare the prediction with the actual labels, we use the table() function. It summarizes the labels of two columns. Type table(predict_model$class,
# churn_test$Churn) and observe the results.

table(predict_model,churn_test$Churn)

# note The horizontal axis represents the true labels in the test data. Vertical axis shows the predicted values.

# A better way to summarize the results is the confusion matrix.need to load the caret and e1071 packages from library(). Then
# create the confusionMatrix(). The two arguments are same as with table(), since those are the two columns to be compared.

confusionMatrix(predict_model,churn_test$Churn)

# we can see the same table along with accuracy, sensitivity and specificity. The accuracy should be between 64-78 %. an accuracy of 70%, for example, means that 70 %
# of the total test points are classified correctly. It involves both types of labels - Yes and No .

# But in our case, accuracy is not a good metric. The main challenge is to 'correctly predict those who actually Churn'.

# Note that the positive class is 'No (Will not Churn)' which is shown at the bottom of the confusionMatrix. Sensitivity is the true positive rate ans specificity is the
# true negative rate.

# Note that the Sensitivity is quite low, which means that the few 'not churn' customers are correctly spotted by Naive Bayes.

# Compare that to Specificity, which is relatively high. This is the fraction of will churn cases which are correctly identified.

#Considering that, the Naive Bayes model is giving comparable results to other more sophisticated model with this data.

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model - Logistic Regression:----------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#Data Prep:
# Since Logistic regression takes numeric variables as inputs we first need to impute Yes and No with and 1 and 0 respectively.

churn_Log <- churn

churn_Log$Churn <- ifelse(churn_Log$Churn == "Yes",1,0)

str(churn_Log)

# The data is now cleaned up and ready for modelling. The next step is to divide the data into train and test sets. But before that, have a look at the summary() of the
# target variable using factor(churn_Log$Churn)  number of observations in the two classes.

summary(factor(churn_Log$Churn))

# Note that only 1869 out of total 7032, or mere 26.5% people have the risk of churning. If we randomly choose 30% points as test data, it may not contain any person with
# risk = 1 at all. Thus, we'll use the sample.split() function from the caTools package. It ensures an equal proportion of the two classes in the train and test data.
#Since we have alsready created the split ration s before we don't need to create it again we will just use it to create our train and test data sets.

Log_Train <- churn_Log[s == "TRUE",]

Log_Test <- churn_Log[s == "FALSE",]

# Now we are ready to build the logistic model. The glm() function, an abbreviation for genralized linear models, is used for building logistic models. Logistic is a
# generalized linear model because the logit equation transforms the log(odds) to be expressed as a linear combination of the variables (though GLMs have other
# conditions too, this is the basic intuition).

# Create the first logistic regression model using glm(). As usual, the first argument is the formula  Churn~. where the dot indicates that we want to include all
# the variables as predictors. The second argument is family = binomial to indicate that we want a logistic regression model. The third and the last one is data =
# Log_Train. Store the results in 'log_model_1'.

log_model_1 <- glm(Churn~.,family = binomial,data = Log_Train)

# Look at the summary of log_model_1.

summary(log_model_1)

# Let's first note the significant variables - `Contract : Month-to-month`, `Contract : One year` and TotalCharges are the most significant ones indicated by three asterisks. The other (lesser significant) ones are
# `DeviceProtection : No`, `gender : Female` etc. Now we need a systematic way to identify which variables can be safely discarded.

# The stepwise selection approach is an automated way of identifying the important variables. The algorithm compares a large number of possible models by adding or
# subtracting each variable and comparing one of the evaluative metrics like R-square, AIC, BIC (Bayesian Information Criterion) etc. The stepAIC() function compares
# the AIC values (Akaike Information Criterion) of the models and chooses the combination of variables with the least AIC.

# Apply stepwise variable selection using the stepAIC() function. The first argument is the name of the model and the second is direction = "both" to specify that we
# want to use a bidirectional selection approach (forward and backward). Store the results in a variable 'step'.

step <- stepAIC(log_model_1,direction = "both")

# Type 'step' to look at the model suggested by stepAIC().

step

# Build a log_model_2 with the variables suggested by stepAIC().

log_model_2 <- glm(formula = Churn ~ SeniorCitizen + `Contract : Month-to-month` + 
                     `Contract : One year` + `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` + `PaymentMethod : Credit card (automatic)` + 
                     `PaymentMethod : Electronic check` + `StreamingMovies : No` + 
                     `StreamingTV : No` + `TechSupport : No` + `tenure : Short` + 
                     TotalCharges + `OnlineBackup : No`, family = binomial, data = Log_Train)

#now we will check the summary of log_model_2

summary(log_model_2)

# We need to check whether any of the variables are collinear with some other ones. If so, we will remove one of them (whichever is relatively less significant).
#Use vif(model_2) to identify the correlated variables.

vif(log_model_2)

#we see that TotalCharges has the highest VIF followed by `Contract : Month-to-month`,`Contract : One year`,`DeviceProtection : No internet service`,
# and `tenure : Short`. But inspite of having high VIF we cannot simply remove them since they are all highly significant variable.

# So, We start by removing the insnificant variable first which is `OnlineBackup : No`, now we will create a new model without this variavle

log_model_3 <- glm(formula = Churn ~ SeniorCitizen + `Contract : Month-to-month` + 
                     `Contract : One year` + `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` + `PaymentMethod : Credit card (automatic)` + 
                     `PaymentMethod : Electronic check` + `StreamingMovies : No` + 
                     `StreamingTV : No` + `TechSupport : No` + `tenure : Short` + 
                     TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_3

summary(log_model_3)

#we see that by removing `OnlineBackup : No`, the AIC didn't increase much., now we will remove SeniorCitizen

log_model_4 <- glm(formula = Churn ~ `Contract : Month-to-month` + 
                     `Contract : One year` + `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` + `PaymentMethod : Credit card (automatic)` + 
                     `PaymentMethod : Electronic check` + `StreamingMovies : No` + 
                     `StreamingTV : No` + `TechSupport : No` + `tenure : Short` + 
                     TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_4

summary(log_model_4)

#we see that by removing SeniorCitizen, the AIC didn't increase much., now we will remove `PaymentMethod : Credit card (automatic)`

log_model_5 <- glm(formula = Churn ~ `Contract : Month-to-month` + 
                     `Contract : One year` + `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` +  
                     `PaymentMethod : Electronic check` + `StreamingMovies : No` + 
                     `StreamingTV : No` + `TechSupport : No` + `tenure : Short` + 
                     TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_5

summary(log_model_5)

#we see that by removing `PaymentMethod : Credit card (automatic)`, the AIC didn't increase much., now we will remove `StreamingMovies : No`

log_model_6 <- glm(formula = Churn ~ `Contract : Month-to-month` + 
                     `Contract : One year` + `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` +  
                     `PaymentMethod : Electronic check` +  
                     `StreamingTV : No` + `TechSupport : No` + `tenure : Short` + 
                     TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_6

summary(log_model_6)

#after removing `StreamingMovies : No`, we see that all the variables are having 2 and three star, se now we will again check for VIF

vif(log_model_6)

#we see that still we VIF greater than 2 so and all that variable are highly significant, so we will check for coolinear variables
#so first we need to create a DF with all the high VIF variable

High_VIF <- subset(Log_Train,select = c(`Contract : Month-to-month`,`Contract : One year`,`DeviceProtection : No internet service`,`tenure : Short`,TotalCharges))

colnames(High_VIF) <- c("Month_to_month","One_year","No_internet_service","Short","TotalCharges")

#now we will check the correlation on the DF high_vif

cor(High_VIF)

#we see that totalcharges and tenure : Short are negatively correlated with -0.7022006 and Contract : Month-to-month is correlated with Contract : One year with -0.57069505

#so we will the removing both the correlated variable which are less significant than their correlated pair

#without tenure : Short

log_model_7 <- glm(formula = Churn ~ `Contract : Month-to-month` + 
                     `Contract : One year` + `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` +  
                     `PaymentMethod : Electronic check` +  
                     `StreamingTV : No` + `TechSupport : No` + 
                     TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_7

summary(log_model_7)

#after removing tenure : Short, we see that AIC has increased which was expected, will now remove Contract : One year

log_model_8 <- glm(formula = Churn ~ `Contract : Month-to-month` + 
                     `Dependents : No` + `DeviceProtection : No internet service` + 
                     `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                     `PaperlessBilling : No` +  
                     `PaymentMethod : Electronic check` +  
                     `StreamingTV : No` + `TechSupport : No` + 
                     TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_8

summary(log_model_8)

#after removing Contract : One year, we see that AIC has increased which was expected, Now we are left with all the significant variable lets check VIF

vif(log_model_8)

#we see that all the vif are having 2 vif apart from the high sigmificant one, So our final model will be

log_model_final <- glm(formula = Churn ~ `Contract : Month-to-month` + 
                         `Dependents : No` + `DeviceProtection : No internet service` + 
                         `InternetService : DSL` + `MultipleLines : No` + `OnlineSecurity : No` + 
                         `PaperlessBilling : No` +  
                         `PaymentMethod : Electronic check` +  
                         `StreamingTV : No` + `TechSupport : No` + 
                         TotalCharges, family = binomial, data = Log_Train)

#now we will check the summary of log_model_final

summary(log_model_final)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model - Evaluation - Logistic Regression:----------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#Threshold Value

# We have a decent model now, but that doesn't mean that it discriminates well between customer with churn and not-churn. Besides, all we have now is
# probabilities of a customer being under churn, but at which probability do we say 'churn = 1'? Is the threshold 50%, 60%, 70%?
# 
# We need to set a threshold probability which gives us the highest true positive rate (i.e. identify truly risky ones accurately) and, at the same time, the least
# false positive rate (i.e. the least probability of false alarm).
# ROC Curves are used to see how well the model can separate positive and negative examples and to identify the best threshold for separating them.

#To create an ROC Curve, the general procedure is - 1) Predict probabilities of test data, 2) Create a 'predictions' object using the predicted probabilities and the
#test labels and 3) evaluate the performance of predictions using a 'performance' object

# First, we will use the predict() function to predict the probabilities of 'churn = 1' for the test points. Use the command predict(log_model_final, newdata = Log_Test[,-2], type =
# "response"). The second argument is the test data without the class labels; type = "response" returns the predicted probabilities. Store the prediction results in the
# variable 'predictions_data'.

predictions_data <- predict(log_model_final,newdata = Log_Test[,-2],type = "response")

# Create a prediction object named 'pred_object' using prediction(predictions_data, Log_Test$Churn). The function will evaluate the test data labels using the
# probabilities stored in predictions_data.

pred_object <- prediction(predictions_data,Log_Test$Churn)

# Now we are at the third step of creating the ROC curve - creating a 'performance' object. All types of predictor evaluations are performed using the performance()
# function. The performance() function takes in three arguments - the prediction object (pred_object) and two performance measures to evaluate the model.

# For ROC curves, we use the True Positive Rate and the False Positive Rate as the two evaluation metrics. Create the performance object using performance(). The first
# argument is the prediction object created above; measure = "tpr" and x.measure = "fpr" are the second and third arguments respectively. Store the results in the
# variable performance_measures.

performance_measures <- performance(pred_object,measure = "tpr",x.measure = "fpr")

#Plotting the ROC curve is easy now. Type plot(performance_measures).

plot(performance_measures,colorize=T, lwd=3, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),main="A ROC curve of LR Model!")

# The curve lies ahead of the y=x line, which is a good news. That's the first sanity check we must perform. Next, we need to know the area under the curve. Higher area
# (like 0.90) indicates that the TPR is high and the FPR low for some probability thresholds.

# The performance() function also calculates the area under the curve by simply mentioning "auc" as the evaluation measure. In the performance() command above, replace
# the measure with "auc" and remove the x.measure. Store the result in a variable named 'auc'.

auc <- performance(pred_object,measure = "auc")

# Type auc <- auc@y.values[[1]] to store the auc value in the variable.

auc <- auc@y.values[[1]]

# Type auc to see the value of the area under the ROC curve.

auc

# The auc value lying above 0.83 is a good sign. As a lower bound, it should be more than 0.50 since 0.50 is the area under the line y=x (or TPR = FPR). Equal values of
# TPR and FPR may as well be acheived by a random model (by guessing). An upper bound is obviously 1 which corresponds to the area of the entire area. This is the
# (unrealistic) case when the True Positive Rate is skyrocketing while the FPR is always 0.

##C-statistic:

#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.

#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

Log_Train$predicted_prob <- predict(log_model_final,newdata = Log_Train[,-2],type = "response")

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(Log_Train$predicted_prob,Log_Train$Churn)

#we see that the c-statistic for test data is 8.37, which is close to 1, we can safely say that our model has good discriminative power.

##KS-statistic

#now we will evaluate our model using KS-statistic, we will use prediction() function from package (ROCR)
#first we will store the prediction object in model_score_train

model_score_train <- prediction(Log_Train$predicted_prob,Log_Train$Churn)

#now we will create performance object using performance function

model_perf_train <- performance(model_score_train,"tpr","fpr")

#now we will create a vector called ks_table_train of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_train <- attr(model_perf_train,"y.values")[[1]] - (attr(model_perf_train,"x.values")[[1]])

#so the maximum of the ks_table_train is our ks-statistic

ks_train <- max(ks_table_train)

#type ks_train to see that ks-statistic

ks_train

#our ks-statistic is 0.51,now we will see in which decile it lies, so we will find the index for the maximum value first

ks_index_train <- which(ks_table_train == ks_train)

ks_index_train

#now since the row is 1825, we will divide it by total number of rows in test ds which is 4922

ks_decile_train <- ks_index_train/nrow(Log_Train)

ks_decile_train

#so our ks-statistic is at 0.37 which is good, in 4th decile in test data set.

#so we will first add a column in our test data sets as predicted_prob and assign the value of predict function

Log_Test$predicted_prob <- predict(log_model_final,newdata = Log_Test[,-2],type = "response")

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(Log_Test$predicted_prob,Log_Test$Churn)

#we see that the c-statistic for test data is 8.36, which is close to 1, we can safely say that our model has good discriminative power.

##KS-statistic

#now we will evaluate our model using KS-statistic, we will use prediction() function from package (ROCR)
#first we will store the prediction object in model_score

model_score <- prediction(Log_Test$predicted_prob,Log_Test$Churn)

#now we will create performance object using performance function

model_perf <- performance(model_score,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table <- attr(model_perf,"y.values")[[1]] - (attr(model_perf,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks <- max(ks_table)

#type ks to see that ks-statistic

ks

#our ks-statistic is 0.54,now we will see in which decile it lies, so we will find the index for the maximum value first

ks_index <- which(ks_table == ks)

ks_index

#now since the row is 852, we will divide it by total number of rows in test ds which is 2110

ks_decile <- ks_index/nrow(Log_Test)

ks_decile

#so our ks-statistic is at 0.40 which is good, in 4th decile in test data set.

##Confusion Matrix 

#we now need to check accuracy, sensitivity and specificity keeping the probability threshold as 0.3, 0.5 and 0.7 respectively.

#now we will construct confusion matrix with predicted propability .3

confusionMatrix(as.numeric(Log_Test$predicted_prob > 0.30),Log_Test$Churn)

# we can see the table with accuracy, sensitivity and specificity. The accuracy is at 76% which means that 76 %
# of the total test points are classified correctly. It involves both types of labels - Yes and No .

# But in our case, accuracy is not a good metric. The main challenge is to 'correctly predict those who actually Churn'.

# Note that the positive class is '0 (Will not Churn)' which is shown at the bottom of the confusionMatrix. Sensitivity is the true positive rate and specificity is the
# true negative rate.

# Note that the Sensitivity is at 77%, which means that relatively high customer will 'not churn' are correctly spotted by Logistic Regression.

# Compare that to Specificity is at 76%, which is also relatively high. This is the fraction of customer who will churn and are correctly identified.

#we will again check the metrics for .5

confusionMatrix(as.numeric(Log_Test$predicted_prob > 0.50),Log_Test$Churn)

# The accuracy is at 79% which means that 79 %of the total test points are classified correctly. It involves both types of labels - Yes and No .

# the Sensitivity is at 89%, which means that relatively high customer will 'not churn' are correctly spotted by Logistic Regression.

# Compare that to Specificity is at 54%, which is relatively low. This is the fraction of customer who will churn and are correctly identified.

#since our aim is to correctly identify the bad customer we will not use this threashold

#we will again check the metrics for .7

confusionMatrix(as.numeric(Log_Test$predicted_prob > 0.70),Log_Test$Churn)

# The accuracy is at 76% which means that 76 %of the total test points are classified correctly. It involves both types of labels - Yes and No .

# the Sensitivity is at 98%, which means that relatively high customer will 'not churn' are correctly spotted by Logistic Regression.

# Compare that to Specificity is at 17%, which is relatively low. This is the fraction of customer who will churn and are correctly identified.

#since our aim is to correctly identify the bad customer we will not use this threashold as well,

#so we will go with the threashold value of 0.3

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------Model - SVM:--------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#Since SVM also uses only numeric variables,we will use the logistic training and testing data sets for building SVM models

Log_Train$Churn <- as.factor(Log_Train$Churn)

Log_Test$Churn <- as.factor(Log_Test$Churn)

#we will first create a model with cost = 0.1

# svm_model_1 with cost = 0.1

svm_model_1 <- svm(Churn ~., data = Log_Train, kernel = "linear", cost = 0.1, scale = F)

#we will now use the plot function to see the model which we have created

plot(svm_model_1,Log_Train)

#fitted results

fitted.results <- predict(svm_model_1, Log_Test[,-2])

#error

misClasificError <- mean(fitted.results != Log_Test$Churn)

#Accuracy of model

print(paste('Accuracy',1-misClasificError))

table(fitted.results,Log_Test$Churn)

#now we will use the cross validation to get the optimal modal

confusionMatrix(fitted.results,Log_Test$Churn)

#with cost = 0.1, we are getting a accuracy of %80, Sensitivity of 91%, and Specificity of 50%

#now we will use the summary function 

summary(svm_model_1)

# svm_model_2 with cost = 0.5

svm_model_2 <- svm(Churn ~., data = Log_Train, kernel = "linear", cost = 0.5, scale = F) 

#we will now use the plot function to see the model which we have created

plot(svm_model_2,Log_Train)

#fitted results

fitted.results <- predict(svm_model_2, Log_Test[,-2])

#error

misClasificError <- mean(fitted.results != Log_Test$Churn)

#Accuracy of model

print(paste('Accuracy',1-misClasificError))

table(fitted.results,Log_Test$Churn)

#now we will use the cross validation to get the optimal modal

confusionMatrix(fitted.results,Log_Test$Churn)

#with cost = 0.5, we are getting a accuracy of %79, Sensitivity of 90%, and Specificity of 51%

# svm_model_3 with cost = 100

svm_model_3 <- svm(Churn ~., data = Log_Train, kernel = "linear", cost = 100, scale = F) 

#we will now use the plot function to see the model which we have created

plot(svm_model_3,Log_Train)

#fitted results

fitted.results <- predict(svm_model_3, Log_Test[,-2])

#error

misClasificError <- mean(fitted.results != Log_Test$Churn)

#Accuracy of model

print(paste('Accuracy',1-misClasificError))

table(fitted.results,Log_Test$Churn)

#now we will use the cross validation to get the optimal modal

confusionMatrix(fitted.results,Log_Test$Churn)

#with cost = 100, we are getting a accuracy of %79, Sensitivity of 89%, and Specificity of 54%

#so we will stick with 0.5 as our cost for SVM, So our final model will be


svm_model_final <- svm(Churn ~., data = Log_Train, kernel = "linear", cost = 0.5, scale = F)

summary(svm_model_final)

