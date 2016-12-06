#*****************************************************************************************************************************
#******************************************************"Mycar Dream"**********************************************************
##****************************************************************************************************************************

#first we will set our working directory where we have all the necessary files for this assignment

setwd("E:/UpGrad IIIT PGDDA Program/Course 3/Graded Assignment/Assignment Linear Regression")

#install.packages("MASS")
#install.packages("car")

library(ggplot2)
library(MASS)
library(car)

# We will be prepare the dataset for application of the linear regression model, to automate the process of predicting the car
# mileage which fits the customer preferences, based on the dataset of car features and attributes obtained by market surveys.

# We will be using "car mileage dataset" which the firm has gathered. It consists of a large dataset of different types of cars
# and their attributes across the world.

# The aim here is to predict the city-cycle fuel consumption in miles per gallon, in terms of 3 multivalued discrete and 5 
# continuous attributes. The detail description of the attributes are given in the data Dictionary.

# Firstly, we import the "carMPG.csv" and store it in a variable "carmileage".

carmileage <- read.csv("carMPG.csv",stringsAsFactors = FALSE)

# Now, we will display the dataset to get a primary understanding of the various attributes present in the car mileage 
# dataset.

View(carmileage)

# we Note that the data set has 9 variables in totality with 398 observation, where the variable Cylinders,Model year and Origin should be a factor
# and the rest 5 variables excluding Car name(nominal variable), should be of numeric/integer type

# We will ensure that the dataset is imported in the correct format. So, will check the structure of the carmileage dataset.

str(carmileage)

# we can see that the "Cylinders","Model_year" and "Origin" variable should be of factor type, but is not type by default. and
# we also see that "Horsepower" variable should be of numeric/integer type, but it is not by default.
# So, we will change the type of "Cylinders","Model_year" and "Origin" variable to factor.

carmileage$Cylinders <- as.factor(carmileage$Cylinders)

carmileage$Model_year <- as.factor(carmileage$Model_year)

carmileage$Origin <- as.factor(carmileage$Origin)

carmileage$Horsepower <- as.numeric(carmileage$Horsepower)

# now we will check the structure of carmileage dataset again to ensure that all the variables are in correct format now.
#we also notice that while converting horsepower to numeric NA was introduced, for values "?". we will deal with na later but
#will check the structure

str(carmileage)

# we see that the Cylinders" variable is now of factor type with 5 levels, "Model_year" variable is now of factor type with 13
#levels and "Origin" variable is now of factor type with 3 levels. and also "Horsepower" variable is now of numeric type

# Now, we need to remove duplicate values (if any) in the dataset.

unique(carmileage)

# we see that the total no. of observations in the dataset is still 398, indicating that there were no
# duplicates in the data set.

#we wlso need to check for data granularity, so we will chekc for unique key in our data set, so we check for duplicate values
#in car name

x <- which(duplicated(carmileage$Car_Name))

#we see that there are 93 duplicate value in car name so car name cannot be our unique key, but what happen if we check model
#year and car name.

carmileage$uniquekey <- paste(trimws(tolower(carmileage$Car_Name)),trimws(tolower(carmileage$Model_year)),sep = ",")

y <- which(duplicated(carmileage$uniquekey))

#we see that there are two duplicate for the uniquekey at row 175 and 343, having car name "ford pinto" and "plymouth reliant"

z <- carmileage[carmileage$Car_Name == "ford pinto",]

#we see that the car name and model number are repeated for model year 2010, so we are going to remove the duplicate row from
#our data set

carmileage <- carmileage[-y,]

#now we are also going to remove our uniquekey column

carmileage <- carmileage[,-10]


# Next important step is to check for missing values and treat if any.

sum(is.na(carmileage))

# we see that there is 6 NA values in the data sets, so lets check which column has NA's, will use sappy

sapply(carmileage, function(x) sum(is.na(x)))

#we notice that only horsepower has NA's , which is introduced by coercion when changing from chr to numeric so we will
#replace it with the mean of the horsepower

carmileage$Horsepower[which(is.na(carmileage$Horsepower))] <- mean(carmileage$Horsepower,na.rm = TRUE)

# Next step is to treat the outliers (if any). To check for outliers, we find out the quantile values at
# each 1% interval and wherever there is a high jump from one quantile to another, we cap/floor those
# values.

# Firstly, will check if there are outliers in the MPG variable. will Use quantile(<attribute
# name>,seq(0,1,0.01)) & boxplot to find the value at each percentile.

quantile(carmileage$MPG,seq(0,1,0.01))

boxplot(carmileage$MPG)

#we don't see any high jump in MPG, will next check Displacement.

quantile(carmileage$Displacement,seq(0,1,0.01))

boxplot(carmileage$Displacement)

#we don't see any high jump in Displacement, will next check Horsepower.

quantile(carmileage$Horsepower,seq(0,1,0.01))

boxplot(carmileage$Horsepower)

# we see that there is a jump between 97% and 98%. So, will cap all values above 198.000 (97%) to 198.000.
# will Use 

carmileage$Horsepower[which(carmileage$Horsepower > 198)] <- 198

#will next check Weight.

quantile(carmileage$Weight,seq(0,1,0.01))

boxplot(carmileage$Weight)

#we don't see any high jump in weight,so will move on to next Acceleration.

quantile(carmileage$Acceleration,seq(0,1,0.01))

boxplot(carmileage$Acceleration)

#we see that there is sudden jump between 0% and 1% So, will cap all values below 9.475 (1%) to 9.475.and also we see
#there is sudden jump between 98% and 99% So, will cap all values above 21.810 (98%) to 21.810.

carmileage$Acceleration[which(carmileage$Acceleration < 9.475)] <- 9.475

carmileage$Acceleration[which(carmileage$Acceleration > 21.810)] <- 21.810

#Now we have no more variable to chekc for outlier, we will move on to our factor variable, In the bignning we have seen that
#Cylinder had 5 levels from 3-8

#As a general practice is to club the levels of a factor variable together to reduce the number of levels. here we will club the
#levels of Cylinder variables into group of 2.

#so let us create 3 levels and binning the levels of "Cylinder" into  bins of 3. Create the first new level "Low" which will
#include factor levels 3,4 and 5 of the variable Cylinder.

levels(carmileage$Cylinders)[1:3] <- "low" 

# Display the structure of the data set to check if the clubbing of the levels has been done properly.

str(carmileage)

#Now,we will create the second level "mid" clubbing the 2 levels of "Cylinder".

levels(carmileage$Cylinders)[2] <- "mid" 

# Display the structure of the data set to check if the clubbing of the levels has been done properly.

str(carmileage)

#Now,we will create the 3rd level "high" clubbing the last levels of "Cylinder".

levels(carmileage$Cylinders)[3] <- "high" 

# Now, display the structure of the data set again to check if the clubbing of the levels has been done properly.

str(carmileage)

plot_1 <- ggplot(carmileage,aes(x=carmileage$Cylinders,y = carmileage$MPG))+geom_bar(stat = "identity")

plot_1 + labs(title="Car MPG based on Cylinder",x="Cylinder",y="MPG")

plot_2 <- ggplot(carmileage,aes(x=carmileage$Origin,fill = carmileage$Origin))+geom_bar()

plot_2 + labs(title="Car Origin",x="Origin",y="Frequency",fill = "Origin")

#now we will create a new variable with the brand name and convert itto a factor attribute

carmileage$Brand_Name <- gsub("\\ .*","",trimws(tolower(carmileage$Car_Name)))

# Now, will display the structure of the data set again to check if the new variable has been create successfully.

View(carmileage)

str(carmileage)

#while viewing the brand name variable we notice that the brand have some spelling incorrect so we will first currect those 
#incorrect brand name

carmileage$Brand_Name <- sub("\\chev.*","chevrolet",carmileage$Brand_Name)

carmileage$Brand_Name <- sub("toyouta","toyota",carmileage$Brand_Name)

carmileage$Brand_Name <- sub("vw","volkswagen",carmileage$Brand_Name)

carmileage$Brand_Name <- sub("vokswagen","volkswagen",carmileage$Brand_Name)

carmileage$Brand_Name <- sub("maxda","mazda",carmileage$Brand_Name)

carmileage$Brand_Name <- sub("\\mercedes.*","mercedes-benz",carmileage$Brand_Name)

#after correcting the brand name we will convert the brand name into factor variable

carmileage$Brand_Name <- as.factor(carmileage$Brand_Name)

#now we will check the structure of our data set

str(carmileage)

#we will now club the level of "Model_year" into group of 3 old,mid and new.

#Create the first new level "Old_Model" which will include factor levels 1:7 of the variable model_year.

levels(carmileage$Model_year)[1:4] <- "Old_Model" 

#will check the structure of the variable

str(carmileage)

#now we will create second level "Mid" which will include factor levels 2:5.

levels(carmileage$Model_year)[2:5] <- "Mid_Model" 

#will check the structure of the variable

str(carmileage)


#now we will create second level "New_Model" which will include factor levels 2:7.

levels(carmileage$Model_year)[3:7] <- "New_Model"

#will check the structure of the variable

str(carmileage)

plot_3 <- ggplot(carmileage,aes(x=carmileage$Model_year,fill = carmileage$Model_year))+geom_bar()

plot_3 + labs(title="Car Models Period",x = "",y="Number of Cars",fill = "Models")

# Let's view the carmileage dataframe to ensure that all the variable are binned appropriately.

View(carmileage)

#since we have already grouped car name into brand name we no longer require car_name variable in our model
#so we will remove the car_name variable from our data set

carmileage <- carmileage[,-9]

#will check the structure of the variable

str(carmileage)


# Now we have all the variables in the correct format. 

# Now we have completed all the basic data cleaning and preparation steps. The next step is to create dummy variables for all
# the categorical variables. 

# Dummy variables basically convert categorical values to integers so that models can perform mathematical operations on them.
#since dummy variable are created seperately, I have made a function that will automate the process of creating dummy variable
#of factor class from a given data set and the number of levels (n) will be n-1. I have place the source code for the function in
#Github at:-
#install_git("https://github.com/prk327/AtConP")
#library(AtConP), below is the source code for the function:-

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

#We will use the above function to create dummy variables of all the factor class

df.matrix(carmileage)  

#now we will Cbind all the dummy variable to the carmileage df

carmileage_1 <- cbind(carmileage[,c(-2,-7,-8,-9)],dummy_Cylinders,dummy_Brand_Name,dummy_Model_year,dummy_Origin)

# Now that we have prepared our data, let's finally view how the dataset looks like.

View(carmileage_1)

str(carmileage_1)

# Now we have a dataframe with all the attributes (dummy + numeric/integer) converted to numeric or integer type. This is now
# ready for building models.

# The last step is to split the data into training and testing data. Recall that as a rule of thumb, we randomly split
# training-testing data in 70-30 ratio. Since the carmileage_1 dataframe has 396 observations, we'll have 277 training and 119
# testing observations.

# To split the dataframe, we'll generate 277 random indices and assign the corresponding rows as training data. The rest 119
# indices will be testing data. To generate 70 % random indices, use the sample() function. sample(vector, n) chooses n
# observations randomly from the vector. For example, sample(1:100, 50) will choose 50 random values from 1-100.

# will Use the sample() function to generate training indices. The vector is 1:nrow(carmileage_1) and number of observations are
# | 0.7*nrow(carmileage_1). Store the result in train.indices.

train.indices <- sample(1:nrow(carmileage_1),0.7*nrow(carmileage_1))

# now we will extract all the 'train.indices' from the dataframe carmileage_1 and store the training data in train.

train <- carmileage_1[train.indices,]

# now we will create testing data. Note that carmileage_1[-<indices>, ] will return all the rows apart from the indices, which is what
# we want. will store the results in test.

test <- carmileage_1[-train.indices,]

# now lets Look at the str() of train.

str(train)

# now we have 277 random observations as train and 119 as test. we can now build predictive models using them.

# now our ultimate aim is to buid a model to predict the city-cycle fuel consumption in miles per gallon. So Let's start.

#display the train dataset to get a primary understanding of the various attributes present in the dataset.

View(train)

#aslo Ensure that the train dataset is in the correct format by checking it's structure.

str(train)

# Note that all the variables are of int/numeric type which is the requirement of model building. Now, we are all set to build
# the model on the train dataset. So, let's start.

#| Given the business objective, the dependent variable for the model is MPG.
#So, Let's build the first model and store it in a variable model_1. Use model_1<-lm().

model_1 <- lm(MPG~.,data = train)

# After building a model, next immediate step should always be to check the summary of the model to get an understanding of the
# significance of the independent variables. Check the summary of model_1 using summary().

summary(model_1)

# | Note that there are quite a number of insignificant variables. It is not worthwhile to build the model including all the
# | variables, so eventually we will exclude the insignificant ones.

# we will use step-wise function to remove the extremely insignificant variables in the beginning itself.
# To use the stepAIC(), load the MASS package from the library. Use library().


step <- stepAIC(model_1,direction = "both")

# Now, type "step" to check the model to be built after application of stepwise function, thereby removing the extremely
# insignificant variables.

step

# Note the "call" part of the output shows the new model. Store it in a new variable model_2. Use model_2<-lm(<model with
# significant variables>).

model_2 <- lm(formula = MPG ~ Horsepower + Weight + `Cylinders : mid` + 
                `Brand_Name : datsun` + `Brand_Name : plymouth` + `Brand_Name : pontiac` + 
                `Brand_Name : renault` + `Model_year : Old_Model` + `Model_year : Mid_Model` + 
                `Origin : 1` + Displacement, data = train)

# As mentioned previously, as a next step, check the summary of model_2. Use summary().

summary(model_2)

# Note that there are still insignificant variables present in the model. Also note that the adjusted R-squared has increased
# from the previous model.We will remove the insignificant variable based on p values or 3 stars,since we only required 5 variables
#for our model we won't be doing vif analysis,so apart from "Horsepower","Weight","`Cylinders : mid`","`Model_year : Old_Model`"
#and "`Model_year : Mid_Model`"
#all the other variables are insignificant,so we will remove all those variables from our new model_3.

model_3 <- lm(formula = MPG ~ Horsepower + Weight + `Cylinders : mid` +
                `Model_year : Old_Model` + 
                `Model_year : Mid_Model`, data = train)

#check the summary of model_3. Use summary().

summary(model_3)

# Now the model has 5 variables all of which are significant. So, model_3 is the final model.

# Note that the value of Adjusted R-squared is ~ 83%. Now, let us move forward to test the model on test data. Recall that we had
# created the test data in the beginning itself.

# Now, we will use model_4 to predict the MPG value for test data. will Use Predict<-predict(name of the model, test[, -10])

Predict_MPG <- predict(model_3,test[,-1])

# In order to check how accurate are the predictions of the model, we need to find the r-squared value between the predicted and
# actual values of MPG.

# R-squared is defined as the square of correlation between actual and predicted values of the variable. Use
# (cor(test$MPG,Predict_MPG))^2 to find the r-squared value.

(cor(test$MPG,Predict_MPG))^2

#we now get a r square of 84, which is good as per our case study, Generally, a deviation of +(-) 5% in the R-squared value
#of the model for the test data is acceptable.

c_test <- cbind(test,Predict_MPG)

plot_4 <- ggplot(c_test,aes(x=c_test$Predict_MPG,y=c_test$MPG))+geom_point() + geom_smooth()

plot_4 + labs(title="Relation between test and predicted MPG",x="Predicted MPG",y="Test MPG")

