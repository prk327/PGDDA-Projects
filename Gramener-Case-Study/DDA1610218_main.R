rm(list = ls())

#*********************************************************************************************************#
#****************************************Gramener Case Study**********************************************#
#*********************************************************************************************************#

#Loading ggplot packages
#loading tukeyc package

library(ggplot2)
library(moments)
library(GGally)



#Assign the directory for the case study

setwd("E:/UpGrad IIIT PGDDA Program/Course 2/Gramener Case Study")

#*********************************************************************************************************#
#*****************************Checkpoint-1: Data Cleaning and Preparation*********************************#
#*********************************************************************************************************#

#Loading the file loan.csv in r consol

loan <- read.csv("loan.csv",stringsAsFactors = FALSE,na.strings = "NA")

str(loan)


#*********************************************************************************************************#
#********************************************Business Rule************************************************#
#*********************************************************************************************************#

#since the company has decided to work only on driver variable we will 
#subset the loan file with driver variable

loan_Driver <- subset(loan,select = c(id,annual_inc,loan_amnt,funded_amnt,int_rate,grade,dti,emp_length,purpose,home_ownership,loan_status))

#*********************************************************************************************************#
#***************************************Checking Granularity**********************************************#
#*********************************************************************************************************#


#we will check for any dupliacte values in our driver file, since ID is a unique ID assigned
#we will use it for checking any duplicate entries

loan_Driver$id <- trimws(tolower(loan_Driver$id))

x <- loan_Driver[which(duplicated(loan_Driver$id)),]

#since we found that the duplicate is only related to blank,
#we will look for the value in loan_status column and once we confirm that there is no value in it either
#we will removes all the rows that have blank values in loan_status.

x <- which(loan_Driver$loan_status == "")

y <- loan_Driver[x,]

#since we see that all the values are blank of 7 obs we will remove those rows fro our data set

loan_Driver <- loan_Driver[-c(which(loan_Driver$loan_status == "")),]

#now we will check for the data dictionery or structure of loan_driver

str(loan_Driver)

#now we will check for any NA values in our driver variable 

sum(is.na(loan_Driver$id)) #no NA found

sum(is.na(loan_Driver$annual_inc)) #4 NA values found

#we also notice that there are NA values precent in emp_lenght with a value of n/a which we cannot get by is.na function
#so we will search by n/a in loan df

loan_Driver$emp_length <- trimws(tolower(loan_Driver$emp_length))

x <- which(loan_Driver$emp_length == "n/a")

str(x)

y <- loan_Driver[x,]

#now we will impute the value of mode of emp_lenght in n/a values

table(loan_Driver$emp_length)

#so we have 10+ years as the highest number of observation so we will replace n/a with 10+ years

loan_Driver[x,8] <- "10+ years"


#since we found only 4 NA values in annual_inc we will remove those rows as well

loan_Driver <- loan_Driver[-c(which(is.na(loan_Driver$annual_inc))),]

#now we will check for any NA values in our file

sum(is.na(loan_Driver))

#we don't find any NA values so our 2nd objective is completed.

#*********************************************************************************************************#
#******************************************Preparing the Data*********************************************#
#*********************************************************************************************************#

#we need to remove rows with loan_status = "fully paid"
#we will first check for the values in loan_status, and the better way to do that is to convert
#a character variable into factors

x <-  as.factor(loan_Driver$loan_status)

levels(x)

table(loan_Driver$loan_status)


#we see that there are two levels for fully paid value, 1st is "Fully Paid" itself and 2nd is "Does not meet the credit policy. Status:Fully Paid"
#so we will convert the value of 2nd value same as 1st value, and will do the same for "charge off" as well

x <- which(loan_Driver$loan_status == "Does not meet the credit policy. Status:Fully Paid")

y <- which(loan_Driver$loan_status == "Does not meet the credit policy. Status:Charged Off")

loan_Driver[x,11] <- "Fully Paid"

loan_Driver[y,11] <- "Charged Off"

#now we will remove the values "Fully Paid" from status col

loan_Driver <- loan_Driver[-c(which(loan_Driver$loan_status == "Fully Paid")),]

#now we will create a new col loan_status_1 with three levels based on the values of loan status that are:
#Current_new, Default_new and late



loan_Driver$loan_status <- trimws(tolower(loan_Driver$loan_status))

loan_Driver$loan_status_1 <-
  ifelse(
    loan_Driver$loan_status == "current" |
      loan_Driver$loan_status == "in grace period",
    "Current_New",
    ifelse(
      loan_Driver$loan_status == "default" |
        loan_Driver$loan_status == "charged off",
      "Default_New",
      ifelse(
        loan_Driver$loan_status == "late (16-30 days)" |
          loan_Driver$loan_status == "late (31-120 days)",
        "Late",
        "NA"
      )
    )
  )


#now we will crate two bin variable for int_rate and emp_lenght
#we will create a new bin int rate grp

str(loan_Driver$int_rate)

#since the int_rate is a character variable we will first convert it to numberic variable

loan_Driver$int_rate <-  as.double(sub("%","",loan_Driver$int_rate))

#now we will group int_rate variable as below:

loan_Driver$int_rate_grp <- 
  ifelse(
    loan_Driver$int_rate < 10,"Low",
    ifelse(
      loan_Driver$int_rate >= 10 &
        loan_Driver$int_rate <= 18,"Medium",
      ifelse(
        loan_Driver$int_rate > 18,"High",
        "NA"
      )
    )
  )

#grouping of emp_length varaible as below

str(loan_Driver$emp_length)

table(loan_Driver$emp_length)

loan_Driver$emp_length <- trimws(tolower(loan_Driver$emp_length))

loan_Driver$emp_len_grp <-
  ifelse(
    loan_Driver$emp_length %in% c("< 1 year", "1 year", "2 years", "3 years", "4 years"),
    "Junior",
    ifelse(
      loan_Driver$emp_length %in% c("5 years", "6 years", "7 years", "8 years") ,
      "Mid-Level",
      ifelse(
        loan_Driver$emp_length %in% c("9 years", "10+ years"),
        "Senior",
        "NA"
      )
    )
  )


table(loan_Driver$emp_len_grp)

#since we are not interested in variable int_rate and emp_length and loan_status we will remove it from our data set

Loan_Final <- subset(loan_Driver,select = -c(int_rate,emp_length,loan_status))



#now our first check point is completed

#*********************************************************************************************************#
#***********************************Checkpoint 2: Exploratory Data Analysis*******************************#
#*********************************************************************************************************#

#*********************************************************************************************************#
#*******************************************Univariate Analysis*******************************************#
#*********************************************************************************************************#


#first we will check the types of attributes in our data set using str() command

str(Loan_Final)

#we see that there are 11 variables and variable "loan_status_1" is the one which we will consider for our analysis
#now we will check for the summary of the data set

summary(Loan_Final)

#since we have character varaible we will first convert them into factor variable for better analysis
#we will create a function to convert all character variable to factor variable

chr_to_factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    } 
    return(x)
  }

#now we will use the function chr_to_factor to convert character variable to factor variable

Loan_Final <- chr_to_factor(Loan_Final)


str(Loan_Final)

summary(Loan_Final)

#from visual inspection we see that in annual_inc there is some outlies present so we will analyse this 
#through quantile function

quantile(Loan_Final$annual_inc)

#we see that 75% of the annual income are with in 78000 the rest are may be outliers
#in order to accurately understand the outliers we will use boxplot on all continuous variables

boxplot(Loan_Final$annual_inc) #outlier present

boxplot(Loan_Final$loan_amnt) #No Outlier

boxplot(Loan_Final$funded_amnt) #outlier Present

boxplot(Loan_Final$dti) #No Outlier

#so we see that there exist an outlier in annual_inc and funded_amnt and both are present at the high end
#so we will use the boxplot.stat command to get the exact outlier values

boxplot.stats(Loan_Final$annual_inc)

#we will use the outlier value of annual_inc to get the index of rows for imputation

outlier_values <- boxplot.stats(Loan_Final$annual_inc)$out

x <- which(Loan_Final$annual_inc %in% outlier_values)

#we will impute the outlier with the median of annual_inc

Loan_Final[x,2] <- median(Loan_Final$annual_inc)

#now we will check for any outlier values in annual_inc

boxplot(Loan_Final$annual_inc)

#now we will use the outlier values of funded_amt to get the row index

outlier_values <- boxplot.stats(Loan_Final$funded_amnt)$out

x <- which(Loan_Final$funded_amnt %in% outlier_values)

#we will impute the outlier with median

Loan_Final[x,4] <- median(Loan_Final$funded_amnt)

#now we will again check for any outlier values

boxplot(Loan_Final$funded_amnt)

#since we have treated all the outlier values of continuous variable by using median
#Now we will check for the distribution of the outlier variable

#for Annual_inc

x <- Loan_Final$annual_inc

y <- dnorm(x,mean = mean(x),sd= sd(x))

plot(x,y,main = "Distribution of Annual Income",xlab ="Annual Income",ylab ="Frequency" ) 


#for Funded_amt

x <- Loan_Final$funded_amnt

y <- dnorm(x,mean = mean(x),sd= sd(x))

plot(x,y,main = "Distribution of Funded Amount",xlab = "Funded Amount",ylab = "Frequency")

#again we will check for the outliers using skewness and kurtosis
#skewness of value equal to 0 is normal and kurtosis of value +3 is normal 

skewness(Loan_Final$annual_inc) #value of 0.7367393 is not far from normal distribution
kurtosis(Loan_Final$annual_inc) #value of 3.265278 is some what equal to +3

skewness(Loan_Final$funded_amnt) #value of 0.6494923 is not far from normal distribution
kurtosis(Loan_Final$funded_amnt) #value of 2.656002 is some what equal to 3

#so we can say that after visual inspection and as well as quantitative inspection
#the continuous variable in our data set are normally distributed


Plot1 <- ggplot(Loan_Final,aes(x=Loan_Final$annual_inc)) + geom_histogram(col="red",fill="blue",alpha=0.4,binwidth = 1000)

Plot1 + labs(title = "Distribution of Annual Income", x = "Annual Income", y = "Frequency")

Plot2 <- ggplot(Loan_Final,aes(x=Loan_Final$loan_amnt)) + geom_histogram(col="yellow",fill="red",alpha=0.4,binwidth = 1000)

Plot2 + labs(title = "Distribution of Loan Amount",x = "Loan Amount",y = "Frequency")


Plot3 <- ggplot(Loan_Final,aes(x=Loan_Final$funded_amnt)) + geom_histogram(col="green",fill="blue",alpha=0.4,binwidth = 1000)

Plot3 + labs(title= "Distribution of Funded Amount",x = "Funded Amount",y = "Frequency")

Plot4 <- ggplot(Loan_Final,aes(x=Loan_Final$dti)) + geom_histogram(col="green",fill="red",alpha=0.4)

Plot4 + labs(title = "Distribution of DTI",x = "Debt To Income Ration",y = "Frequency")



#*********************************************************************************************************#
#******************************************Multivariate Analysis******************************************#
#*********************************************************************************************************#


#now we want to find out if there is any corelation exist between any of the continuous variables

#we will first subset our data set with only continuous variables

con_loan  <- subset(Loan_Final,select = c(annual_inc,loan_amnt,funded_amnt,dti))


#now we will use the package ggally to analyse correlation between different variables

ggpairs(con_loan)

#using Correlation Matrix cor()

cor(con_loan)


#from the above package we see clearly that there exist a corelation between variable
#Funded_Amount and Loan_amount, and the corelation value is 0.859 which is equal to 1 positive corelated
#which means if the loan amount increses funded amount also increases which is what we expect

#we can also see this with the help of a scatter plot

plot5 <- ggplot(Loan_Final,aes(loan_amnt,funded_amnt)) + geom_point(col = "red",alpha = 0.4)

plot5 + labs(title = "Relation between Funded Amount and Loan Amount",x = "Loan Amount",y = "Funded Amount")

#now we will Analysing Categorical variables using summary function
#since we have 6 variable to analyse we will create a data set of only categorical variables

cat_loan <- subset(subset(Loan_Final,select = -c(id,annual_inc,loan_amnt,funded_amnt,dti)))

summary(cat_loan)

#from the summary of categorical variable we see that most of the people were taking loan
#for debt_consolidation,junior employee are the one who are taking more loan,and there are 
#high number of default in loan repayment,where as most of the interest rate are at medium

#we need to figure out the reason for default for that we will plot

#first we will check for the emp_len_grp who tends to default more

emp_plot <- ggplot(Loan_Final,aes(x=emp_len_grp,fill = emp_len_grp))+ geom_bar()+facet_wrap(~loan_status_1)

emp_plot + labs(title="No of loan taken by each employment group and their Status",x = "",y="No of Loan Availed",fill = "Employment Group")

#we clearly see that junior employees with a employment lent of 1 to 4 years tend to default more than rest
#of their counterparts

#now we will check for purpose which are in high default

pur_plot <- ggplot(Loan_Final,aes(x=purpose,fill = purpose))+ geom_bar()+facet_wrap(~loan_status_1)

pur_plot + labs(title="Purpose of loan taken and their Status",x = "",y="No of Loan Availed",fill = "Purpose")

#we see that high no of default are for debt_consolidation
#now we will check for home ownership

home_plot <- ggplot(Loan_Final,aes(x=home_ownership,fill = home_ownership))+ geom_bar()+facet_wrap(~loan_status_1)

home_plot + labs(title="home_ownership of the applicant and their Status",x = "",y="No of Loan Availed",fill = "home_ownership")

#we see that applicant with home_ownership of Mortgage and Rent tends to default more

##now we will check for int_rate_grp

int_plot <- ggplot(Loan_Final,aes(x=int_rate_grp,fill = int_rate_grp))+ geom_bar()+facet_wrap(~loan_status_1)

int_plot + labs(title="Interest Rate of the Loan and their Status",x = "",y="No of Loan Availed",fill = "Interest Rate")

#we see that if the interest rate is medium people tends to default more

##now we will check for group

grp_plot <- ggplot(Loan_Final,aes(x=grade,fill = grade))+ geom_bar()+facet_wrap(~loan_status_1)

grp_plot + labs(title="Grade Assigned by LC and their Status",x = "",y="No of Loan Availed",fill = "Grade")

#now we need to see if annual income differ is loan status

Inc_Stat <- ggplot(Loan_Final,aes(x = Loan_Final$loan_status_1,y = Loan_Final$annual_inc,fill = loan_status_1)) + geom_boxplot()

Inc_Stat + labs(title = "Distribution of Annual Income on Loan Status",x="",y = "Annual Income",fill = "Loan Status")

#now we see the distribution of funded amount on loan status

Fun_Stat <- ggplot(Loan_Final,aes(x=loan_status_1,y=funded_amnt,fill = loan_status_1)) + geom_boxplot()

Fun_Stat + labs(title = "Distribution of Funded Amount On Loan Status",x="",y="Funded Amount",fill = "Loan Status")

#now we see the distribution of loan amount on loan status

Lon_stat <- ggplot(Loan_Final,aes(x=loan_status_1,y=loan_amnt,fill = loan_status_1)) + geom_boxplot()


Lon_stat + labs(title = "Distribution of Loan Amount on Status of Loan",x="",y="Loan Amount",fill = "Loan Status")

#now we check for DTI with loan status

dti_stat <- ggplot(Loan_Final,aes(x=loan_status_1,y=dti,fill = loan_status_1)) + geom_boxplot()


dti_stat + labs(title = "Distribution of DTI over Loan Status",x="",y="Debt To Income Ratio",fill = "Loan Status")


#we again check for DTI on Interest Rate

dti_int <- ggplot(Loan_Final,aes(x= int_rate_grp,y=dti,fill = int_rate_grp)) + geom_boxplot()

dti_int + labs(title = "Distribution of DTI on Interest Rate Group",x="",y="Debt To Income Ratio",fill = "Interest Rate Group")


#we will aslo export the data set for further analysis by tableau

write.table(Loan_Final,file = "Loan_Final.csv",sep = ",",row.names = FALSE)

#Conclusion

#now we can conclude that the combination of employmet_len = Junior,home_ownership = mortgage
#purpose = debt_consolidation are very likely to default

#*********************************************************************************************************#
#***********************************Checkpoint 3: Hypothesis Testing**************************************#
#*********************************************************************************************************#

#Now we to check whether there is significant difference between mean of two categorical variables
#Loan Status and Interest Rate on continuous variables

#First we will create different sets of data for calculating our hypothesis

#we will start with Loan Status



Loan_Hypo2 <- subset(Loan_Final,select = c(annual_inc,loan_amnt,funded_amnt,dti,loan_status_1))

Loan_Current <- subset(Loan_Hypo2,Loan_Hypo2$loan_status_1 == "Current_New")

Loan_Default <- subset(Loan_Hypo2,Loan_Hypo2$loan_status_1 == "Default_New")


#1 Test , Whether there is a significant difference between Annual_Default and Annual_Current mean

#Null Hypothesis : H0 : A1 = A2

#Anternative Hypothesis : H1 : A1 <> A2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

A1 <- Loan_Current$annual_inc

A2 <- Loan_Default$annual_inc

Test1 <- t.test(A1,A2,alternative = "two.sided",conf.level = 0.95)

Test1


#Since the p-value is less than alpha 2.2e-16 or is less than 0.0001 we can reject the null Hypothesis and say that 
#there is significant difference between means of A1 and A2

#2 Test Whether there is a significant difference between Loan_Amount_Default and Loan_Amount_Current mean

#Null Hypothesis - H0 : L1 = L2

#Alternative Hypothesis - H1 : L1 <> L2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

L1 <- Loan_Current$loan_amnt

L2 <- Loan_Default$loan_amnt

Test2 <- t.test(L1,L2,alternative = "two.sided",mu = 0,conf.level = 0.95)

Test2

#Since the p-value is less than alpha 2.2e-16 or is less than 0.0001 we can reject the null Hypothesis and say that 
#there is significant difference between means of L1 and L2

#3 Test Whether there is a significant difference between Funded_Amount_Default and Funded_Amount_Current mean

#Null Hypothesis - H0 : F1 = F2

#Alternative Hypothesis - H1 : F1 <> F2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

F1 <- Loan_Current$funded_amnt

F2 <- Loan_Default$funded_amnt

Test3 <- t.test(F1,F2,alternative = "two.sided",conf.level = 0.95)

Test3

#Since the p-value is less than alpha 2.2e-16 or is less than 0.0001 we can reject the null Hypothesis and say that 
#there is significant difference between means of F1 and F2

#4 Test Whether there is a significant difference between DTI_Default and DTI_Current mean

#Null Hypothesis - H0 : D1 = D2

#Alternative Hypothesis - H1 : D1 <> D2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

D1 <- Loan_Current$dti

D2 <- Loan_Default$dti

Test4 <- t.test(D1,D2,alternative = "two.sided",conf.level = 0.95)

Test4

#Since the p-value is less than alpha 0.025 or P value = 0.002016 we can reject the null Hypothesis and say that 
#there is significant difference between means of D1 and D2 with 95% Confidence


#Now we will test for Interest rate group, and befire that we will subset the data

Interest_Hypo <- subset(Loan_Final,select = c(annual_inc,loan_amnt,funded_amnt,dti,int_rate_grp))
  
Interest_High <- subset(Interest_Hypo,Interest_Hypo$int_rate_grp == "High")

Interest_Low <- subset(Interest_Hypo,Interest_Hypo$int_rate_grp == "Low")

#Interest Rate Group Test

#5 Test Whether there is a significant difference between Annual_Income_High_Interest_Rate and Annual_Income_Low_Interest mean

#Null Hypothesis - H0 : A3 = A4

#Alternative Hypothesis - H1 : A3 <> A4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

A3 <- Interest_High$annual_inc

A4 <- Interest_Low$annual_inc

Test5 <- t.test(A3,A4,alternative = "two.sided",conf.level = 0.95)

Test5

#Since the p-value is less than alpha 2.2e-16 we can reject the null Hypothesis and say that 
#there is significant difference between means of A3 and A4 with 95% Confidence

#6 Test Whether there is a significant difference between Loan_Amount_High_Interest_Rate and Loan_Amount_Low_Interest mean

#Null Hypothesis - H0 : L3 = L4

#Alternative Hypothesis - H1 : L3 <> L4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

L3 <- Interest_High$loan_amnt

L4 <- Interest_Low$loan_amnt

Test6 <- t.test(L3,L4,alternative = "two.sided",conf.level = 0.95)

Test6

#Since the p-value is less than alpha 2.2e-16 we can reject the null Hypothesis and say that 
#there is significant difference between means of L3 and L4 with 95% Confidence


#7 Test Whether there is a significant difference between Funded_Amount_High_Interest_Rate and Funded_Amount_Low_Interest mean

#Null Hypothesis - H0 : F3 = F4

#Alternative Hypothesis - H1 : F3 <> F4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

F3 <- Interest_High$funded_amnt

F4 <- Interest_Low$funded_amnt

Test7 <- t.test(F3,F4,alternative = "two.sided",conf.level = 0.95)

Test7

#Since the p-value is less than alpha 2.2e-16 we can reject the null Hypothesis and say that 
#there is significant difference between means of F3 and F4 with 95% Confidence

#8 Test Whether there is a significant difference between DTI_High_Interest_Rate and DTI_Low_Interest mean

#Null Hypothesis - H0 : D3 = D4

#Alternative Hypothesis - H1 : D3 <> D4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

D3 <- Interest_High$dti

D4 <- Interest_Low$dti

Test8 <- t.test(D3,D4,alternative = "two.sided",conf.level = 0.95)

Test8

#Since the p-value 9.795e-05 is greater than alpha 0.025 we cannot reject the null Hypothesis and say that 
#there is no significant difference between means of D3 and D4


#*********************************************************************************************************#
#**************************************************Thank You**********************************************#
#*********************************************************************************************************#


