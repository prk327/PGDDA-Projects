##--------------------------------------------------------------------------------------------------------------------------------------##
##--------------------------------------------------------BFS Capstone Project----------------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##



#devtools::install_git("https://github.com/klarsen1/Information.git")
#install.packages('caret')
library(caret)
library(plyr)
library(Information)
library(ggplot2)
#devtools::install_git("https://github.com/prk327/AtConP.git")
library(AtConP)
library(MASS)
library(car)
library(glmnet)
#install.packages("ROSE")
library(ROSE)
#install.packages("ggthemes")
library(ggthemes)
library(ROCR)
library(grid)
library(dplyr)
library(scales)
library(gridExtra)
library(data.table)
library(tidyr)
#install.packages("broom")
library(broom)
library(Hmisc)
library(ROCR)

##Business Understanding:-

#We will start our analysis by using the Cross Industry Standard Process for Data Mining. So First we will get ourself familier with 
#the business of the company and its problem.

#So we will be working for company "CredX" which is a leading credit card provider. It gets thousands of credit
#card applications every year. But the problem is it is experiancing an increase in credit loss and therefore 
# we have to find out the reason for this increase in credit loss and also to used the historical data to build
#a predictive models that can distinguish between the good and bad customers.

#So lets begin our analysis.

##Data Understanding

#To understand the data we first have to import the date into r enviournment..
#so first we will define our working directory as follows

setwd("E:/UpGrad IIIT PGDDA Program/BFS Capstone Project")

#now we will import the data into r enviournment, we will name the "demographic" data as credx_demo and credit bureau data as credx_credit

credx_demo <- read.csv(file = "Demographic data.csv",header = TRUE,stringsAsFactors = FALSE)

credx_credit <- read.csv(file = "Credit Bureau data.csv",header = TRUE,stringsAsFactors = FALSE)

#now we will explore the data to get ourself familier with both the data sets.

summary(credx_demo)

summary(credx_credit)

#So we will check first the number of observation in credit and demographic data

nrow(credx_credit)

nrow(credx_demo)

ncol(credx_credit)

ncol(credx_demo)

#we see that both the data sets have equal number of observation i.e 71295. the credit data has 19 variables and demographic data has 12
#variables

str(credx_credit)

str(credx_demo)

#we see that the credit bureau data has 18 independent variable and 1 dependent variable "Performance.Tag", and all variable are of integer
#type. The demographic data has 11 independent variabe and 1 dependent variable "Performance.Tag", with 7 integer and 5 character variable.

#let also see the data visually using view()

View(credx_demo)

View(credx_credit)

#through visual inspection we see that demographic data has some blank data aswell. we will check to see if there is any NA present in our
#both the data or not.

sapply(credx_demo,function(x) sum(is.na(x)))

sapply(credx_credit,function(x) sum(is.na(x)))

sum(is.na(credx_demo))

sum(is.na(credx_credit))

#we see that demographic data has 1428 na values and credit data has 3028 na values, apart from this demographic data also has blank values

#to overcome this problem in demographic data we may either reimport the demographic data with option na.strings = c(""," ","NA"), or we can impute
#blank with na through sappy or lappy

#the first option is what I prefer, so I will reimport the data using na.strings = c(""," ","NA",NA) option.

credx_demo <- read.csv(file = "Demographic data.csv",header = TRUE,na.strings = c(""," ","NA",NA))

sapply(credx_demo,function(x) sum(is.na(x)))

sum(is.na(credx_demo))

#now if we again check our data for na values we see that we have 1577 na values, which is 149 more data points.

#now we will check to see what are those variables which have NA's, we will use summary to check these in detail

summary(credx_demo)

summary(credx_credit)

#we see that the age variable has the lowest value as -3 which is clearly an error and also income has -0.5 as its lowest value which is 
#again an outlier.

#What is more concerning is that the dependent variable has 1425 NA's, which means that this population has been denied
#for loan. So we will remove these observation and store in validation data sets.

#let also check any duplicacy in the data we have, since Application.ID is unique we will check its duplicate

x <- credx_demo[which(duplicated(credx_demo$Application.ID)),]

#we see that there are 3 observation which have duplicate application id, so we will use the application id to see what are those duplicat

View(credx_demo[credx_demo$Application.ID %in% c(765011468,653287861,671989187),])

#we see that all the 3 observation have duplicate application id and there mustbe some error in recording those transaction, so we will
#remove those observation from our demographic data

credx_demo <- credx_demo[-which(duplicated(credx_demo$Application.ID)),]

#now we will check to see if credit data has any duplicate

View(credx_credit[which(duplicated(credx_credit$Application.ID)),])

# we see that the same observation is also duplicate in credit data, so we will remove it

credx_credit <- credx_credit[-which(duplicated(credx_credit$Application.ID)),]

#now we have 71292 observation in both our data sets,

#now we will create a master file by merging both demographic and credit bureau data.

#first we will create a vector with the column name of demo and credit

Demographic <- colnames(credx_demo)

Credit_bureau <- colnames(credx_credit)

#now we will check to see if all the application id were present in both the data

which(credx_demo$Application.ID %in% credx_credit$Application.ID == FALSE)

#We see that there were no application ID that were missing in both the file.so we will merge both the file using Application id

Master_Data <- merge(credx_demo,credx_credit,by="Application.ID",all = FALSE)

#we see that Performance.Tag tag which is our dependent variable are created for both the file, we will verify to see if the both
#have the same performance tag, so as to keep only one dependent variable in our master file,
#in orger to check the duplicate we will subtract one from another and if we found non zero, we will conclude that they are different

Master_Data$Tag_dif <- Master_Data$Performance.Tag.x - Master_Data$Performance.Tag.y

#now we will check non zero in Tag_diff

levels(factor(Master_Data$Tag_dif))

#since we have only one level which is 0, we can conclude that both the variable are same and we will only keep one

Master_Data <- subset(Master_Data,select = -c(Performance.Tag.x,Tag_dif))

colnames(Master_Data)[29] <- "Performance.Tag"

#now we will check the summary of our data

summary(Master_Data)

str(Master_Data)

#from our earlier inspection we know that Performance.Tag has 1425 NA's, so we will remove those from our master file and store it in
#a new file name validation

validation <- Master_Data[which(is.na(Master_Data$Performance.Tag)),]

#next, we need to remove those NA's from our master file

Master_Data <- Master_Data[-which(is.na(Master_Data$Performance.Tag)),]

#So, after removing the NA's we have 69867 observation. 
#next, we need to check the total number of missing values in our master file

sum(is.na(Master_Data))

(sum(is.na(Master_Data))/nrow(Master_Data))*100

#we see that the total number of NA in our master file is 1718, which is only 2.5% of the total observation we can safely remove those
#data points from our master file

#we need to first check which observation has missing values

sapply(Master_Data,function(x) sum(is.na(x)))

#we see that, Gender, Marital.Status, No.of.dependents, Education, Profession, Type.of.residence, Avgas.CC.Utilization, No.of.trades.opened.in.last.6.months,
#Presence.of.open.home.loan, and Outstanding.Balance have NA's, so we will remove them one by one

Master_Data <- Master_Data[-which(is.na(Master_Data$Gender)),]

Master_Data <- Master_Data[-which(is.na(Master_Data$Marital.Status..at.the.time.of.application.)),]

Master_Data <- Master_Data[-which(is.na(Master_Data$No.of.dependents)),]

Master_Data <- Master_Data[-which(is.na(Master_Data$Education)),]

Master_Data <- Master_Data[-which(is.na(Master_Data$Profession)),]

Master_Data <- Master_Data[-which(is.na(Master_Data$Type.of.residence)),]

Master_Data <- Master_Data[-which(is.na(Master_Data$Avgas.CC.Utilization.in.last.12.months)),]

#we will check to see how many more NA's were there

sapply(Master_Data,function(x) sum(is.na(x)))

sum(is.na(Master_Data))

#we see that there were no more na values left in any of the column, we will also check the %of NA valus which we have removed

((69867 - nrow(Master_Data))/69867)*100

#So, we have just removed only 1.6 % of the observation which has NA values. Now It's time to use the Information package to calculate
#WOE and IV for all the variables.

#But, before that we also need to remove the Application ID since its a key variables to access each data points, and its not useful
#for model building

Applicant <- Master_Data[,1]

Master_Data <- Master_Data[,2:29]

##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------EDA Using WOE -------------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

#WOE and IV Analysis:

str(Master_Data)

#now we will create a Information value table using create_infotables command

IV <- create_infotables(data = Master_Data,y = "Performance.Tag",parallel = TRUE)

#now we will look at the most important variables 

head(IV$Summary)

# We see that "No of Inquiries in last 12 months excluding home auto loans", "Avgas CC Utilization in last 12 months", "No of PL trades
# opened in last 12 months", "No of trades opened in last 12 months" has IV more than 0.3 which indicates that these variable have Strong
# predictive Power where as "Outstanding Balance" and "Total No of Trade" has Medium predictive Power.


#Now we will create a seperate data sets of WOE values, we will use function from my github repository to impute the woe values.

master_woe <- DF.Replace.WOE(Master_Data,IV,"Performance.Tag")

#now we will check to see the distribution of the age:woe values

plot_infotables(IV,"Age")

#we see that the age group between 51 to 53 has the high chances of default since their WOE is very less.

#next, Gender

plot_infotables(IV,"Gender")

#we see that in comparison to female male has the high chances of default.

#next, Marital.Status

plot_infotables(IV,"Marital.Status..at.the.time.of.application.")

#we see that the married has the high chances of default, since their percentage of good customers were very less in comparison
# to their bad customers

plot_infotables(IV,"No.of.dependents")

#we see that the applicant who has 2 dependents has the higher chances of default.

plot_infotables(IV,"Income")

#we see that the applicant who has "Income" group between 49 to 60 has the higher chances of default.

plot_infotables(IV,"Education")

#we see that the applicant whose Education is masters have equal number of good and bad customers and others have the high evidence

plot_infotables(IV,"Profession")

#we see that the applicant whose Profession is SAL has the higher chances of default.

plot_infotables(IV,"Type.of.residence")

#we see that the applicant whose Type.of.residence is others has the higher chances of default.

plot_infotables(IV,"No.of.months.in.current.residence")

#we see that the applicant whose No.of.months.in.current.residence is between 6 to 9 months has the higher chances of default.

plot_infotables(IV,"No.of.months.in.current.company")

#we see that the applicant whose No.of.months.in.current.company is between 41 to 61 months has the higher chances of default.

plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")

#we see that the applicant whose No.of.times.90.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.

plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")

#we see that the applicant whose No.of.times.60.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.

plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")

#we see that the applicant whose No.of.times.30.DPD.or.worse.in.last.6.months is 0 has the higher chances of default and same goes for
#12 months also.

plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")

#we see that the applicant whose Avgas.CC.Utilization.in.last.12.months is between 0 to 14 has the higher chances of default.

plot_infotables(IV,"No.of.trades.opened.in.last.6.months")

#we see that the applicant whose No.of.trades.opened.in.last.6.months is between 0 to 1 has the higher chances of default.

plot_infotables(IV,"No.of.trades.opened.in.last.12.months")

#we see that the applicant whose No.of.trades.opened.in.last.12.months is between 0 to 2 has the higher chances of default.

plot_infotables(IV,"No.of.PL.trades.opened.in.last.12.months")

#we see that the applicant whose No.of.PL.trades.opened.in.last.6 and 12.months is 0 has the higher chances of default.

plot_infotables(IV,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

#we see that the applicant whose No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. is 0 has the higher chances of default.

plot_infotables(IV,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

#we see that the applicant whose No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. is 0 has the higher chances of default.

plot_infotables(IV,"Presence.of.open.home.loan")

#we see that the applicant whose Presence.of.open.home.loan is 1 has the higher chances of default.

plot_infotables(IV,"Outstanding.Balance")

#we see that the applicant whose Outstanding.Balance is between 0 to 56065 and between 1362889 to 3289931 has the higher chances of default.

plot_infotables(IV,"Total.No.of.Trades")

#we see that the applicant whose Total.No.of.Trades is between 1 to 4 has the higher chances of default.

plot_infotables(IV,"Presence.of.open.auto.loan")

#we see that the applicant whose Presence.of.open.auto.loan is between 1 has the higher chances of default.

##That being us to the end of EDA using WOE, now we will use traditional method to clean our master data using different statistical techniques.

##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------EDA & Outlier Treatment----------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##


#let check corelation between continous variables, using ggplot, but before that we need to create a DF of continuous variable

convar <- con_var(Master_Data)

#now we will check the coorelation

x <- as.data.frame(cor(convar))

#we see that there is correlation between "No.of.trades.opened.in.last.12.months" and "No.of.trades.opened.in.last.6.months", 
# between "No.of.PL.trades.opened.in.last.12.months" and "No.of.trades.opened.in.last.6.months", and between "Total.No.of.Trades" and
# "No.of.trades.opened.in.last.6.months"
#we will visualize this using ggplot

ggplot(
  Master_Data,
  aes(
    x = No.of.trades.opened.in.last.12.months,
    y = No.of.trades.opened.in.last.6.months,
    col = factor(Performance.Tag)
  )
) + geom_smooth() + ggtitle("Correlation between No of Trades open in last 6 months and 12 months") + labs(col = "Performance Tag")

#we see that as the trades in last 12 month increases the number of trades in last 6 month also increases which is obvious

ggplot(
  Master_Data,
  aes(
    x = No.of.PL.trades.opened.in.last.12.months,
    y = No.of.trades.opened.in.last.12.months,
    col = factor(Performance.Tag)
  )
) + geom_smooth() + ggtitle("Correlation between No of Trades open in last 12 months and PL trades in 12 months") + labs(col = "Performance Tag")

#we see that as no of pl trades in last 12 months increases no of trades in 12 months also increases

ggplot(
  Master_Data,
  aes(
    x = Total.No.of.Trades,
    y = No.of.trades.opened.in.last.12.months,
    col = factor(Performance.Tag)
  )
) + geom_smooth() + ggtitle("Correlation between Total No of Trades and trades in 12 months") + labs(col = "Performance Tag")

#we see that in the beginning months applicant uses the card very ofthen till they have used 10 transaction and then they slow down their card usage.


#now let's check the distribution of Gender

ggplot(Master_Data, aes(
  x = Master_Data$Gender,
  fill = factor(Master_Data$Performance.Tag)
)) + geom_bar(position = "fill") + ggtitle("Distribution of Gender over Performance Tag") + xlab("Gender") + ylab("count") + labs(fill =
                                                                                                                                    "Performance Tag")

#we see that both gender group are distributed proportionally to performance tag, though population of male applicant are higher

#now we will check the distribution of Marital.Status..at.the.time.of.application.

ggplot(
  Master_Data,
  aes(
    x = Master_Data$Marital.Status..at.the.time.of.application.,
    fill = factor(Master_Data$Performance.Tag)
  )
) + geom_bar(position = "stack") + ggtitle("Distribution of Marital Status at the time of application over Performance Tag") + xlab("Marital Status") + ylab("count") + labs(fill =
                                                                                                                                                                               "Performance Tag")

#we see that both Marital Status group are distributed proportionally to performance tag, though population of married applicant are higher

#now we will check distribution of No.of.dependents

ggplot(Master_Data,
       aes(
         x = Master_Data$No.of.dependents,
         fill = factor(Master_Data$Performance.Tag)
       )) + geom_bar(position = "fill") + ggtitle("Distribution of No of dependents over Performance Tag") + xlab("Dependents") + ylab("count") + labs(fill =
                                                                                                                                                         "Performance Tag")

#we see that the applicant with 3 Dependents has the higher chances of default than the rest

#next Profession,

#we will check its distribution

ggplot(Master_Data, aes(
  x = Master_Data$Profession,
  fill = factor(Master_Data$Performance.Tag)
)) + geom_bar(position = "fill") + ggtitle("Distribution of Profession over Performance Tag") + xlab("Profession") + ylab("count") + labs(fill =
                                                                                                                                            "Performance Tag")

#we see that the applicant with Profession SE has the higher chances of default than the rest

#next, Type.of.residence

#we will check its distribution

ggplot(Master_Data,
       aes(
         x = Master_Data$Type.of.residence,
         fill = factor(Master_Data$Performance.Tag)
       )) + geom_bar(position = "fill") + ggtitle("Distribution of Residence Type over Performance Tag") + xlab("Residence") + ylab("count") + labs(fill =
                                                                                                                                                      "Performance Tag")

#we see that the applicant with residence type living with parents has the higher chances of default than the rest, though high volume of
#applicant are in rented residence

#next, No.of.trades.opened.in.last.6.months

#we will check its distribution

ggplot(
  Master_Data,
  aes(
    x = Master_Data$No.of.trades.opened.in.last.6.months,
    fill = factor(Master_Data$Performance.Tag)
  )
) + geom_bar(position = "fill") + ggtitle("Distribution of No of trades opened in last 6 months over Performance Tag") + xlab("Trades Opened") + ylab("count") + labs(fill =
                                                                                                                                                                        "Performance Tag")

#we see that the trades which are open 4 times in last 6 months tends to default more,

ggplot(
  Master_Data,
  aes(
    x = Master_Data$No.of.trades.opened.in.last.6.months,
    fill = factor(Master_Data$Performance.Tag)
  )
) + geom_bar() + ggtitle("Distribution of No of trades opened in last 6 months over Performance Tag") + xlab("Trades Opened") + ylab("count") + labs(fill =
                                                                                                                                                       "Performance Tag")

#we also see that the data is slightly skewed towards right, so we have to make some sort of transformation to make it normal, so we will be performing natural log transformation

log_t = function(x) {
  ifelse((x) <= 0, 0, log((x)))
}

Master_Data$No.of.trades.opened.in.last.6.months <- log_t(Master_Data$No.of.trades.opened.in.last.6.months)

#next, Education

ggplot(Master_Data, aes(
  x = Master_Data$Education,
  fill = factor(Master_Data$Performance.Tag)
)) + geom_bar(position = "fill") + ggtitle("Distribution of Education over Performance Tag") + xlab("Education") + ylab("count") + labs(fill =
                                                                                                                                          "Performance Tag")

#we see that approximately 7% of the Others in Education are at default than rest

#next, No.of.trades.opened.in.last.12.months

ggplot(
  Master_Data,
  aes(
    x = Master_Data$No.of.trades.opened.in.last.12.months,
    fill = factor(Master_Data$Performance.Tag)
  )
) + geom_bar() + ggtitle("Distribution of No of trades opened in last 12 months over Performance Tag") + xlab("No of trades opened in last 12 months") + ylab("count") + labs(fill =
                                                                                                                                                                                "Performance Tag")

#we also see that the data is slightly skewed towards right, 

Master_Data$No.of.trades.opened.in.last.12.months <- log_t(Master_Data$No.of.trades.opened.in.last.12.months)

#now we will again check the distribution

ggplot(Master_Data,
       aes(x = Master_Data$No.of.trades.opened.in.last.12.months)) + geom_bar() + 
        ggtitle("Distribution of No of trades opened in last 12 months over Performance Tag") + 
          xlab("No of trades opened in last 12 months") + ylab("count") + labs(fill ="Performance Tag")

#now we will check the summary

summary(Master_Data)

#we will be using boxplot to check the outliers

boxplot.stats(Master_Data$Age)$out

#we see that the outliers lies before the first quartile, so we will use the quatile function to see sudden aprupt movement in data

quantile(Master_Data$Age,seq(0,1,0.01))

#we see that there is sudden increase in the age from 0% to 1% so we will cap the age at 1% which is 27

Master_Data$Age[which(Master_Data$Age < 27)] <- 27

#now we will again check the distribution of the age variable

ggplot(Master_Data,aes(x=Master_Data$Age)) + geom_density(col="black",fill = "grey") + ggtitle("Distribution of Age") + xlab("Age") + ylab("Density")

#we see that the distribution of age is a normal distribution, since both mean and median are 45.

#now we will check for outliers in income

boxplot.stats(Master_Data$Income)$out

quantile(Master_Data$Income,seq(0,1,0.01))

#though we see that the boxplot doesnot show any outliers, but we know that income cannot be negative, so we will impute negative value
#with 0

Master_Data$Income[which(Master_Data$Income < 0)] <- 0

#now we will check the distribution of income.

ggplot(Master_Data,aes(x=Master_Data$Income)) + geom_density(col = "red",fill = "pink") + ggtitle("Distribution of Income") + xlab("Income") + ylab("Density")

#we see that the distribution of Income is a normal distribution, since both mean and median are 27.

#next no of No of months in current residence

boxplot.stats(Master_Data$No.of.months.in.current.residence)$out

quantile(Master_Data$No.of.months.in.current.residence,seq(0,1,0.01))

#we don't see any outliers in No of months in current residence,

#we will now check its distribution

ggplot(Master_Data,aes(x = Master_Data$No.of.months.in.current.residence)) +
        geom_density(col ="black", fill = "violet") +
          ggtitle("Distribution of No of months in current residence") + xlab("Months") + ylab("Density")

#we see that the "Distribution of No of months in current residence" is highly right skewed, so we have to make some sort of transformation to
#to make it normal, so we will be performing natural log transformation

ggplot(Master_Data, aes(x = log_t(
  Master_Data$No.of.months.in.current.residence
))) + geom_density(col = "black", fill = "slategray1") + ggtitle("Distribution of No of months in current residence") + xlab("Months") + ylab("Density")

#we see that the distribution of No.of.months.in.current.residence is a normal distribution, since both mean and median are 2 approx.

#so we will transform the variable to log

Master_Data$No.of.months.in.current.residence <- log_t(Master_Data$No.of.months.in.current.residence)

#next is No of months in current company

boxplot.stats(Master_Data$No.of.months.in.current.company)$out

quantile(Master_Data$No.of.months.in.current.company,seq(0,1,0.01))

#we see that there is sudden jump from 99% to 100% so we will cap our value at99%

Master_Data$No.of.months.in.current.company[which(Master_Data$No.of.months.in.current.company > 74)] <- 74

#now we will check its distribution

ggplot(Master_Data,aes(x=Master_Data$No.of.months.in.current.company)) + geom_density(fill= "cyan",col = "black") +
      ggtitle("Distribution of Months in current company") + xlab("Months") + ylab("Density")

#we see that the distribution of No.of.months.in.current.company is a normal distribution, since both mean and median are 34 approx.

#now we will check for outliers in credit data "Avgas.CC.Utilization.in.last.12.months"

boxplot.stats(Master_Data$Avgas.CC.Utilization.in.last.12.months)$out

quantile(Master_Data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01))

#we see that there is a suddem jump after 94%, so we will cap our value at94% which is 91

Master_Data$Avgas.CC.Utilization.in.last.12.months[which(Master_Data$Avgas.CC.Utilization.in.last.12.months > 91)] <- 91

#now we will check its distribution

ggplot(Master_Data,
       aes(x = Master_Data$Avgas.CC.Utilization.in.last.12.months)) +
        geom_density(na.rm = TRUE, col = "red", fill = "honeydew1") +
        ggtitle("Distribution of Credit Card Utilization") + xlab("No Of Utilization") + ylab("Density")

#we see that the distribution is skewed towards right, so we will perform log transformation of the utilization variable

#now we will check the distribution after the transformation

ggplot(Master_Data, aes(x = log_t(
  Master_Data$Avgas.CC.Utilization.in.last.12.months
))) + geom_density(na.rm = TRUE, col = "red", fill = "honeydew1") + ggtitle("Distribution of Credit Card Utilization") + xlab("No Of Utilization") + ylab("Density")

#we see that the distribution of Avgas.CC.Utilization.in.last.12.months is a normal distribution after log transformation, since both mean and median are 3 approx.

#so we will convert the variable Avgas.CC.Utilization.in.last.12.months into natural log

Master_Data$Avgas.CC.Utilization.in.last.12.months <- log_t(Master_Data$Avgas.CC.Utilization.in.last.12.months)

#next, Outstanding Balance

boxplot(Master_Data$Outstanding.Balance)

#we don't see any outliers in the Outstanding
#now we will check its distribution

ggplot(Master_Data,
       aes(
         x = Master_Data$Outstanding.Balance,
         fill = factor(Master_Data$Performance.Tag)
       )) + geom_density(col = "blue", position = "fill") +
        ggtitle("Distribution of Outstanding Balance over good and bad") + xlab("Outstanding Balance") + ylab("Density") + labs(fill ="Performance Tag")

#we see that the data is highly skewed towards right, we will perform log reansformation

log10_t = function(x) {
  ifelse((x) <= 0, 0, log10((x)))
}

ggplot(Master_Data, aes(
  x = log10_t(Master_Data$Outstanding.Balance),
  fill = factor(Master_Data$Performance.Tag)
)) + geom_histogram(col = "blue", na.rm = TRUE) + ggtitle("Distribution of Outstanding Balance over good and bad") + xlab("Outstanding Balance") + ylab("Density") + labs(fill =
                                                                                                                                                                            "Performance Tag")

#so we see that the distribution is now a normal distribution since both mean and median are 5 approx,
#so we will convert outstanding balance to log10

Master_Data$Outstanding.Balance <- log10_t(Master_Data$Outstanding.Balance)

#next, No.of.PL.trades.opened.in.last.12.months

boxplot((Master_Data$No.of.PL.trades.opened.in.last.12.months))

ggplot(Master_Data,
       aes(x = Master_Data$No.of.PL.trades.opened.in.last.12.months)) +
        geom_density(col ="blue", fill = "yellow") +
        ggtitle("Distribution of No of PL trades opened in last 12 months") +
        xlab("No of PL trades opened in last 12 months") + ylab("Frequency") + labs(fill ="Performance Tag")

#we see that the distribution is right skewed, so we will use log transformation

Master_Data$No.of.PL.trades.opened.in.last.12.months <- log_t(Master_Data$No.of.PL.trades.opened.in.last.12.months)

boxplot(Master_Data$No.of.PL.trades.opened.in.last.12.months)

#next, No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

boxplot((Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))

ggplot(
  Master_Data,
  aes(
    x = Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
    fill = factor(Master_Data$Performance.Tag)
  )
) + geom_histogram(col = "blue", na.rm = TRUE) + ggtitle("Distribution of No of Inquiries in last 6 months excluding home auto loans over good and bad
") + xlab("No of Inquiries in last 6 months excluding home auto loans") + ylab("Density") + labs(fill =
                                                                                                   "Performance Tag")

#we see that the distribution is right skewed, so we will use log transformation

Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- log_t(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

boxplot(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

#next, No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

boxplot(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.) + title("Outliers Detection Inquiries.in.last.12.months",ylab = "No Of Inquiries") 

ggplot(
  Master_Data,
  aes(
    x = Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
    fill = factor(Master_Data$Performance.Tag)
  )
) + geom_histogram(col = "blue", na.rm = TRUE) + ggtitle(
  "Distribution of No of Inquiries in last 12 months excluding home auto loans over good and bad"
) + xlab("No of Inquiries in last 12 months excluding home auto loans") + ylab("Density") + labs(fill =
                                                                                                   "Performance Tag")

#we see that the distribution is right skewed, so we will use log transformation

Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- log_t(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

boxplot(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

#next, Total.No.of.Trades

boxplot(Master_Data$Total.No.of.Trades)

ggplot(Master_Data,
       aes(
         x = Master_Data$Total.No.of.Trades,
         fill = factor(Master_Data$Performance.Tag)
       )) + geom_density(col = "blue", position = "fill") + ggtitle("Distribution of Total No of Trades over good and bad") + xlab("Total No of Trades") + ylab("Density") + labs(fill =
                                                                                                                                                                                    "Performance Tag")

#we see that the distribution is right skewed, so we will use log transformation

Master_Data$Total.No.of.Trades <- log_t(Master_Data$Total.No.of.Trades)

boxplot(Master_Data$Total.No.of.Trades)

#now we need to bin the column into groups, so we will information package to create a information table and then use the binning function from
#AtConP to convert the data to groups from the information table

#so the columns which we required to bin, we will create a new DF

bin_data <- Master_Data[,c(2,3,6:8,11:16,20,24,27:28)]

str(bin_data)

#now we will use the above data to create a information table

IV1 <- create_infotables(data = bin_data,y = "Performance.Tag",parallel = TRUE)

#now since inforamtion table is created we will extract the binning values from the information table and replacwe it with the original value using
#package AtConP DF.Replace.Bin

cat_data <- DF.Replace.Bin(bin_data,IV1,"Performance.Tag")

#now we will remove the bin data variable from the master data

Master_Data <- Master_Data[,-c(2,3,6:8,11:16,20,24,27:28)]

#now we will cbind.data.frame the cat_data and master data

Master_Data <- cbind.data.frame(Master_Data,cat_data)

#Now we need to check outliers in the categorical variable through by
#implementing the AVF algorithm for outlier detection in categorical data (A. Koufakou, E.G. Ortiz, et al http://www.enriquegortiz.com/publications/outlierDetection_ictai07.pdf) in R.
#the algorithm is in my github library which I have already loaded in the beginning.
#before implementing the algorithm we need to convert all the categorical variable into factor variable

str(Master_Data)

Master_Data <- df.factor(Master_Data)

str(Master_Data)

#now we will calculate the AVF score

Master_AVF <- df.AVF(Master_Data)

#we will now check the distribution of AVF score, which is slightly left skewed, so we will perform log transformation.

ggplot(Master_AVF,aes(x=Master_AVF$AVF_Score)) + geom_density()

boxplot(Master_AVF$AVF_Score)

#we see that there is no outliers in the categorical variable so we are good

#now we need to convert the categorical variable to dummy , we will be using df.matrix() from package AtConP, since it creates K-1 dummy variable and
#return a data frame with both continuous and dummy variable 

model_data <- df.matrix(Master_Data)

#now lets check the model data

str(model_data)

#now we need to first convert our performance var to factor

model_data$Performance.Tag <- as.factor(model_data$Performance.Tag)

master_woe$Performance.Tag <- as.factor(master_woe$Performance.Tag)

#now we need to Identifying Correlated Predictors, we will use caret package.
#Given a correlation matrix, the findCorrelation function uses the following algorithm to flag predictors for removal:

#we need to first remove our performance variable from the data 

predictors <- model_data[-14]

descrCor <- cor(predictors)

summary(descrCor[upper.tri(descrCor)])

#we see that the highest correlation is 93%, so we will remove the descriptors with absolute correlations above 0.75

#first we will filter the variable with correlations above 0.75 and save it in highlyCorDescr

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)

#now we will removve the highly correlated variable from the total variable

filteredDescr <- predictors[,-highlyCorDescr]

descrCor2 <- cor(filteredDescr)

summary(descrCor2[upper.tri(descrCor2)])

#so after removing the correlated variable we get max correlation of 72%

#now we will cbind.data.frame the performance tag with the filteredDescr

com_data <- cbind.data.frame(filteredDescr,model_data[14])

#now we need to check our data for Linear Dependencies
#The function findLinearCombos uses the QR decomposition of a matrix to enumerate sets of linear combinations (if they exist).

#first we need to create a data frame without dependent variable

ltfrDesign <- com_data[-32]

#findLinearCombos will also return a vector of column positions can be removed to eliminate the linear dependencies:

comboInfo <- findLinearCombos(ltfrDesign)

comboInfo

#so we don't see any linear dependencies in our data set



##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------Modelling------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

seed <- 1809

#since all the variable in our model data are now numeric type we are ready to implement any machine learning algorithm on it.

#since we need to model demographic data seperately we will seperate demographic data from both model_data and master_woe

demo_data <- model_data[,c(1:5,15,17:26,14)]

demo_woe <- master_woe[,c(1:10,28)]

#we need to first check the ratio of good and bad customers

summary(model_data$Performance.Tag)

#we see that the data has only 4.21% of the population as the bad customer, which means the data is baiyes towards good customers

#now we will split our data Based on the Outcome

#we will seperate our dependent variable first

demo_data_y <- demo_data[, "Performance.Tag"]

demo_woe_y <- demo_woe[, "Performance.Tag"]

master_woe_y <- master_woe[, "Performance.Tag"]

com_data_y <- com_data[, "Performance.Tag"]

#now we will create predictors data for all the above data sets

demo_data_p <- demo_data[-17]

demo_woe_p <- demo_woe[-11]

master_woe_p <- master_woe[-28]

com_data_p <- com_data[-32]

#now we need to Standardize our data by Combining the scale and center transforms will standardize your data.
#Attributes will have a mean value of 0 and a standard deviation of 1.

demo_data_p <- preProcess(demo_data[-17], method=c("center", "scale"))

demo_woe_p <- preProcess(demo_woe[-11], method=c("center", "scale"))

master_woe_p <- preProcess(master_woe[-28], method=c("center", "scale"))

com_data_p <- preProcess(com_data[-32], method=c("center", "scale"))

#now we will transform the dataset using the parameters

demo_data_p <- predict(demo_data_p, demo_data[-17])

demo_woe_p <- predict(demo_woe_p, demo_woe[-11])

master_woe_p <- predict(master_woe_p, master_woe[-28])

com_data_p <- predict(com_data_p, com_data[-32])

#now we will combine the predictor and dependent variable together

demo_data_c <- cbind.data.frame(demo_data_p,demo_data_y)

demo_woe_c <- cbind.data.frame(demo_woe_p,demo_woe_y)

master_woe_c <- cbind.data.frame(master_woe_p,master_woe_y)

com_data_c <- cbind.data.frame(com_data_p,com_data_y)

#naming the dependent variable

colnames(demo_data_c)[17] <- "Performance.Tag"

colnames(demo_woe_c)[11] <- "Performance.Tag"

colnames(master_woe_c)[28] <- "Performance.Tag"

colnames(com_data_c)[32] <- "Performance.Tag"

#now we will create a training set for each data table

set.seed(seed)

demo_data_t <- createDataPartition(demo_data_y, p = 0.8, list = FALSE)

demo_woe_t <- createDataPartition(demo_woe_y, p = 0.8, list = FALSE)

master_woe_t <- createDataPartition(master_woe_y, p = 0.8, list = FALSE)

com_data_t <- createDataPartition(com_data_y, p = 0.8, list = FALSE)

#now we will create training and test data sets first 

demo_data_train <- demo_data_c[demo_data_t, ]

demo_data_test <- demo_data_c[-demo_data_t, ]

demo_woe_train <- demo_woe_c[demo_woe_t,]

demo_woe_test <- demo_woe_c[-demo_woe_t,]

master_woe_train <- master_woe_c[master_woe_t,]

master_woe_test <- master_woe_c[-master_woe_t,]

com_data_train <- com_data_c[com_data_t,]

com_data_test <- com_data_c[-com_data_t,]

##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------Training------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

set.seed(seed)

str(demo_data_train)

##now first we will create a model for our demographic data

#Let's check the severity of imbalance in demo_data_train data set:

#check table

table(demo_data_train$Performance.Tag)

#check classes distribution

prop.table(table(demo_data_train$Performance.Tag))

#As we see, this data set contains only 4% of positive cases and 96% of negative cases. This is a severely imbalanced data set. So, how badly
#can this affect our prediction accuracy ? Let's build a model on this data. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(Performance.Tag ~ . , data = demo_data_train, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.0072 tells that only 0.72 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + `Profession:Bin:SE`, family = binomial(logit), 
               data = demo_data_train)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.007 tells that only 0.7 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so all the coefficient are within 2 vif no need to remove any more variable
#Profession:bin:se is insignificant so we will remove it

model_3 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company , family = binomial(logit), 
               data = demo_data_train)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( model_3, newdata = demo_data_test[-17] , type = "response" )

accuracy.meas(demo_data_test$Performance.Tag, pred.demo)

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(demo_data_test$Performance.Tag, pred.demo, plotit = F)

#AUC = 0.589 is a terribly low score. Therefore, it is necessary to balanced data before applying a machine learning algorithm. In this case,
#the algorithm gets biased toward the majority class and fails to map minority class.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(model_3, newdata = demo_data_test[-17], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, demo_data_test$Performance.Tag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, demo_data_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.03969697, "1", "0"))

conf_final <- confusionMatrix(predicted_response, demo_data_test$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#so we are getting a accuracy of 46% , Sensitivity of 68% and Specificity of 46% through logistic regression on demographic data sets.

##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------Training Demo WOE----------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

set.seed(seed)

##now first we will create a model for our demographic data

#Let's check the severity of imbalance in demo_woe_train data set:

#check table

table(demo_woe_train$Performance.Tag)

#check classes distribution

prop.table(table(demo_woe_train$Performance.Tag))

#As we see, this data set contains only 4% of positive cases and 96% of negative cases. This is a severely imbalanced data set. So, how badly
#can this affect our prediction accuracy ? Let's build a model on this data. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(Performance.Tag ~ . , data = demo_woe_train, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.0163 tells that only 1.63 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = Performance.Tag ~ `Age:WOE` + `No.of.dependents:WOE` + 
                 `Income:WOE` + `Profession:WOE` + `No.of.months.in.current.residence:WOE` + 
                 `No.of.months.in.current.company:WOE`, family = binomial(logit), 
               data = demo_woe_train)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.016 tells that only 1.6 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so all the coefficient are within 2 vif no need to remove any more variable for VIF, but we see that there are insignificant variable,so we will
#remove those variable

#Will remove `Age:WOE`

model_3 <- glm(formula = Performance.Tag ~ `No.of.dependents:WOE` + 
                 `Income:WOE` + `Profession:WOE` + `No.of.months.in.current.residence:WOE` + 
                 `No.of.months.in.current.company:WOE`, family = binomial(logit), 
               data = demo_woe_train)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see A pseudo R square of 0.0159 decreases from 0.016 tells that only 1.6 percent of the variance is explained.

#will remove `No.of.dependents:WOE` because of insignificance

model_4 <- glm(formula = Performance.Tag ~  
                 `Income:WOE` + `Profession:WOE` + `No.of.months.in.current.residence:WOE` + 
                 `No.of.months.in.current.company:WOE`, family = binomial(logit), 
               data = demo_woe_train)

summary(model_4)

summary_glm <- summary(model_4)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see A pseudo R square of 0.0157 decreases from 0.0159 tells that only 1.6 percent of the variance is explained.

#will remove `Profession:WOE` because of insignificance

model_5 <- glm(formula = Performance.Tag ~  
                 `Income:WOE` + `No.of.months.in.current.residence:WOE` + 
                 `No.of.months.in.current.company:WOE`, family = binomial(logit), 
               data = demo_woe_train)

summary(model_5)

summary_glm <- summary(model_5)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see A pseudo R square of 0.0154 decreases from 0.0157 tells that only 1.5 percent of the variance is explained.
#since now all our variable are significant we will not remove any variable.

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( model_5, newdata = demo_woe_test[-17] , type = "response" )

accuracy.meas(demo_woe_test$Performance.Tag, pred.demo)

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(demo_woe_test$Performance.Tag, pred.demo, plotit = F)

#AUC = 0.596 is a terribly low score. Therefore, it is necessary to balanced data before applying a machine learning algorithm. In this case,
#the algorithm gets biased toward the majority class and fails to map minority class.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(model_5, newdata = demo_woe_test[-17], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, demo_woe_test$Performance.Tag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, demo_woe_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, demo_woe_test$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#so we are getting a accuracy of 54% , Sensitivity of 60% and Specificity of 53% through logistic regression on woe demographic data sets.


##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------Training master_woe_-------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

set.seed(seed)

str(master_woe_train)

##now first we will create a model for our demographic data

#Let's check the severity of imbalance in master_woe_train data set:

#check table

table(master_woe_train$Performance.Tag)

#check classes distribution

prop.table(table(master_woe_train$Performance.Tag))

#As we see, this data set contains only 4% of positive cases and 96% of negative cases. This is a severely imbalanced data set. So, how badly
#can this affect our prediction accuracy ? Let's build a model on this data. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(Performance.Tag ~ . , data = master_woe_train, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.0463 tells that only 4.63 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = Performance.Tag ~ `Age:WOE` + `Income:WOE` + `No.of.months.in.current.company:WOE` + 
                 `No.of.times.30.DPD.or.worse.in.last.6.months:WOE` + `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + 
                 `Avgas.CC.Utilization.in.last.12.months:WOE` + `No.of.trades.opened.in.last.12.months:WOE` + 
                 `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                 `Outstanding.Balance:WOE` + `Total.No.of.Trades:WOE`, family = binomial(logit), 
               data = master_woe_train)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.0458 tells that only 4.5 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so we see that `No.of.trades.opened.in.last.12.months:WOE` has a VIF score of 6.69 so we will remove it from our model

model_3 <- glm(formula = Performance.Tag ~ `Age:WOE` + `Income:WOE` + `No.of.months.in.current.company:WOE` + 
                 `No.of.times.30.DPD.or.worse.in.last.6.months:WOE` + `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + 
                 `Avgas.CC.Utilization.in.last.12.months:WOE` +  
                 `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                 `Outstanding.Balance:WOE` + `Total.No.of.Trades:WOE`, family = binomial(logit), 
               data = master_woe_train)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

vif(model_3)

#so we see that `No.of.times.30.DPD.or.worse.in.last.6.months:WOE` has a VIF score of 5.93 so we will remove it from our model

model_4 <- glm(formula = Performance.Tag ~ `Age:WOE` + `Income:WOE` + `No.of.months.in.current.company:WOE` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + 
                 `Avgas.CC.Utilization.in.last.12.months:WOE` +  
                 `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                 `Outstanding.Balance:WOE` + `Total.No.of.Trades:WOE`, family = binomial(logit), 
               data = master_woe_train)

summary(model_4)

summary_glm <- summary(model_4)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

vif(model_4)

#so we see that `Total.No.of.Trades:WOE` has a VIF score of 2.32 so we will remove it from our model

model_5 <- glm(formula = Performance.Tag ~ `Age:WOE` + `Income:WOE` + `No.of.months.in.current.company:WOE` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + 
                 `Avgas.CC.Utilization.in.last.12.months:WOE` +  
                 `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                 `Outstanding.Balance:WOE`, family = binomial(logit), 
               data = master_woe_train)

summary(model_5)

vif(model_5)

#so all the variable has vif less than 2 so we are good

summary_glm <- summary(model_5)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#also the variance explained by the model does not change, lets remove the variable using significance
#we see that variable `Age:WOE` has very low significance, so we will remove it

model_6 <- glm(formula = Performance.Tag ~ `Income:WOE` + `No.of.months.in.current.company:WOE` + 
                     `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + `Avgas.CC.Utilization.in.last.12.months:WOE` + 
                     `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                     `Outstanding.Balance:WOE`, family = binomial(logit), data = master_woe_train)

summary(model_6)

#we see that variable `No.of.months.in.current.company:WOE` has very low significance, so we will remove it

model_7 <- glm(formula = Performance.Tag ~ `Income:WOE` +  
                 `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + `Avgas.CC.Utilization.in.last.12.months:WOE` + 
                 `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                 `Outstanding.Balance:WOE`, family = binomial(logit), data = master_woe_train)

summary(model_7)

#we see that variable `Income:WOE`  has very low significance, so we will remove it

model_8 <- glm(formula = Performance.Tag ~   
                 `No.of.times.30.DPD.or.worse.in.last.12.months:WOE` + `Avgas.CC.Utilization.in.last.12.months:WOE` + 
                 `No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.:WOE` + 
                 `Outstanding.Balance:WOE`, family = binomial(logit), data = master_woe_train)

summary(model_8)

#now all our variable are significance, lets also check the variance explained by the model

summary_glm <- summary(model_8)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#so the variance explained by the model is 0.0445 which is very low.

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( model_8, newdata = master_woe_test[-28] , type = "response" )

accuracy.meas(master_woe_test$Performance.Tag, pred.demo)

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(master_woe_test$Performance.Tag, pred.demo, plotit = F)

#AUC = 0.688 is a terribly low score. Therefore, it is necessary to balanced data before applying a machine learning algorithm. In this case,
#the algorithm gets biased toward the majority class and fails to map minority class.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(model_8, newdata = master_woe_test[-28], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, master_woe_test$Performance.Tag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, master_woe_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, master_woe_test$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(master_woe_test$Performance.Tag, predicted_response, plotit = T)


#AUC is 63% and we are getting a accuracy of 66% , Sensitivity of 60% and Specificity of 66% through logistic regression on master woe data sets.

##--------------------------------------------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------Training master_data-------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

set.seed(seed)

##now first we will create a model for our demographic data

#Let's check the severity of imbalance in com_data_train data set:

#check table

table(com_data_train$Performance.Tag)

#check classes distribution

prop.table(table(com_data_train$Performance.Tag))

#As we see, this data set contains only 4% of positive cases and 96% of negative cases. This is a severely imbalanced data set. So, how badly
#can this affect our prediction accuracy ? Let's build a model on this data. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(Performance.Tag ~ . , data = com_data_train, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.0422 tells that only 4.2 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Outstanding.Balance + Total.No.of.Trades + `Education:Bin:Others` + 
                 `Type.of.residence:Bin:Others` + `No.of.times.90.DPD.or.worse.in.last.6.months:Bin:[0,0]` + 
                 `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]` + 
                 `Presence.of.open.home.loan:Bin:[0,0]`, family = binomial(logit), 
               data = com_data_train)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.0416 tells that only 4 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so we see that `No.of.times.90.DPD.or.worse.in.last.6.months:Bin:[0,0]` has a VIF score > 2, so we will remove the variable.

#will remove `No.of.times.90.DPD.or.worse.in.last.6.months:Bin:[0,0]` 

model_3 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Outstanding.Balance + Total.No.of.Trades + `Education:Bin:Others` + 
                 `Type.of.residence:Bin:Others` +  
                 `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]` + 
                 `Presence.of.open.home.loan:Bin:[0,0]`, family = binomial(logit), 
               data = com_data_train)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

vif(model_3)

#we see that Outstanding.Balance has a VIF score > 2, so we will remove the variable.

#will remove Outstanding.Balance

model_4 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Total.No.of.Trades + `Education:Bin:Others` + 
                 `Type.of.residence:Bin:Others` +  
                 `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]` + 
                 `Presence.of.open.home.loan:Bin:[0,0]`, family = binomial(logit), 
               data = com_data_train)

summary(model_4)

summary_glm <- summary(model_4)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

vif(model_4)

#we see that no variable  has a VIF score > 2, so we will remove the variable base on significance

#will remove `Presence.of.open.home.loan:Bin:[0,0]`


model_5 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Total.No.of.Trades + `Education:Bin:Others` + 
                 `Type.of.residence:Bin:Others` +  
                 `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`, family = binomial(logit), 
               data = com_data_train)

summary(model_5)

vif(model_5)

summary_glm <- summary(model_5)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing `Presence.of.open.home.loan:Bin:[0,0]`
#the variance explained by the model is same at 0.0412 

#will remove `Type.of.residence:Bin:Others`


model_6 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Total.No.of.Trades + `Education:Bin:Others` + 
                 `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`, family = binomial(logit), 
               data = com_data_train)

summary(model_6)

vif(model_6)

summary_glm <- summary(model_6)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing `Type.of.residence:Bin:Others`
#the variance explained by the model is decrease from 0.0412 to  0.041, but since the variable is insignificant we have to remove it

#will remove No.of.months.in.current.residence


model_7 <- glm(formula = Performance.Tag ~  
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Total.No.of.Trades + `Education:Bin:Others` + 
                 `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`, family = binomial(logit), 
               data = com_data_train)

summary(model_7)

vif(model_7)

summary_glm <- summary(model_7)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing No.of.months.in.current.residence
#the variance explained by the model is decrease from 0.0412 to  0.0408, but since the variable is insignificant we have to remove it

#will remove `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]`


model_8 <- glm(formula = Performance.Tag ~  
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Total.No.of.Trades + `Education:Bin:Others` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`, family = binomial(logit), 
               data = com_data_train)

summary(model_8)

vif(model_8)

summary_glm <- summary(model_8)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing `No.of.times.90.DPD.or.worse.in.last.12.months:Bin:[1,1]`
#the variance explained by the model is decrease from 0.0408 to  0.0406, but since the variable is insignificant we have to remove it

#will remove `Education:Bin:Others`


model_9 <- glm(formula = Performance.Tag ~  
                 No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                 Total.No.of.Trades +  
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                 `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                 `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`, family = binomial(logit), 
               data = com_data_train)

summary(model_9)

vif(model_9)

summary_glm <- summary(model_9)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing `Education:Bin:Others`
#the variance explained by the model is decrease from 0.0406 to  0.0404, but since the variable is insignificant we have to remove it

#will remove No.of.months.in.current.company

model_10 <- glm(formula = Performance.Tag ~  
                  Avgas.CC.Utilization.in.last.12.months + 
                  Total.No.of.Trades +  
                  `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                  `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` + 
                  `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`, family = binomial(logit), 
                data = com_data_train)

summary(model_10)

vif(model_10)

summary_glm <- summary(model_10)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing No.of.months.in.current.company
#the variance explained by the model is decrease from 0.0404 to  0.0402, but since the variable is insignificant we have to remove it

#will remove `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`

model_11 <- glm(formula = Performance.Tag ~  
                  Avgas.CC.Utilization.in.last.12.months + 
                  Total.No.of.Trades +  
                  `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                  `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` 
                  , family = binomial(logit), 
                data = com_data_train)

summary(model_11)

vif(model_11)

summary_glm <- summary(model_11)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing `No.of.times.30.DPD.or.worse.in.last.12.months:Bin:[1,2]`
#the variance explained by the model is decrease from 0.0402 to  0.0399, but since the variable is insignificant we have to remove it

#so are final model is

final_model <- glm(formula = Performance.Tag ~  
                     Avgas.CC.Utilization.in.last.12.months + 
                     Total.No.of.Trades +  
                     `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[0,0]` + 
                     `No.of.times.60.DPD.or.worse.in.last.12.months:Bin:[1,1]` 
                   , family = binomial(logit), 
                   data = com_data_train)

summary(final_model)

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( final_model, newdata = com_data_test[-32] , type = "response" )

accuracy.meas(com_data_test$Performance.Tag, pred.demo)

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(com_data_test$Performance.Tag, pred.demo, plotit = T)

#AUC = 0.666 is a terribly low score. Therefore, it is necessary to balanced data before applying a machine learning algorithm. In this case,
#the algorithm gets biased toward the majority class and fails to map minority class.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model, newdata = com_data_test[-32], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, com_data_test$Performance.Tag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, com_data_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, com_data_test$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(com_data_test$Performance.Tag, predicted_response, plotit = T)

#AUC is 61% and we are getting a accuracy of 67% , Sensitivity of 55% and Specificity of 67% through logistic regression on master data sets.

##--------------------------------------------------------------------------------------------------------------------------------------##
##--------------------------------------------------------Balanced Training master_data-------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

set.seed(seed)

#As we know, the data set contains only 4% of positive cases and 96% of negative cases. which is a severely imbalanced data set.
#after training the data and checking the performance we are only getting a 67% highest accuracy, So now we are require to use some technique
#to balance our data sets.

#ROSE and DMwR helps us to perform sampling strategies quickly. We'll work on a problem of binary classification.

#ROSE (Random Over Sampling Examples) package helps us to generate artificial data based on sampling methods and smoothed bootstrap approach.

#As we see, data set contains only 2% of positive cases and 98% of negative cases. This is a severely imbalanced data set. So, how badly
#can this affect our prediction accuracy ? Let's build a model on this data. I'll be using decision tree algorithm for modeling purpose.

#first we need to create a copy of our original data sets.

com_data_train_rose <- com_data_train

com_data_test_rose <- com_data_test

master_woe_train_rose <- master_woe_train

master_woe_test_rose <- master_woe_test

demo_woe_train_rose <- demo_woe_train

demo_woe_test_rose <- demo_woe_test

#we need to remove any special format from column.

colnames(com_data_train_rose)[1:32] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(com_data_train_rose)[1:32]))

colnames(com_data_test_rose)[1:32] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(com_data_test_rose)[1:32]))

colnames(master_woe_train_rose)[1:28] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(master_woe_train_rose)[1:28]))

colnames(master_woe_test_rose)[1:28] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(master_woe_test_rose)[1:28]))

colnames(demo_woe_train_rose)[1:11] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(demo_woe_train_rose)[1:11]))

colnames(demo_woe_test_rose)[1:11] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(demo_woe_test_rose)[1:11]))

#now we will ROSE package to generate data

#ROSE helps us to generate data synthetically as well. The data generated using ROSE is considered to provide better estimate of original data.

com_data_train_rose <- ROSE(PerformanceTag ~ ., data = com_data_train_rose, seed = 1)$data

master_woe_train_rose <- ROSE(PerformanceTag ~ ., data = master_woe_train_rose, seed = 1)$data

demo_woe_train_rose <- ROSE(PerformanceTag ~ ., data = demo_woe_train_rose, seed = 1)$data

table(com_data_train_rose$PerformanceTag)

table(master_woe_train_rose$PerformanceTag)

table(demo_woe_train_rose$PerformanceTag)

#now since all our data are balanced we will now use logistic regression to chekc the accuracy

##--------------------------------------------------------------------------------------------------------------------------------------##
##--------------------------------------Training on Balanced master_woe_train_rose data-------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

set.seed(seed)

#First we will train master_woe_train_rose data sets. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(PerformanceTag ~ . , data = master_woe_train_rose, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.07 tells that only 7 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = PerformanceTag ~ AgeWOE + GenderWOE + MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentcompanyWOE + Nooftimes90DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes60DPDorworseinlast12monthsWOE + Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast6monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.0699 tells that only 7 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so we see that all the variables are with in a VIF score of less than or approx 2, so we will remove the variable using significance

#will remove GenderWOE

model_3 <- glm(formula = PerformanceTag ~ AgeWOE + MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentcompanyWOE + Nooftimes90DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes60DPDorworseinlast12monthsWOE + Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast6monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

vif(model_3)



#will remove Nooftimes60DPDorworseinlast12monthsWOE

model_4 <- glm(formula = PerformanceTag ~ AgeWOE + MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentcompanyWOE + Nooftimes90DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast6monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_4)

summary_glm <- summary(model_4)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

vif(model_4)



#will remove NoofPLtradesopenedinlast6monthsWOE


model_5 <- glm(formula = PerformanceTag ~ AgeWOE + MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentcompanyWOE + Nooftimes90DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_5)

vif(model_5)

summary_glm <- summary(model_5)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))


#the variance explained by the model is same at 0.0697

#will remove AgeWOE 


model_6 <- glm(formula = PerformanceTag ~ MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentcompanyWOE + Nooftimes90DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_6)

vif(model_6)

summary_glm <- summary(model_6)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))


#will remove TypeofresidenceWOE


model_7 <- glm(formula = PerformanceTag ~ MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE +
                 NoofmonthsincurrentcompanyWOE + Nooftimes90DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_7)

vif(model_7)

summary_glm <- summary(model_7)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing TypeofresidenceWOE
#the variance explained by the model is decrease from 0.0697 to  0.0695, but since the variable is insignificant we have to remove it

#will remove Nooftimes90DPDorworseinlast6monthsWOE


model_8 <- glm(formula = PerformanceTag ~ MaritalStatusatthetimeofapplicationWOE + 
                 NoofdependentsWOE + IncomeWOE + ProfessionWOE +
                 NoofmonthsincurrentcompanyWOE +  
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                 NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = master_woe_train_rose)

summary(model_8)

vif(model_8)

summary_glm <- summary(model_8)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#we see that after removing Nooftimes90DPDorworseinlast6monthsWOE

#the variance explained by the model is decrease from 0.0695 to  0.0694, but since the variable is insignificant we have to remove it

#since now all the variable are significant we will consider model_8 as our final model

#so are final model is

final_model <- glm(formula = PerformanceTag ~ MaritalStatusatthetimeofapplicationWOE + 
                     NoofdependentsWOE + IncomeWOE + ProfessionWOE +
                     NoofmonthsincurrentcompanyWOE +  
                     Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                     Nooftimes30DPDorworseinlast12monthsWOE + 
                     AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                     NoofPLtradesopenedinlast12monthsWOE + 
                     NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                     OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
                   data = master_woe_train_rose)

summary(final_model)

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( final_model, newdata = master_woe_test_rose[-28] , type = "response" )

accuracy.meas(master_woe_test_rose$PerformanceTag, pred.demo)

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(master_woe_test_rose$PerformanceTag, pred.demo, plotit = T)

#AUC = 0.688 is a terribly low score. but it is more than imbalance data.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model, newdata = master_woe_test_rose[-28], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(master_woe_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 64% and we are getting a accuracy of 64% , Sensitivity of 65% and Specificity of 64% through logistic regression on master data sets.

##Model Evaluation C-statistic

#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.
#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(predictions_logit,master_woe_test_rose$PerformanceTag)

#we see that the c-statistic for test data is 6.88, which show very good discriminative power of the model.

##ks-statistic
#now we will evaluate our model using KS-statistic, we will prediction() function from package (ROCR)

#first we will store the prediction object in model_score_test

model_score_test <- prediction(predictions_logit,master_woe_test_rose$PerformanceTag)

#now we will create performance object using performance function

model_perf_test <- performance(model_score_test,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks_test <- max(ks_table_test)

ks_test

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_test_index <- which(ks_table_test == ks_test)

ks_test_index

#now since the row is 109, we will divide it by total number of rows in trainind ds which is 300

ks_test_decile <- ks_test_index/nrow(master_woe_test_rose)

ks_test_decile

#since the ks-statistic is at 0.51, therefore our ks-ststistic lies in the 5th decile.

##--------------------------------------------------------------------------------------------------------------------------------------##
##------------------------------Training on Balanced master_woe_train_rose data through randon forest-----------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

#now we will use random forest to model our training data with 100 trees

library(randomForest)

rforest <- randomForest(PerformanceTag ~ ., master_woe_train_rose,ntree=100)

summary(rforest)

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(rforest, newdata = master_woe_test_rose[-28], type = "prob")[,2]

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(master_woe_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 64% and we are getting a accuracy of 64% , Sensitivity of 64% and Specificity of 64% through random forest on master data sets.

##--------------------------------------------------------------------------------------------------------------------------------------##
##-------------------------------Training on Balanced master_woe_train_rose data through glmnet-----------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

#now we will use glmnet

#now I will use the glmnet package (Friedman, Hastie, and Tibshirani 2008), that fits a generalized linear model via penalized maximum likelihood.
#The algorithm implemented in the package computes the regularization path for the elastic-net penalty over a grid of values for the regularization
#parameter ????. The tuning parameter ???? controls the overall strength of the penalty. A second tuning parameter, called the mixing percentage
#and denoted with ????, represents the elastic-net penalty (Zou and Hastie 2005). This parameter takes value in [0,1][0,1] and bridges the gap between
#the lasso (??=1??=1) and the ridge (??=0??=0) approaches.

#The train() function requires the model formula together with the indication of the model to fit and the grid of tuning parameter values to use.
#In the code below this grid is specified through the tuneGrid argument, while trControl provides the method to use for choosing the optimal values
#of the tuning parameters (in our case, 10-fold cross-validation). Finally, the preProcess argument allows to apply a series of pre-processing
#operations on the predictors (in our case, centering and scaling the predictor values).

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),lambda = seq(.01, .2, length = 20))

glmnet_ctrl <- trainControl(method = "cv", number = 10)

glmnet_fit <- train(PerformanceTag ~ ., data = master_woe_train_rose,method = "glmnet",preProcess = c("center", "scale"),tuneGrid = glmnet_grid,trControl = glmnet_ctrl)

glmnet_fit

trellis.par.set(caretTheme())

plot(glmnet_fit, scales = list(x = list(log = 2)))

pred_classes <- predict(glmnet_fit, newdata = master_woe_test_rose[-28])

table(pred_classes)

pred_probs <- predict(glmnet_fit, newdata = master_woe_test_rose[-28], type = "prob")

head(pred_probs)

#-------------------------
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(glmnet_fit, newdata = master_woe_test_rose[-28], type = "prob")[,2]

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value of 14% for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(master_woe_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 64% and we are getting a accuracy of 64% , Sensitivity of 64% and Specificity of 64% through glmnet on master woe data sets.

##--------------------------------------------------------------------------------------------------------------------------------------##
##---------------------------------Training on Balanced com_data_train_rose data through glmnet-----------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

#now we will see that accuracy on com master data by running glmnet algorithm

#----------------------------------------------------------------------------------------------------------------------------------------------

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),lambda = seq(.01, .2, length = 20))

glmnet_ctrl <- trainControl(method = "cv", number = 10)

glmnet_fit <- train(PerformanceTag ~ ., data = com_data_train_rose,method = "glmnet",preProcess = c("center", "scale"),tuneGrid = glmnet_grid,trControl = glmnet_ctrl)

glmnet_fit

trellis.par.set(caretTheme())

plot(glmnet_fit, scales = list(x = list(log = 2)))

pred_classes <- predict(glmnet_fit, newdata = com_data_test_rose[-32])

table(pred_classes)

pred_probs <- predict(glmnet_fit, newdata = com_data_test_rose[-32], type = "prob")

head(pred_probs)

#-------------------------
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(glmnet_fit, newdata = com_data_test_rose[-32], type = "prob")[,2]

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, com_data_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, com_data_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value of 14% for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, com_data_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(com_data_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 63% and we are getting a accuracy of 65% , Sensitivity of 60% and Specificity of 65% through glmnet on com master data sets.

##--------------------------------------------------------------------------------------------------------------------------------------##
##---------------------------------Training on Balanced demo_woe_train_rose data through logistic regression----------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##


#now we will train demo woe data with balanced data.....

set.seed(seed)

#First we will train master_woe_train_rose data sets. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(PerformanceTag ~ . , data = demo_woe_train_rose, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.0202 tells that only 2 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = PerformanceTag ~ AgeWOE + GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + EducationWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentresidenceWOE + NoofmonthsincurrentcompanyWOE, 
               family = binomial(logit), data = demo_woe_train_rose)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.0202 tells that only 2 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so we see that all the variables are with in a VIF score of less than or approx 2, so we will remove the variable using significance

#will remove GenderWOE

model_3 <- glm(formula = PerformanceTag ~ AgeWOE + NoofdependentsWOE + 
                 IncomeWOE + EducationWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentresidenceWOE + NoofmonthsincurrentcompanyWOE, 
               family = binomial(logit), data = demo_woe_train_rose)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#will remove EducationWOE

model_4 <- glm(formula = PerformanceTag ~ AgeWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE + TypeofresidenceWOE + 
                 NoofmonthsincurrentresidenceWOE + NoofmonthsincurrentcompanyWOE, 
               family = binomial(logit), data = demo_woe_train_rose)

summary(model_4)

summary_glm <- summary(model_4)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#will remove TypeofresidenceWOE


model_5 <- glm(formula = PerformanceTag ~ AgeWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE +  
                 NoofmonthsincurrentresidenceWOE + NoofmonthsincurrentcompanyWOE, 
               family = binomial(logit), data = demo_woe_train_rose)

summary(model_5)

vif(model_5)

summary_glm <- summary(model_5)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))


#the variance explained by the model is same at 0.02

#since now all the variable are significant we will consider model_5 as our final model

#so are final model is

final_model <- glm(formula = PerformanceTag ~ AgeWOE + NoofdependentsWOE + 
                     IncomeWOE + ProfessionWOE +  
                     NoofmonthsincurrentresidenceWOE + NoofmonthsincurrentcompanyWOE, 
                   family = binomial(logit), data = demo_woe_train_rose)

summary(final_model)

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( final_model, newdata = demo_woe_test_rose[-11] , type = "response" )

accuracy.meas(demo_woe_test_rose$PerformanceTag, pred.demo)

#we see that precision is 8.6%, recall is 8.5% and f score is 4.3%

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(demo_woe_test_rose$PerformanceTag, pred.demo, plotit = F)

#AUC = 0.599 is a terribly low score. but it is more than imbalance data.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model, newdata = demo_woe_test_rose[-11], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, demo_woe_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, demo_woe_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, demo_woe_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(demo_woe_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 56% and we are getting a accuracy of 56% , Sensitivity of 56% and Specificity of 57% through logistic regression on demo woe data sets.

##Model Evaluation C-statistic

#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.
#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(predictions_logit,demo_woe_test_rose$PerformanceTag)

#we see that the c-statistic for test data is 5.98, which doesnot show very good discriminative power of the model.

##ks-statistic
#now we will evaluate our model using KS-statistic, we will prediction() function from package (ROCR)

#first we will store the prediction object in model_score_test

model_score_test <- prediction(predictions_logit,demo_woe_test_rose$PerformanceTag)

#now we will create performance object using performance function

model_perf_test <- performance(model_score_test,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks_test <- max(ks_table_test)

ks_test

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_test_index <- which(ks_table_test == ks_test)

ks_test_index

#now since the row is 3742, we will divide it by total number of rows in trainind ds which is 13738

ks_test_decile <- ks_test_index/nrow(demo_woe_test_rose)

ks_test_decile

#since the ks-statistic is at 0.27, therefore our ks-ststistic lies in the 3rd decile, which is good


#since all the data sets are giving an accuracy of some what 60 to 65%, we will now implement PCA to see if we can get any better model.

##--------------------------------------------------------------------------------------------------------------------------------------##
##--------------------------------------------------------PCA Training on Balanced data-------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##

#we will use WOE and complete master data.

#principal component analysis

prin_comp_woe <- prcomp(master_woe_train_rose[-28], scale. = T)

names(prin_comp_woe)

#The prcomp() function results in 5 useful measures:

#1. center and scale refers to respective mean and standard deviation of the variables that are used for normalization prior to implementing PCA

#2. The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector.
#This is the most important measure we should be interested in.

prin_comp_woe$rotation

#This returns 27 principal components loadings. Is that correct ? Absolutely. In a data set, the maximum number of principal component loadings
#is a minimum of (n-1, p). Let's look at first 4 principal components and first 5 rows.

prin_comp_woe$rotation[1:5,1:4]

#3. In order to compute the principal component score vector, we don't need to multiply the loading with data. Rather, the matrix x has the
#principal component score vectors in a 54958 × 27 dimension.

dim(prin_comp_woe$x)

#Let's plot the resultant principal components.

biplot(prin_comp_woe, scale = 0)

#The parameter scale = 0 ensures that arrows are scaled to represent the loadings. To make inference from image above,
#focus on the extreme ends (top, bottom, left, right) of this graph.

#4. The prcomp() function also provides the facility to compute standard deviation of each principal component.
#sdev refers to the standard deviation of principal components.

##compute standard deviation of each principal component

std_dev <- prin_comp_woe$sdev

##compute variance

pr_var <- std_dev^2

#check variance of first 10 components

pr_var[1:10]

#We aim to find the components which explain the maximum variance. This is because, we want to retain as much information as
#possible using these components. So, higher is the explained variance, higher will be the information contained in those components.

#To compute the proportion of variance explained by each component, we simply divide the variance by sum of total variance. This results in:

#proportion of variance explained

prop_varex <- pr_var/sum(pr_var)


prop_varex[1:20]

#This shows that first principal component explains 28.8% variance. Second component explains 8% variance. Third component explains 4% variance
#and so on. So, how do we decide how many components should we select for modeling stage ?

#The answer to this question is provided by a scree plot. A scree plot is used to access components or factors which explains the most of variability
#in the data. It represents values in descending order.

#scree plot

plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#The plot above shows that ~ 15 components explains around 80% variance in the data set. In order words, using PCA we have reduced 27 predictors to
#15 without compromising on explained variance. This is the power of PCA> Let's do a confirmation check, by plotting a cumulative variance plot.
#This will give us a clear picture of number of components.

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#This plot shows that 15 components results in variance close to ~ 80%. Therefore, in this case, we'll select number of components as 15 [PC1 to PC15]
#and proceed to the modeling stage. This completes the steps to implement PCA on train data. For modeling, we'll use these 30 components as predictor
#variables and follow the normal procedures.

#Predictive Modeling with PCA Components

#After we've calculated the principal components on training set, let's now understand the process of predicting on test data using these components.
#The process is simple. Just like we've obtained PCA components on training set, we'll get another bunch of components on testing set.
#Finally, we train the model.

#But, few important points to understand:
  
#  We should not combine the train and test set to obtain PCA components of whole data at once. Because, this would violate the entire assumption of
#generalization since test data would get 'leaked' into the training set. In other words, the test data set would no longer remain 'unseen'.
#Eventually, this will hammer down the generalization capability of the model.
#We should not perform PCA on test and train data sets separately. Because, the resultant vectors from train and test PCAs will have different
#directions ( due to unequal variance). Due to this, we'll end up comparing data registered on different axes. Therefore, the resulting vectors from 
#train and test data should have same axes.

#So, what should we do?

#We should do exactly the same transformation to the test set as we did to training set, including the center and scaling feature.

#add a training set with principal components

train.data <- data.frame(PerformanceTag = master_woe_train_rose$PerformanceTag, prin_comp_woe$x)

##we are interested in first 15 PCAs

train.data <- train.data[,1:16]

#run a glmnet

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),lambda = seq(.01, .2, length = 20))

glmnet_ctrl <- trainControl(method = "cv", number = 10)

glmnet_fit <- train(PerformanceTag ~ ., data = train.data,method = "glmnet",preProcess = c("center", "scale"),tuneGrid = glmnet_grid,trControl = glmnet_ctrl)

glmnet_fit

trellis.par.set(caretTheme())

plot(glmnet_fit, scales = list(x = list(log = 2)))

#transform test into PCA

test.data <- predict(prin_comp_woe, newdata = master_woe_test_rose)

test.data <- as.data.frame(test.data)

#select the first 15 components

test.data <- test.data[,1:15]

##make prediction on test data

predictions_logit <- predict(glmnet_fit, test.data,type = "prob")[,2]

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, master_woe_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(master_woe_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 64% and we are getting a accuracy of 64% , Sensitivity of 64% and Specificity of 64% through PCA glmnet on woe master data sets.


##---------------------------------------------------------Validation Data sets------------------------------------------------------##

##Validation Data sets

str(validation)

#since validation data sets also includes application id, we will first remode id

rejected_Applicant <- validation[,1]

validation <- validation[,-1]

#now we will check to is is there any na values apart from PerformanceTag

sum(is.na(validation))

sapply(validation,function(x) sum(is.na(x)))

#we see that there are 1462 NA values which includes 1 for Education, 1 for Profession, 35 for Avgas.CC.Utilization.in.last.12.months and rest
#for Performance.Tag.

#so we are not bother about the na on Performance.Tag since we are going to predict id but for rest we will use mean, median or mode to impute the
#na values

summary(validation$Education)

#we see that Masters are more frequent so we will replace na with masters

validation$Education[which(is.na(validation$Education))] <- "Masters"

#now for Profession

summary(validation$Profession)

#we see that SAL are more frequent so we will replace na with SAL

validation$Profession[which(is.na(validation$Profession))] <- "SAL"

#now for Avgas.CC.Utilization.in.last.12.months

summary(validation$Avgas.CC.Utilization.in.last.12.months)

#we see that the median is at 51.00 so we will replace it with the median

validation$Avgas.CC.Utilization.in.last.12.months[which(is.na(validation$Avgas.CC.Utilization.in.last.12.months))] <- median(validation$Avgas.CC.Utilization.in.last.12.months,na.rm = TRUE)

#now we will again check to see if there is any na apart from PerformanceTag

sapply(validation,function(x) sum(is.na(x)))

#we see that there is no na values in our validation data sets so we will go ahead and replace the values with the woe values

validation_woe <- DF.Replace.WOE(validation,IV,Dependent = "Performance.Tag")

#now we will transform our data 

#we will seperate our dependent variable first

validation_woe_y <- validation_woe[, "Performance.Tag"]

#now we will create predictors data for all the above data sets

validation_woe_p <- validation_woe[-28]

#now we need to Standardize our data by Combining the scale and center transforms will standardize your data.
#Attributes will have a mean value of 0 and a standard deviation of 1.

validation_woe_p <- preProcess(validation_woe[-28], method=c("center", "scale"))

#now we will transform the dataset using the parameters

validation_woe_p <- predict(validation_woe_p, validation_woe[-28])

#now we will combine the predictor and dependent variable together

validation_woe_c <- cbind.data.frame(validation_woe_p,validation_woe_y)

#naming the dependent variable

colnames(validation_woe_c)[28] <- "Performance.Tag"

colnames(validation_woe_c)[1:28] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(validation_woe_c)[1:28]))

#we will now use our best model to predict good and bad from validation data sets

#now since we have selected our best model as:

best_model <- glm(formula = PerformanceTag ~ MaritalStatusatthetimeofapplicationWOE + 
                    NoofdependentsWOE + IncomeWOE + ProfessionWOE +
                    NoofmonthsincurrentcompanyWOE +  
                    Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                    Nooftimes30DPDorworseinlast12monthsWOE + 
                    AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast12monthsWOE + 
                    NoofPLtradesopenedinlast12monthsWOE + 
                    NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                    OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
                  data = master_woe_train_rose)

#we will use this model to create our score card, we know the cutoff for our model, so we will use that information to predict the probability of
#good and bad and then through that information we will create a score card

predictions_logit <- predict(best_model, newdata = validation_woe_c[-28], type = "response")

predicted_response <- factor(ifelse(predictions_logit >= 0.5247475, "1", "0"))

validation_woe_c$PerformanceTag <- predicted_response

table(validation_woe_c$PerformanceTag)

#we see that out of the rejected population only 249 are classifyed as bad by our model which is approximately 64% accurate, so we now use this data
#and merge it with the master_woe data, to create a more robust model

colnames(master_woe_c)[1:28] <- gsub(" ", "", gsub("[[:punct:]]", " ", colnames(master_woe_c)[1:28]))

final_data <- rbind.data.frame(master_woe_c,validation_woe_c)

#now we will create a training & test data set 

set.seed(seed)

final_data_y <- final_data[,"PerformanceTag"]

final_data_t <- createDataPartition(final_data_y, p = 0.8, list = FALSE)

#now we will create training and test data sets first 

final_data_train <- final_data[final_data_t, ]

final_data_test <- final_data[-final_data_t, ]

final_data_train_rose <- final_data_train

final_data_test_rose <- final_data_test

#now we will ROSE package to generate data

#ROSE helps us to generate data synthetically as well. The data generated using ROSE is considered to provide better estimate of original data.

final_data_train_rose <- ROSE(PerformanceTag ~ ., data = final_data_train_rose, seed = 1)$data

##--------------------------------------------------------------------------------------------------------------------------------------##
##---------------------------------Training on Balanced final_data_train_rose data through logistic regression----------------------------##
##--------------------------------------------------------------------------------------------------------------------------------------##


#now we will train demo woe data with balanced data.....

set.seed(seed)

#First we will train master_woe_train_rose data sets. I'll be using logistic regression algorithm for modeling purpose.

model_glm <- glm(PerformanceTag ~ . , data = final_data_train_rose, family = binomial(logit))

summary(model_glm)

#now we'll quickly check two things for this model. First the p-values. Values below .05 indicates significance, which means the coefficient
#or so called parameters that are estimated by our model are reliable. And second, the pseudo R square. This value ranging from 0 to 1 indicates
#how much variance is explained by our model, if you're familiar with linear regression, this is equivalent to the R squared value, oh, I rounded
#the value to only show the 4 digits after decimal point.

summary_glm <- summary(model_glm)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that not all the variabes are significance, meaning that our model is not a legitimate one.
#A pseudo R square of 0.0747 tells that only 7 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use atepwise aic to select
#the correct variable

stepwise <- stepAIC(model_glm,direction = "both")

stepwise

#now we will select the variable given by the step function

model_2 <- glm(formula = PerformanceTag ~ AgeWOE + GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE + TypeofresidenceWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes60DPDorworseinlast12monthsWOE + Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + Nooftradesopenedinlast6monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_2)

summary_glm <- summary(model_2)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#A fast check on all the p-values of the model indicates that all the variabes are significance, meaning that our model is a legitimate one.
#A pseudo R square of 0.0747 tells that only 7 percent of the variance is explained. In other words, it is telling us that the model isn't powerful
#enough to predict customers who will default with high reliability. Since this is more of a dataset problem ( suggests collecting other variables to
#include to the dataset ) and there's not much we can do about it at this stage, so we'll simply move on to the next part is to use VIC

vif(model_2)

#so we see that all the variables are with in a VIF score of less than or approx 2, so we will remove the variable using significance

#will remove Nooftradesopenedinlast6monthsWOE

model_3 <- glm(formula = PerformanceTag ~ AgeWOE + GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE + TypeofresidenceWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes60DPDorworseinlast12monthsWOE + Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_3)

summary_glm <- summary(model_3)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#will remove TypeofresidenceWOE

model_4 <- glm(formula = PerformanceTag ~ AgeWOE + GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes60DPDorworseinlast12monthsWOE + Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_4)

summary_glm <- summary(model_4)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#will remove AgeWOE


model_5 <- glm(formula = PerformanceTag ~ GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes60DPDorworseinlast12monthsWOE + Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_5)

#will remove Nooftimes60DPDorworseinlast12monthsWOE

model_6 <- glm(formula = PerformanceTag ~ GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + ProfessionWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_6)

#will remove ProfessionWOE

model_7 <- glm(formula = PerformanceTag ~ GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE + TotalNoofTradesWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_7)

#will remove TotalNoofTradesWOE

model_8 <- glm(formula = PerformanceTag ~ GenderWOE + NoofdependentsWOE + 
                 IncomeWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_8)

#will remove GenderWOE

model_9 <- glm(formula = PerformanceTag ~ NoofdependentsWOE + 
                 IncomeWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes90DPDorworseinlast6monthsWOE + Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_9)

#will remove Nooftimes90DPDorworseinlast6monthsWOE

model_10 <- glm(formula = PerformanceTag ~ NoofdependentsWOE + 
                 IncomeWOE + NoofmonthsincurrentcompanyWOE + 
                 Nooftimes60DPDorworseinlast6monthsWOE + 
                 Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                 Nooftimes30DPDorworseinlast12monthsWOE + 
                 AvgasCCUtilizationinlast12monthsWOE + 
                 Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                 NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                 OutstandingBalanceWOE, family = binomial(logit), 
               data = final_data_train_rose)

summary(model_10)

#will remove IncomeWOE

model_11 <- glm(formula = PerformanceTag ~ NoofdependentsWOE + 
                  NoofmonthsincurrentcompanyWOE + 
                  Nooftimes60DPDorworseinlast6monthsWOE + 
                  Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                  Nooftimes30DPDorworseinlast12monthsWOE + 
                  AvgasCCUtilizationinlast12monthsWOE + 
                  Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                  NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                  OutstandingBalanceWOE, family = binomial(logit), 
                data = final_data_train_rose)

summary(model_11)

vif(model_11)

summary_glm <- summary(model_11)

list(summary_glm$coefficient, round(1 - (summary_glm$deviance / summary_glm$null.deviance), 4))

#the variance explained by the model is same at 0.0739

#since now all the variable are significant we will consider model_11 as our final model

#so are final model is

final_model <- glm(formula = PerformanceTag ~ NoofdependentsWOE + 
                     NoofmonthsincurrentcompanyWOE + 
                     Nooftimes60DPDorworseinlast6monthsWOE + 
                     Nooftimes30DPDorworseinlast6monthsWOE + Nooftimes90DPDorworseinlast12monthsWOE + 
                     Nooftimes30DPDorworseinlast12monthsWOE + 
                     AvgasCCUtilizationinlast12monthsWOE + 
                     Nooftradesopenedinlast12monthsWOE + NoofPLtradesopenedinlast12monthsWOE + 
                     NoofInquiriesinlast6monthsexcludinghomeautoloansWOE + NoofInquiriesinlast12monthsexcludinghomeautoloansWOE + 
                     OutstandingBalanceWOE, family = binomial(logit), 
                   data = final_data_train_rose)

summary(final_model)

#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas, it computes important metrics such
#as precision, recall & F measure.

pred.demo  <- predict.lm( final_model, newdata = final_data_test_rose[-28] , type = "response" )

accuracy.meas(final_data_test_rose$PerformanceTag, pred.demo)

#we see that precision is 0.075, recall is 0.336 and f score is 0.061

#We'll check the final accuracy of this model using ROC curve. This will give us a clear picture, if this model is worth. Using the function
#roc.curve available in this package:

roc.curve(final_data_test_rose$PerformanceTag, pred.demo, plotit = F)

#AUC = 0.683 is a terribly low score. but it is more than imbalance data.

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model, newdata = final_data_test_rose[-28], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, final_data_test_rose$PerformanceTag, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, final_data_test_rose$PerformanceTag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

# Let's choose a cutoff value 0.5346465 for final model

predicted_response <- factor(ifelse(predictions_logit >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(predicted_response, final_data_test_rose$PerformanceTag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(final_data_test_rose$PerformanceTag, predicted_response, plotit = T)

#AUC is 64% and we are getting a accuracy of 65% , Sensitivity of 64% and Specificity of 65% through logistic regression on final data data sets.

##Model Evaluation C-statistic

#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.
#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(predictions_logit,final_data_test_rose$PerformanceTag)

#we see that the c-statistic for test data is 6.832209e-01, which doesnot show very good discriminative power of the model.

##ks-statistic
#now we will evaluate our model using KS-statistic, we will prediction() function from package (ROCR)

#first we will store the prediction object in model_score_test

model_score_test <- prediction(predictions_logit,final_data_test_rose$PerformanceTag)

#now we will create performance object using performance function

model_perf_test <- performance(model_score_test,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks_test <- max(ks_table_test)

ks_test

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_test_index <- which(ks_table_test == ks_test)

ks_test_index

#now since the row is 5716, we will divide it by total number of rows in trainind ds which is  14023

ks_test_decile <- ks_test_index/nrow(final_data_test_rose)

ks_test_decile

#since the ks-statistic is at 0.4076161, therefore our ks-ststistic lies in the 4th decile, which is good

##----------------------------------------------------------application score card--------------------------------------------##

##now we have our best model, its time to create applicant score card.

#we first need to create applicant id for test and default

app <- as.data.frame(Applicant)

colnames(app) <- "Application_ID"

rej <- as.data.frame(rejected_Applicant)

colnames(rej) <- "Application_ID"

App_ID <- rbind.data.frame(app,rej)

# Applicant_train <- as.data.frame(App_ID[final_data_t, ])
# 
# Applicant_test <- as.data.frame(App_ID[-final_data_t, ])

final_data_train_rose$Applicant <- App_ID[final_data_t, ]

final_data_test_rose$Applicant <- App_ID[-final_data_t, ]

#we will now predict the probability of default using the final model

final_data_train_rose$PD <- predict(final_model, newdata = final_data_train_rose[-28], type = "response")

#we will use the below function to create the score card from predicted probability of default

#we will now use the function to create below score card

Applicant_ScoreCard <- df.score(final_data_train_rose$Applicant,final_data_train_rose$PD,final_data_train_rose$PerformanceTag,400,20,10)

#we can plot the score and ln odds of the applicant scorecard as below

ggplot(Applicant_ScoreCard,aes(x = Applicant_ScoreCard$Scores,y=Applicant_ScoreCard$ln_Odds)) + geom_line()

table(Applicant_ScoreCard$True_Label)

str(Applicant_ScoreCard)

#now we are require to calculate the threshold scores, we can use the same concepts which we have use earlier

summary(Applicant_ScoreCard$Scores)

## Model Evaluation: Logistic Regression

# Let's use the score cutoff of 50.

Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$PD >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(Applicant_ScoreCard$Pred, Applicant_ScoreCard$True_Label, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$PD >= cutoff, "1", "0"))
  conf <- confusionMatrix(Applicant_ScoreCard$Pred, Applicant_ScoreCard$True_Label, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

#so we can use the cuttoff value to get the cut off score

Applicant_ScoreCard$Scores[which(abs(Applicant_ScoreCard$PD) >= abs(cutoff[y,1]))][1]

#so our cut off score is 108

# Let's choose a cutoff value 0.5346465 for final model

Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$PD >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(Applicant_ScoreCard$Pred, Applicant_ScoreCard$True_Label, positive = "1")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(Applicant_ScoreCard$True_Label, Applicant_ScoreCard$Pred, plotit = T)

#AUC is 64% and we are getting a accuracy of 65% , Sensitivity of 63% and Specificity of 65% through logistic regression on final train data data sets.

##Model Evaluation C-statistic

#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.
#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(Applicant_ScoreCard$PD,Applicant_ScoreCard$True_Label)

#we see that the c-statistic for test data is 6.832209e-01, which doesnot show very good discriminative power of the model.

##ks-statistic
#now we will evaluate our model using KS-statistic, we will prediction() function from package (ROCR)

#first we will store the prediction object in model_score_test

model_score_test <- prediction(Applicant_ScoreCard$PD,Applicant_ScoreCard$True_Label)

#now we will create performance object using performance function

model_perf_test <- performance(model_score_test,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks_test <- max(ks_table_test)

ks_test

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_test_index <- which(ks_table_test == ks_test)

ks_test_index

#now since the row is 5716, we will divide it by total number of rows in trainind ds which is  56098

ks_test_decile <- ks_test_index/nrow(Applicant_ScoreCard)

ks_test_decile

#since the ks-statistic is at 0.4076161, therefore our ks-ststistic lies in the 4th decile, which is good

##----------------------------------------------------------------------------------------------------------------------------------------##

##Now we will test our model on test data sets

#we will now predict the probability of default using the final model

final_data_test_rose$PD <- predict(final_model, newdata = final_data_test_rose[-28], type = "response")

#we will use the below function to create the score card from predicted probability of default
#we will now use the function to create below score card

Applicant_ScoreCard <- df.score(final_data_test_rose$Applicant,final_data_test_rose$PD,final_data_test_rose$PerformanceTag,400,20,10)

#we can plot the score and ln odds of the applicant scorecard as below

ggplot(Applicant_ScoreCard,aes(x = Applicant_ScoreCard$Scores,y=Applicant_ScoreCard$ln_Odds)) + geom_line()

table(Applicant_ScoreCard$True_Label)

str(Applicant_ScoreCard)

#now we are require to calculate the threshold scores, we can use the same concepts which we have use earlier

summary(Applicant_ScoreCard$Scores)

## Model Evaluation: Logistic Regression

# Let's use the score cutoff of 50.

Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$PD >= 0.50, "1", "0"))



# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(Applicant_ScoreCard$Pred, Applicant_ScoreCard$True_Label, positive = "1")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$PD >= cutoff, "1", "0"))
  conf <- confusionMatrix(Applicant_ScoreCard$Pred, Applicant_ScoreCard$True_Label, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1]

#so we can use the cuttoff value to get the cut off score

Applicant_ScoreCard$Scores[which(abs(Applicant_ScoreCard$PD) >= abs(cutoff[y,1]))][1]

#so our cut off score is 108

# Let's choose a cutoff value 0.5346465 for final model

Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$PD >= cutoff[y,1], "1", "0"))

conf_final <- confusionMatrix(Applicant_ScoreCard$Pred, Applicant_ScoreCard$True_Label, positive = "1")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

roc.curve(Applicant_ScoreCard$True_Label, Applicant_ScoreCard$Pred, plotit = T)

#AUC is 64% and we are getting a accuracy of 65% , Sensitivity of 64% and Specificity of 65% through logistic regression on final test data sets.

##Model Evaluation C-statistic

#Now we will evaluate our model using C-statistic, we will use rcorr.cens from Hmisc package, since C-C-statistic uses predicted probabilities
#we first need to predict probabilities.
#so we will first add a column in our train data sets as predicted_prob and assign the value of predict function

#now we will use the rcorr.cens function to calculate c-statistic on test data

rcorr.cens(Applicant_ScoreCard$PD,Applicant_ScoreCard$True_Label)

#we see that the c-statistic for test data is 6.832209e-01, which doesnot show very good discriminative power of the model.

##ks-statistic
#now we will evaluate our model using KS-statistic, we will prediction() function from package (ROCR)

#first we will store the prediction object in model_score_test

model_score_test <- prediction(Applicant_ScoreCard$PD,Applicant_ScoreCard$True_Label)

#now we will create performance object using performance function

model_perf_test <- performance(model_score_test,"tpr","fpr")

#now we will create a vector called ks_table of difference between cumulative tpr and cummulative false positive rate from the performance object

ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

#so the maximum of the ks_table is our ks-statistic

ks_test <- max(ks_table_test)

ks_test

#now we will see in which decile it lies, so we will find the index for the maximum value first

ks_test_index <- which(ks_table_test == ks_test)

ks_test_index

#now since the row is 5716, we will divide it by total number of rows in trainind ds which is  14023

ks_test_decile <- ks_test_index/nrow(Applicant_ScoreCard)

ks_test_decile

#since the ks-statistic is at 0.4076161, therefore our ks-ststistic lies in the 4th decile, which is good

##----------------------------------------------------------------Thank you-------------------------------------------------------------------------##

