rm(list = ls())

#Assign the directory for the case study

setwd("E:/UpGrad IIIT PGDDA Program/Course 2/Hypothesis Case Study")

dir()


#loading the file Article popularity dataset and storing it in art_pop DF using read.csv


art_pop <- read.csv("Article popularity dataset.csv",header = TRUE,stringsAsFactors = FALSE,na.strings = "NA")

#checking the structure of the DF art_pop

str(art_pop)

#checking for any na values in DF

sum(is.na(art_pop))

#there is no na values in DF so we will check the summary of shares variable

summary(art_pop$shares)

#In summary we see that the min share is 1 and the max share is 843300, which is quite high
#we also check this with the quantile function

quantile(art_pop$shares)

#we see that 25% of the share are equal to or less than 946 and 75% of the share are equal to or less than 2800
#we also see that there is huge difference between 75th and 100 percentile, so we will check for outliers
#by ploting boxplot

boxplot(art_pop$shares,main = "Shares per data channels", boxwex=0.2)

#since we see that there is outliers we will calculate the upper and lower hinge

lower_hinge <- quantile(art_pop$shares,0.25)

Upper_hinge <- quantile(art_pop$shares,0.75)

#from the quantile we see that 75% of the share are at 2800 and 25% is at 946.
#So to know the exact level of outlier we will calculate the IQR

# to detect the outlier we will first calculate the IQR

IQR <- Upper_hinge - lower_hinge

IQR

#so we know know that the IQR is 1854 and as a rule of thumb any value lies beyond
#1.5*IQR is an outlier, so Q1-1.5*IQR and Q3+1.5*IQR are the outliers

steps <- 1.5*IQR

steps + Upper_hinge

#so any thing above 1854+2781 and 946-2781 are an outlier
# we can also detect the outliers by the box.plot .stats

boxplot.stats(art_pop$shares)


#since the outlies are only present in the upper hinge from 5581, so any value
#above 5581 will be treated as outliers and we will capping method ot outlier treatment
#so we will first replace the outlier above 5581 with NA and then 
#calculate the 95% of the value excluding NA and inpute it in the NA values

#we will first get the index of the outliers and store it in y

y <- which(art_pop$shares >= 5581)

#we then use the index to impute NA values in the DF

art_pop[y,8] <- NA_integer_

#now we will calculate the 95% of the shares

x <- quantile(art_pop$shares,0.95,na.rm = TRUE)

#now we will replace all the NA values in shares with the value of x ie 4200

art_pop[y,8] <- x

#now we will again check if there is any outlier still present in out shares column

boxplot.stats(art_pop$shares)

boxplot(art_pop$shares)

#Since the box plot stats did not show any value in out so out outlier treatment is good

summary(art_pop)

#Now our second objective is to extract day of publishing of each article from the url

View(art_pop)

#we see that the date is in betwwen the url so we need to find the starting length of the date
#so we will use regexec to know the starting point of date in url

x <- regexec("2",art_pop$URL)

#we see that all the date are starting from the 21th character and ending at 30th character
#we will use substring function to extract the date string
#and since we want a column in days of week, we will convert that string into date format
#and then use the weekdays function to get the days of week in column DayofPublishing

art_pop$DayOfPublishing <-  weekdays(as.Date(substr(art_pop$URL,x,30),"%Y/%m/%d"))

str(art_pop$DayOfPublishing)

#since the day of week is a character type we will convert it to factor type for further analysis

art_pop$DayOfPublishing <- as.factor(art_pop$DayOfPublishing)

levels(art_pop$DayOfPublishing)

#so now we have 7 levels of day variable which represent days of week
#we now have to create another column with weekdays and weekends

#we will first create a vector with weekdays

weekdays1 <- c("Monday","Tuesday","Wednesday","Thursday","Friday")

#we will use the if function and replace the true with the weekdays and false with weekends


art_pop$weeks <- ifelse(art_pop$DayOfPublishing %in% weekdays1, "Weekdays", "Weekends")



#we also see that the data channel var are having different column, so for our
#analysis we will also merge the data channels into one column and name it data_channels

art_pop$data_channels <-
  paste(
    art_pop$data_channel_is_lifestyle,
    art_pop$data_channel_is_entertainment,
    art_pop$data_channel_is_bus,
    art_pop$data_channel_is_socmed,
    art_pop$data_channel_is_tech,
    art_pop$data_channel_is_world,
    sep = ""
  )


# Now we will look at the structure of the data_channels column

str(art_pop$data_channels)

#we found that it is of character type so we will convert it to factor type

art_pop$data_channels <- as.factor(art_pop$data_channels)

#and now we will check for the levels of the factors


levels(art_pop$data_channels)

#now since the number "000000" does not make any sense we will name the variables

levels(art_pop$data_channels) <-
  c(
    "Others",
    "World",
    "Technology",
    "Social Media",
    "Business",
    "Entertainment",
    "Lifestyle"
  )

#now we will again check for the levels and see if we have made the correct levels

levels(art_pop$data_channels)

#now we will check for the shares on each channels using plot

library(ggplot2)

plot_1 <- ggplot(art_pop,aes(x=art_pop$data_channels,y=shares,fill = art_pop$data_channels)) + geom_boxplot()

plot_1 + labs(title = "No of Shares per Data Channels\n",x = "Data Channels",y="Shares",fill = "Data Channels")

#now we need to fill the table given in the assignment for that we will create a subset
#of data channels and weeks

#but first we need to group data_channels in two category social media and others

art_pop$Channels <- ifelse(art_pop$data_channels != "Social Media","Others","Social Media")

#Now since we have formatted our data sets we will subset our data sets based on 
#problem given

#firts we will create 8 DF as per the problem given from the sample provided


x <- subset(art_pop,select = c(Channels,weeks,shares))


Z1 <- subset(x,x$weeks == "Weekdays")

Z2 <- subset(x,x$weeks == "Weekends")

Y1 <- subset(x,x$Channels == "Social Media")

Y2 <- subset(x,x$Channels == "Others")

X11 <- subset(x,x$Channels == "Social Media" & x$weeks == "Weekdays")

X12 <- subset(x,x$Channels == "Social Media" & x$weeks == "Weekends")

X21 <- subset(x,x$Channels == "Others" & x$weeks == "Weekdays")

X22 <- subset(x,x$Channels == "Others" & x$weeks == "Weekends")

#for Q1 

#step 1 Setup hypothesis and determine level of significance
#NULL hypothesis states that there is no difference between the mean so
# Ho : z1 = z2 therefore
# H1 : z1 not equal to z2

#now we want to be 100 - 1 = 99% sure about the result, before we come to a
#decision that 2 means are different
#thus, we fix alpha = 1 - 0.99 = 0.01

#step 2 carrying out sampling exercise and calculate the sample statists
#since we already have the sample we can continue with our test


#step 3 selecting the appropriate t test, since we don't know the
#population standard deviation and population mean is same as sample mean
#we would select t test i.e t_value <-  function(x,y) {(mean(x) - mean(y))/(sqrt(((sd(x)^(2))/(length(x))) + ((sd(y)^(2))/(length(y)))))}

#Step 4 setting up decision rule
#since we are performing 2 tail test we will set 
#if t value is greater than (-alpha/2) and if t value is less than (+alpha/2) we fail
#to reject the null hypothesis
#since if n>30 t value approximates z value so our critival value will be
#-2.576 < t value < 2.576

#step 5 calculate the test statistics

t_value <-  function(x,y) {(mean(x) - mean(y))/(sqrt(((sd(x)^(2))/(length(x))) + ((sd(y)^(2))/(length(y)))))}

t_value(Z1$shares,Z2$shares)


#we will now calculate the t value using inbuid function and verify our result

t.test(Z1$shares,Z2$shares,alternative = "two.sided",mu=0,conf.level = 0.99)

#since the manual and inbult function gives the same result our calculation is correct

#step 6 taking the decision regarding rejection or failure to reject the null hypothesis
#since t value of -18 is way to less than the t value of -2.576 we can reject the Null Hypothesis
#So we can say at 99% confidence that the average number of shares for each article
#differ significantly for articles published over weekdays and weekends


#Q2
#step 1 Setup hypothesis and determine level of significance
#NULL hypothesis states that there is no difference between the mean so
# Ho : X12 = X22 therefore
# H1 : X12 not equal to X22

#now we want to be 100 - 1 = 99% sure about the result, before we come to a
#decision that 2 means are different
#thus, we fix alpha = 1 - 0.99 = 0.01

#step 2 carrying out sampling exercise and calculate the sample statists
#since we already have the sample we can continue with our test

#step 3 selecting the appropriate t test, since we don't know the
#population standard deviation and population mean is same as sample mean
#we would select t test i.e t_value <-  function(x,y) {(mean(x) - mean(y))/(sqrt(((sd(x)^(2))/(length(x))) + ((sd(y)^(2))/(length(y)))))}

#Step 4 setting up decision rule
#since we are performing 2 tail test we will set 
#if t value is greater than (-alpha/2) and if t value is less than (+alpha/2) we fail
#to reject the null hypothesis
#since if n>30 t value approximates z value so our critival value will be
#-2.576 < t value < 2.576

#step 5 calculate the test statistics

t_value(X12$shares,X22$shares)

#we will now calculate the t value using inbuid function and verify our result

t.test(X12$shares,X22$shares,alternative = "two.sided",mu=0,conf.level = 0.99)

#since the manual and inbult function gives the same result our calculation is correct

#step 6 taking the decision regarding rejection or failure to reject the null hypothesis
#since t value of 5.5024 which is more than the t value of 2.576 we can reject the Null Hypothesis
#So we can say at 99% confidence that the average number of shares for each article published over
#weekend differ significantly for articles on Social media channel and other channels

#Q3
#step 1 Setup hypothesis and determine level of significance
#NULL hypothesis states that there is no difference between the mean so
# Ho : X11 = X21 therefore
# H1 : X11 not equal to X21

#now we want to be 100 - 1 = 99% sure about the result, before we come to a
#decision that 2 means are different
#thus, we fix alpha = 1 - 0.99 = 0.01

#step 2 carrying out sampling exercise and calculate the sample statists
#since we already have the sample we can continue with our test

#step 3 selecting the appropriate t test, since we don't know the
#population standard deviation and population mean is same as sample mean
#we would select t test i.e t_value <-  function(x,y) {(mean(x) - mean(y))/(sqrt(((sd(x)^(2))/(length(x))) + ((sd(y)^(2))/(length(y)))))}

#Step 4 setting up decision rule
#since we are performing 2 tail test we will set 
#if t value is greater than (-alpha/2) and if t value is less than (+alpha/2) we fail
#to reject the null hypothesis
#since if n>30 t value approximates z value so our critival value will be
#-2.576 < t value < 2.576

#step 5 calculate the test statistics

t_value(X11$shares,X21$shares)

#we will now calculate the t value using inbuid function and verify our result

t.test(X11$shares,X21$shares,alternative = "two.sided",mu=0,conf.level = 0.99)

#since the manual and inbult function gives the same result our calculation is correct

#step 6 taking the decision regarding rejection or failure to reject the null hypothesis
#since t value of 18.859 which is more than the t value of 2.576 we can reject the Null Hypothesis
#So we can say at 99% confidence that the average number of shares for each article
#published over weekdays differ significantly for articles on Social media channel and other channels

#Q4
#step 1 Setup hypothesis and determine level of significance
#NULL hypothesis states that there is no difference between the mean so
# Ho : X11 = X12 therefore
# H1 : X11 not equal to X12

#now we want to be 100 - 5 = 95% sure about the result, before we come to a
#decision that 2 means are different
#thus, we fix alpha = 1 - 0.95 = 0.05

#step 2 carrying out sampling exercise and calculate the sample statists
#since we already have the sample we can continue with our test


#step 3 selecting the appropriate t test, since we don't know the
#population standard deviation and population mean is same as sample mean
#we would select t test i.e t_value <-  function(x,y) {(mean(x) - mean(y))/(sqrt(((sd(x)^(2))/(length(x))) + ((sd(y)^(2))/(length(y)))))}

#Step 4 setting up decision rule
#since we are performing 2 tail test we will set 
#if t value is greater than (-alpha/2) and if t value is less than (+alpha/2) we fail
#to reject the null hypothesis
#since if n>30, t value approximates z value so our critival value will be
#-1.96 < t value < 1.96

#step 5 calculate the test statistics

t_value(X11$shares,X12$shares)

#we will now calculate the t value using inbuid function and verify our result

t.test(X11$shares,X12$shares,alternative = "two.sided",mu=0,conf.level = 0.95)

#since the manual and inbult function gives the same result our calculation is correct

#step 6 taking the decision regarding rejection or failure to reject the null hypothesis
#since t value of -2.7202 which is more than the t value of -1.96 we can reject the Null Hypothesis
#So we can say at 95% confidence that the average number of shares for Social Media
#articles published over weekdays and weekends differ significantly from each other.


#-----------------------------------------END--------------------------------------------------------------------------------------------#

