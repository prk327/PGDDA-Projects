setwd("E:/UpGrad IIIT PGDDA Program/Course 2/Uber Case Study")
dir()

# DATA PREPARATION

  # 1.
  #we will import the uber request data set in r by specyfying na values of string as "NA"
  Uber_Request <-
    read.csv("Uber request data.csv",
             header = TRUE,
             na.strings = "NA")
  
  #will check the structure of the data sets uber_request
  
  str(Uber_Request)
  
  
  #we see that all the character variable are converted to factor variable which we have done intentionally
  
  
  #For our data preparation we will extract hour from request.time and put it in a new column request.hr
  #will also convert the character string to integer for calculation
  
  Uber_Request$Request.HR <-
    as.integer(substr(Uber_Request$Request.time, 1, 2))
  
  
  
  #Now we will plot the group bar chart depicting the hour-wise trip made at city and airport respectively
  
  #we will first load the ggplot in our library
  
  library(ggplot2)
  
  #we will store the plot in plot1 value 
  
  Plot1 <- ggplot(Uber_Request, aes(x = factor(Request.HR), fill = Pickup.point)) + geom_bar(position = "dodge")
  
  #we will then add labels to the above plot
  
  plot2 <- Plot1 + labs(title = "Hour Wise Trip Request at City & AirPort(5 Days Report)\n", x = "Hours", y = "No Of Request", fill = "PickUp Point\n")
  
  plot2
  
  #for further analysis we will add the date variable through faceit
  
  plot2 + facet_wrap(~Date)
  
  #Now we can see that there is a 5 major time blocks based on frequency
  #so we will divide the 5 time slots into 5 groups in a column name time.slot
  #we will first create a DF with different tiemslots and name it x
  
  x <-
    c("Pre_Morning",
      "Morning_Rush",
      "Day_Time",
      "Evening_Rush",
      "Late_Night")
  
  #now we will create a condition statement to group the time slots in col Time_Slot
  
  Uber_Request$Time_Slot <-
    ifelse(
      Uber_Request$Request.HR <= 4,
      x[1],
      ifelse(
        Uber_Request$Request.HR >= 5 &
          Uber_Request$Request.HR <= 9,
        x[2],
        ifelse(
          Uber_Request$Request.HR >= 10 &
            Uber_Request$Request.HR <= 16,
          x[3],
          ifelse(
            Uber_Request$Request.HR >= 17 &
              Uber_Request$Request.HR <= 21,
            x[4],
            ifelse(
              Uber_Request$Request.HR >= 22 &
                Uber_Request$Request.HR <= 23,
              x[5],
              NA
            )
          )
        )
      )
    )
  
  
  #to know the number of trips made we will create a df where status is trip completed and name it y
  
  y <- subset(Uber_Request,Uber_Request$Status == "Trip Completed")
  
  #we will now order the time slots so as to show the time_slot in order manner in our plot
  
  y$Time_Or <- factor(y$Time_Slot,x)
  
  
  #Now we will plot a bar chart for number of trips made during different time slots
  
  Plot3 <- ggplot(y,aes(x= Time_Or,fill = Time_Or)) + geom_bar()
  
  #again we will label our plot as below
  
  Plot3 + labs(title = "No of Trips Made(Trip Completed)\n",x= "",y= "No of Trips", fill = "Time Slots\n")
  
  
  #we can know the count of trips made in different time slots by using table function
  
  table(y$Time_Or)
  
# PROBLEM IDENTIFICATION: 

  #we will also make the order as per time slots in uber DF
  
  Uber_Request$Time_Slot <- factor(Uber_Request$Time_Slot,x)
  
  
  # 1.
  #Now we will make a stacked bar chart where each bar rep a time slot
  #and Y axis rep frequency of request and different proportion will
  #represent different status
  
  plot4 <- ggplot(Uber_Request,aes(x = Time_Slot,fill = Status)) 
  
  plot4 + geom_bar(position = "stack") + labs(title="No of Request Made at Different Time Slots and Their Status",x="Time Slots",y="No Of Requests",fill = "Status Of Request")
  
  
  summary(Uber_Request)
  
  #2 problem 
  #by visual inspection we see that there is huge demant in morning and evening rush
  #but very few trip are completed
  
  #we can get the num of trips completed vs nummer of total request as follows
  
  #3 Diagnosis
  
  #3.1 Problem
  
  Mor_R <- length(which(Uber_Request$Time_Slot == "Morning_Rush"))
  
  Mor_R_c <- length(which(Uber_Request$Time_Slot == "Morning_Rush" & Uber_Request$Status == "Trip Completed"))
  
  
  Mor_R_C_Per <-   (Mor_R_c/Mor_R)*100
  
  
  #3.2 Problem
  
  Eve_R <- length(which(Uber_Request$Time_Slot == "Evening_Rush"))
  
  Eve_R_c <- length(which(Uber_Request$Time_Slot == "Evening_Rush" & Uber_Request$Status == "Trip Completed"))
  
  
  Eve_R_C_Per <-   (Eve_R_c/Eve_R)*100


# PROBLEM 1:

  #1.	For the time slot when problem 1 exists, plot a stacked bar chart to find out if the problem is more severe for pick-up requests made at the airport or the city
  
  Morning_TimeSlot <- subset(Uber_Request,Uber_Request$Time_Slot == "Morning_Rush")
  
  plot5 <- ggplot(Morning_TimeSlot,aes(x= Pickup.point,fill = Status)) + geom_bar(position = "stack")
  
  plot5 + labs(title="No Of Request on Time_Slot(Morning_Rush) in City & AirPort\n",x = "PickUp Point",y="No Of Request",fill = "Status Of Request")
  
  #plot for percentage breakup of number of issues on pickup points
  
  ggplot(Morning_TimeSlot,aes(x= Pickup.point,fill = Status)) + geom_bar(position = "fill") + labs(title="% Status Of Request on Time_Slot(Morning_Rush) in City & AirPort\n",x = "PickUp Point",y="% Status Of Request",fill = "Status Of Request")
  
  #now we have to calculate the % of issues at airport
  #so number of request at airport x
  
  x <- length(which(Morning_TimeSlot$Pickup.point == "Airport"))
  
  #total number of issues at airport is y
  
  y <- length(which(Morning_TimeSlot$Status != "Trip Completed" & Morning_TimeSlot$Pickup.point == "Airport"))
  
  #percentage of issue at airport is 
  
  (y/x)*100
  
  #Again we have to calculate the % of issues at City
  #so number of request at City is  x
  
  x <- length(which(Morning_TimeSlot$Pickup.point == "City"))
  
  #total number of issues at City is y
  
  y <- length(which(Morning_TimeSlot$Status != "Trip Completed" & Morning_TimeSlot$Pickup.point == "City"))
  
  #percentage of issue at City is 
  
  (y/x)*100
  
  #for problem 2 since the no of trip made in city is x
  #we have to find out no of trips completed from city to airport
  
  y <- length(which(Morning_TimeSlot$Status == "Trip Completed" & Morning_TimeSlot$Pickup.point == "City"))

# PROBLEM 2:
  
  #1.	For the time slot when problem 2 exists, plot a stacked bar chart to find out if the problem is more severe for pick-up requests made at the airport or the city
  
  Evening_TimeSlot <- subset(Uber_Request,Uber_Request$Time_Slot == "Evening_Rush")
  
  plot6 <- ggplot(Evening_TimeSlot,aes(x = Pickup.point,fill = Status)) + geom_bar(position = "stack")
  
  plot6 + labs(title="No Of Request on Time_Slot(Evening_Rush) in City & AirPort\n",x = "PickUp Point",y="No Of Request",fill = "Status Of Request")
  
  #plot for percentage breakup of number of issues on pickup points
  
  ggplot(Evening_TimeSlot,aes(x= Pickup.point,fill = Status)) + geom_bar(position = "fill") + labs(title="% Status Of Request on Time_Slot(Evening_Rush) in City & AirPort\n",x = "PickUp Point",y="% Status Of Request",fill = "Status Of Request")
  
  
  
  #now we have to calculate the % of issues at airport
  #so number of request at airport x
  
  x <- length(which(Evening_TimeSlot$Pickup.point == "Airport"))
  
  #total number of issues at airport is y
  
  y <- length(which(Evening_TimeSlot$Status != "Trip Completed" & Evening_TimeSlot$Pickup.point == "Airport"))
  
  #percentage of issue at airport is 
  
  (y/x)*100
  
  #Again we have to calculate the % of issues at City
  #so number of request at City is  x
  
  x <- length(which(Evening_TimeSlot$Pickup.point == "City"))
  
  #total number of issues at City is y
  
  y <- length(which(Evening_TimeSlot$Status != "Trip Completed" & Evening_TimeSlot$Pickup.point == "City"))
  
  #percentage of issue at City is 
  
  (y/x)*100
  
  #No of trips completed from airport to city is y
  
  y <- length(which(Evening_TimeSlot$Status == "Trip Completed" & Evening_TimeSlot$Pickup.point == "Airport"))
  

  