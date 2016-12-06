##------- ggplot Assignment -------------------

library(ggplot2)

# Part 1: ggplot objects 

# Q1.1 - Create an object using the iris data set where the x axis represents the Sepal.Length and
# y axis represents the Sepal.Width. 
# Store it as an object named sepal_plot

sepal_plot <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))

# Q1.2 - Add a geom_smooth() layer to the sepal_plot object to plot a smoothened plot.
# Complete the following code.

sepal_plot + geom_smooth()



# Q1.3 - To  the smoothened plot, add a layer of points. 
# Complete the following code.

sepal_plot + geom_smooth() + geom_point()
  

#Q1.4 - Create a new ggplot object sepal_plot1 so that color represents the Species.
# Create a point (scatter) plot using the new ggplot object. (Do not plot the geom_smooth() layer)
  
sepal_plot1 <- sepal_plot + geom_point(shape=17,size= 3,aes(col=Species))
sepal_plot1 

  

#Q1.5 - Create another new ggplot object to add the size attribute so that 
# it represents the Petal.Length variable. Create a scatter plot again.
# Let color represent the Species.

sepal_plot2 <- sepal_plot + geom_point(aes(size = Petal.Length,col = Species))
sepal_plot2

# --------------------------------------------------------

# Part 2: Aesthetics and Geometries

#Q2.1  - Use the mtcars data set to make a scatter plot such that the x axis represents the weight, 
# the y axis represents the mpg variable and the three cylinder values are mapped to three colours.

ggplot(mtcars,aes(x=wt,y=mpg,col=factor(cyl))) + geom_point(shape=17,size=3)


# Q2.2 - In the above command, assign the color "blue" to col inside the geom layer.
# Do not change the aesthetic layer from the previous plot.

ggplot(mtcars,aes(x=wt,y=mpg,col=factor(cyl))) + geom_point(shape=17,size=3,col = "blue")

#Q2.3 - Complete the command to map the cyl (cylinder) variable to the label attribute.
# Plot wt and mpg on x and y axes respectively.The output should be a plot where each label 
# represents the cyl value. 

ggplot(mtcars,aes(x=wt,y=mpg,col=factor(cyl))) + geom_text(size = 4,aes(label = cyl))

# ----------------------------------------------------------------

# Part 3: Points and Jitter


#Q3.1 - Use the diamond data set to plot a scatter plot representing carat on the x-axis,
# price on the y-axis and the clarity mapped to the color attribute.

ggplot(diamonds,aes(x = carat,y=price,col = clarity)) + geom_point()

#Q3.2 - In the previous command, assign the transparency parameter alpha = 0.5

ggplot(diamonds,aes(x = carat,y=price,col = clarity)) + geom_point(alpha = 0.5)

#Q3.3 - Create a scatter plot with the alpha parameter as 0.5, price variable represented by the color attribute,
# and the x and y axes representing the clarity and carat variables respectively.

ggplot(diamonds,aes(x=clarity,y=carat,col = price)) + geom_point(alpha = 0.5)
  
#Q3.4 - Do you see overlapping points in the previous plot? 
# Make a plot where the points are spread and reduce the overlap. 

ggplot(diamonds,aes(x=clarity,y=carat,col = price)) + geom_point(position = "jitter",alpha = 0.5)

# ---------------------------------------------------------------

# Part 4:Histograms

#Q4.1 - Using the mtcars data, plot a histogram using binwidth = 1 and the variable mpg on the x axis.

ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 1)


#Q4.2 - In the previous histogram, also display the am values using two colors.

ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 1)+ aes(fill = factor(am))

#Q4.3 - Using the previous command, add the position attribute to get a dodge chart.
# Use position = "dodge"

ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 1,position = "dodge")+ aes(fill = factor(am))


#Q4.4 - Change the histogram plotted above so that the bars occupy the entire length 
# of the y axis and the color represents fraction of am values

ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 1,position = "fill")+ aes(fill = factor(am))


# ---------------------------------------------------------------


# Part 5: Plotting multiple facets on one screen

#Q5.1 - For mtcars data frame, plot a histogram of mpg so that cylinders are represented by 3 colours

ggplot(mtcars,aes(x=mpg,fill = factor(cyl))) + geom_histogram(binwidth = 1)

#Q5.2 - In mtcars, the variable 'gear' represents the number of gears for each car. 
# How many unique values of gear are present in mtcars? 
# Use the unique() function to find out.

Unique_Gear <- unique(mtcars$gear)
Unique_Gear

# Paragraph on facet wraps
# If you want to plot separate histograms for each value of gear on the same screen, 
# you can use another layer of ggplot - facet_wrap. It plots multiple plots in separate windows
# in a grid so that you can view them all at once.
# Like geom layers, this layer can be added to a plot using a + sign.
# Type ?facet_wrap to read the documentation

?facet_wrap

#Q5.3 - Add a facet_wrap layer to the histogram plotted in Q5.1 to plot separate histograms
# for each value of gear. Use binwidth = 2.

mtcars$gear <- as.character(factor(mtcars$gear,labels = c("Gear 3","Gear 4","Gear 5")))

ggplot(mtcars,aes(x=mpg,fill = factor(cyl))) + geom_histogram(binwidth = 2) + facet_wrap(~gear)

#Q5.4 - Let us plot histograms of the diamonds data frame. The variable 'cut' 
# represents the quality of cutting of the diamonds. Using facet_wrap, plot histograms of 
# the 'price' variable so that each facet displays one type of cut.
# Use binwidth = 100.
# Observe that it is now very easy to compare the price distributions of different cuts.

ggplot(diamonds,aes(x=price))+geom_histogram(binwidth = 100)+ facet_wrap(~cut)

#Q5.5 - For diamonds, plot 5 scatter plots corresponding to 5 values of 'cut'
# using facet_wrap. Plot 'carat' on x axis, 'price' on y axis and map color to 'table'. 

ggplot(diamonds,aes(x=carat,y=price,col = table)) + geom_point() + facet_wrap(~cut)


#---------------------------------------------------------------------




